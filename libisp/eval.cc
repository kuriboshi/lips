/*
 * Lips, lisp shell.
 * Copyright 1988, 2020 Krister Joas
 *
 * $Id$
 *
 */

#include "libisp.hh"

using namespace lisp;

extern int brkflg;
extern int interrupt;
extern LISPT findalias(LISPT);
extern void pputc(int, FILE*);

void (*breakhook)(void);         /* Called before going into break. */
int (*undefhook)(LISPT, LISPT*); /* Called in case of undefined function. */

/* 
 * These variables are really local to this file but are needed
 * by the garbage collector.
 */
LISPT fun;                /* Store current function beeing evaluated. */
LISPT expression;         /* Current expression. */
LISPT args;               /* Current arguments. */
alloc::destblock_t* env;  /* Current environment. */
alloc::destblock_t* dest; /* Current destination beeing built. */

CONTROL control; /* Control-stack. */
int toctrl;      /* Control-stack stack pointer. */

static int peval(void);
static int peval1(void);
static int peval2(void);
static int ev0(void), ev1(void), ev2(void), ev3(void), ev4(void), evlam1(void);
static int evlam0(void);
static int ev9(void), ev11(void), ev3p(void);
static int evalargs(void), noevarg(void), evlam(void), spread(void);
static int evlis(void), evlis1(void), evlis2(void), evlis3(void), evlis4(void);
static int noev9(void);
static int evsequence(void), evseq1(void), evseq3(void);
static int evclosure(void), evclosure1(void);
static int eval0(void), apply0(void);
static int everr(void);
static int lookup(void);

static int noeval;        /* Don't evaluate arguments. */
static continuation_t cont; /* Current continuation. */

static LISPT printwhere()
{
  LISPT foo = C_NIL;
  for(int i = toctrl - 1; i; i--) /* find latest completed call */
  {
    if(control[i].type == CTRL_FUNC && control[i].u.f_point == evlam0)
      for(; i; i--)
      {
        if(control[i].type == CTRL_FUNC && control[i].u.f_point == ev0 && control[i - 1].type == CTRL_LISP
          && (TYPEOF(control[i - 1].u.lisp) == CONS && TYPEOF(CAR(control[i - 1].u.lisp)) != CONS))
        {
          foo = control[i - 1].u.lisp;
          fprintf(primerr, " [in ");
          prin2(CAR(foo), C_T);
          pputc(']', primerr);
          goto out;
        }
      }
  }
out:
  pputc('\n', primerr);
  return foo;
}

/*
 * Print errormessage, abort current evaluation, and
 * return to top level.
 */
static void abort(int m, LISPT v)
{
  error(m, v);
  printwhere();
  unwind();
  throw lisp_error("abort");
}

static void overflow()
{
  abort(STACK_OVERFLOW, C_NIL);
}

/* 
 * These macros handles the control stack.  The control stack stores
 * continuations, destinations, and LISPT objects.  There are two macros 
 * to push and to pop pointers and LISPT objects.
 */
inline void push_lisp(LISPT a)
{
  control[toctrl].type = CTRL_LISP;
  control[toctrl++].u.lisp = a;
  if(toctrl >= CTRLBLKSIZE)
    overflow();
}

inline void push_point(lisp::alloc::destblock_t* d)
{
  control[toctrl].type = CTRL_POINT;
  control[toctrl++].u.point = d;
  if(toctrl >= CTRLBLKSIZE)
    overflow();
}

inline void push_func(continuation_t f)
{
  control[toctrl].type = CTRL_FUNC;
  control[toctrl++].u.f_point = f;
  if(toctrl >= CTRLBLKSIZE)
    overflow();
}

inline LISPT pop_lisp()
{
  return control[--toctrl].u.lisp;
}

inline lisp::alloc::destblock_t* pop_point()
{
  return control[--toctrl].u.point;
}

inline continuation_t pop_func()
{
  return control[--toctrl].u.f_point;
}

/*
 * This function prints an error message, and sets up a call
 * to everr that handles breaks.
 */
static void xbreak(int mess, LISPT fault, continuation_t next)
{
  if(mess != 0)
  {
    error(mess, fault);
    printwhere();
  }
  if(breakhook != nullptr)
    (*breakhook)();
  if(env == nullptr)
    throw lisp_error("break");
  xprint(cons(fault, cons(C_BROKEN, C_NIL)), C_T);
  push_func(next);
  cont = everr;
}

/* 
 * mkdestblock - creates a new destination block of size
 *               `s' and initializes it.
 */
inline alloc::destblock_t* mkdestblock(int s)
{
  auto dest = dalloc(s + 1);
  dest[0].var.d_integer = s;
  dest[0].val.d_integer = s;
  dest[0].type = 0;
  return dest;
}

inline void storevar(LISPT v, int i)
{
  dest[i].var.d_lisp = v;
  dest[i].type = 1;
}

inline static alloc::destblock_t* pop_env()
{
  dfree(env);
  return pop_point();
}

static inline void send(LISPT a)
{
  if(dest[0].var.d_integer > 0)
    dest[dest[0].var.d_integer].val.d_lisp = a;
}

static inline LISPT receive()
{
  return dest[dest[0].var.d_integer].val.d_lisp;
}

static inline void next()
{
  if(dest[0].var.d_integer > 0)
    --dest[0].var.d_integer;
}

/* 
 * Make a call to the function in parameter `fun'.  It can handle
 * functions with up to three arguments.
 */
static LISPT call(LISPT fun)
{
  LISPT foo = C_NIL;

  switch(SUBRVAL(fun).argcount)
  {
    case 0:
      foo = (*(SUBRVAL(fun).function0))();
      break;
    case 1:
    case -1:
      foo = (*(SUBRVAL(fun).function1))(dest[1].val.d_lisp);
      break;
    case 2:
    case -2:
      foo = (*(SUBRVAL(fun).function2))(dest[2].val.d_lisp, dest[1].val.d_lisp);
      break;
    case 3:
    case -3:
      foo = (*(SUBRVAL(fun).function3))(dest[3].val.d_lisp, dest[2].val.d_lisp, dest[1].val.d_lisp);
      break;
    default:
      break;
  }
  return foo;
}

/*
 * This is the evaluator, it allocates a destination
 * slot for the result and start munching continuations.
 */
/*
Dummy definition for the pretty printer.
PRIMITIVE eval(LISPT expr)
*/
PRIMITIVE eval(LISPT expr)
{
  /* 
   * Set the current expression to `expr' and push the current
   * destination onto the control stack.  (Why isn't `exp' pushed?)
   */
  expression = expr;
  push_point(dest);
  /* 
   * The result of evalutating `expr' is stored in the destination,
   * which is retrieved with the RECEIVE macro.
   */
  dest = mkdestblock(1);
  /* 
   * This how it works in general: Push the function to be called
   * last, and set the continuation variable `cont' to `peval'.
   * `peval' may push more contiuations onto the stack but eventaully
   * `eval0' is called which returns 1, signalling end of evaluation.
   */
  push_func(eval0);
  cont = peval;
  while(!(*cont)())
    ;
  /* 
   * Retrieve the result of the evaluation and restore the previous
   * destination.
   */
  LISPT foo = receive();
  dest = pop_point();
  /* 
   * Return the result.
   */
  return foo;
}

static int eval0()
{
  dfree(dest);
  return 1;
}

/*
Dummy definition for the pretty printer.
PRIMITIVE apply(f, a)
*/
PRIMITIVE apply(LISPT f, LISPT a)
{
  push_point(dest);
  dest = mkdestblock(1);
  push_lisp(fun);
  fun = f;
  push_lisp(args);
  args = a;
  expression = cons(f, a);
  push_func(apply0);
  cont = peval2;
  while(!(*cont)())
    ;
  LISPT foo = receive();
  dest = pop_point();
  return foo;
}

static int apply0()
{
  dfree(dest);
  args = pop_lisp();
  fun = pop_lisp();
  return 1;
}

static int ev0()
{
  /* 
   * Discard the top of stack (it's the previous expression, see
   * comment in `eval' above) and restore continuation.  The function
   * `ev0' is also used as a placeholder for the beginning of an eval.
   */
  toctrl -= 1;
  cont = pop_func();
  return 0;
}

static int peval()
{
#ifdef TRACE
  if(trace)
    xprint(expression, C_T);
#endif /* TRACE */
  push_lisp(expression);
  push_func(ev0);
  switch(TYPEOF(expression))
  {
    case CONS:
      push_lisp(fun);
      fun = CAR(expression);
      push_lisp(args);
      args = CDR(expression);
      push_func(ev1);
      cont = peval1;
      break;
    case SYMBOL:
      cont = lookup;
      break;
    case INDIRECT:
      send(INDIRECTVAL(expression));
      cont = pop_func();
      break;
    case CVARIABLE:
      send(*CVARVAL(expression));
      cont = pop_func();
      break;
    case FREE:
      abort(CORRUPT_DATA, expression);
      break;
    default:
      send(expression);
      cont = pop_func();
      break;
  }
  return 0;
}

static int ev1()
{
  args = pop_lisp();
  fun = pop_lisp();
  cont = pop_func();
  return 0;
}

static int evalhook(LISPT exp)
{
  LISPT res;

  if(undefhook != nullptr)
    switch((*undefhook)(exp, &res))
    {
      case 1:
        send(res);
        cont = pop_func();
        break;
      case -1:
        abort(NO_MESSAGE, C_NIL);
        break;
      default:
        return 0;
        break;
    }
  return 1;
}

void do_unbound(int (*continuation)(void))
{
  /* 
   * If an undefined symbol has the AUTOLOAD property, we try to
   * load the definition from a file.  If that doesn't succeed, then
   * the symbol is undefined.
   */
  LISPT al = getprop(CAR(expression), C_AUTOLOAD);
  if(!ISNIL(al))
  {
    push_lisp(expression);
    push_point(dest);
    load(al);
    dest = pop_point();
    expression = pop_lisp();
    fun = SYMVALUE(CAR(expression));
    if(TYPEOF(fun) == UNBOUND)
    {
      if(!evalhook(expression))
        xbreak(UNDEF_FUNCTION, CAR(expression), continuation);
    }
    else
      cont = continuation;
  }
  else
  {
    expression = findalias(expression);
    if(EQ(expression, C_ERROR))
      abort(NO_MESSAGE, C_NIL);
    if(TYPEOF(expression) == CONS && TYPEOF(CAR(expression)) == SYMBOL && TYPEOF(SYMVALUE(CAR(expression))) == UNBOUND)
    {
      if(!evalhook(expression))
        xbreak(UNDEF_FUNCTION, CAR(expression), continuation);
    }
    else
    {
      fun = CAR(expression);
      args = CDR(expression);
      cont = continuation;
    }
  }
}

int do_default(int (*continuation)(void))
{
  expression = findalias(expression);
  if(EQ(expression, C_ERROR))
    abort(NO_MESSAGE, C_NIL);
  if(TYPEOF(expression) == CONS && TYPEOF(CAR(expression)) == SYMBOL && TYPEOF(SYMVALUE(CAR(expression))) == UNBOUND)
  {
    if(!evalhook(expression))
      xbreak(UNDEF_FUNCTION, CAR(expression), continuation);
    return 1;
  }
  else
    return 0;
}

static int peval1()
{
  int foo;

  if(brkflg)
    xbreak(KBD_BREAK, fun, peval1);
  else if(interrupt)
    abort(NO_MESSAGE, C_NIL);
  else
    switch(TYPEOF(fun))
    {
      case CLOSURE:
        push_func(peval1);
        cont = evclosure;
        break;
      case SUBR:
        push_point(dest);
        push_func(ev2);
        if((foo = SUBRVAL(fun).argcount) < 0)
        {
          dest = mkdestblock(-foo);
          push_func(noevarg);
          cont = evlis;
        }
        else
        {
          dest = mkdestblock(foo);
          noeval = 0;
          cont = evalargs;
        }
        break;
      case FSUBR:
        push_point(dest);
        push_func(ev2);
        if((foo = SUBRVAL(fun).argcount) < 0)
        {
          dest = mkdestblock(-foo);
          cont = spread;
        }
        else
        {
          dest = mkdestblock(foo);
          noeval = 1;
          cont = evalargs;
        }
        break;
      case LAMBDA:
        noeval = 0;
        cont = evlam;
        break;
      case NLAMBDA:
        noeval = 1;
        cont = evlam;
        break;
      case CONS:
      case INDIRECT:
        expression = fun;
        push_func(ev3);
        cont = peval;
        break;
      case SYMBOL:
        fun = SYMVALUE(fun);
        cont = peval1;
        break;
      case UNBOUND:
        do_unbound(peval1);
        break;
      case STRING:
        if(!evalhook(expression))
          xbreak(ILLEGAL_FUNCTION, fun, peval1);
        break;
      default:
        if(!do_default(peval1))
          xbreak(ILLEGAL_FUNCTION, fun, peval1);
        break;
    }
  return 0;
}

static int peval2()
{
  int foo;

  if(brkflg)
    xbreak(KBD_BREAK, fun, peval2);
  else
    switch(TYPEOF(fun))
    {
      case CLOSURE:
        push_func(peval2);
        cont = evclosure;
        break;
      case SUBR:
      case FSUBR:
        push_point(dest);
        push_func(ev2);
        if((foo = SUBRVAL(fun).argcount) < 0)
        {
          dest = mkdestblock(-foo);
          cont = spread;
        }
        else
        {
          dest = mkdestblock(foo);
          noeval = 1;
          cont = evalargs;
        }
        break;
      case LAMBDA:
      case NLAMBDA:
        noeval = 1;
        cont = evlam;
        break;
      case CONS:
      case INDIRECT:
        expression = fun;
        push_func(ev3p);
        cont = peval;
        break;
      case SYMBOL:
        fun = SYMVALUE(fun);
        cont = peval2;
        break;
      case UNBOUND:
        do_unbound(peval2);
        break;
      case STRING:
        if(!evalhook(expression))
          xbreak(ILLEGAL_FUNCTION, fun, peval2);
        break;
      default:
        if(!do_default(peval2))
          xbreak(ILLEGAL_FUNCTION, fun, peval2);
        break;
    }
  return 0;
}

/*
 * bt - Prints a backtrace of all expressions on the
 *    	stack.
 */
void bt()
{
  int op = printlevel;
  printlevel = 2;
  for(int i = toctrl - 1; i; i--)
  {
    if(control[i].type == CTRL_FUNC && control[i].u.f_point == ev0)
      xprint(control[i - 1].u.lisp, C_T);
  }
  printlevel = op;
}

static int everr()
{
  expression = break0(expression);
  cont = pop_func(); /* Discard one continuation. */
  cont = pop_func();
  return 0;
}

static int noevarg()
{
  args = receive();
  cont = spread;
  return 0;
}

static int evalargs()
{
  if(ISNIL(args))
  {
    cont = pop_func();
  }
  else
  {
    expression = CAR(args);
    if(noeval)
      cont = noev9;
    else
      cont = ev9;
  }
  return 0;
}

static int ev9()
{
  if(ISNIL(CDR(args)))
  {
    cont = peval;
  }
  else
  {
    push_func(ev11);
    cont = peval;
  }
  return 0;
}

static int ev11()
{
  next();
  args = CDR(args);
  expression = CAR(args);
  cont = ev9;
  return 0;
}

static int noev9()
{
nextarg:
  if(ISNIL(CDR(args)))
  {
    send(expression);
    cont = pop_func();
  }
  else
  {
    send(expression);
    next();
    args = CDR(args);
    expression = CAR(args);
    goto nextarg;
  }
  return 0;
}

static int evlis()
{
  if(ISNIL(args))
  {
    cont = pop_func();
  }
  else
  {
    expression = CAR(args);
    cont = evlis1;
  }
  return 0;
}

static int evlis1()
{
  if(ISNIL(CDR(args)))
  {
    push_func(evlis2);
    cont = peval;
  }
  else
  {
    push_func(evlis3);
    cont = peval;
  }
  return 0;
}

static int evlis2()
{
  LISPT x = cons(receive(), C_NIL);
  send(x);
  cont = pop_func();
  return 0;
}

static int evlis3()
{
  push_point(dest);
  dest = mkdestblock(1);
  push_func(evlis4);
  args = CDR(args);
  expression = CAR(args);
  cont = evlis1;
  return 0;
}

static int evlis4()
{
  LISPT x = receive();
  dfree(dest);
  dest = pop_point();
  x = cons(receive(), x);
  send(x);
  cont = pop_func();
  return 0;
}

static int evlam()
{
  int i;
  int ac;
  LISPT foo;

  push_lisp(expression);
  push_point(env);
  push_point(dest);
  int spr = 0;
  if((ac = LAMVAL(fun).argcnt) < 0)
  {
    ac = -ac;
    spr++;
  }
  dest = mkdestblock(ac);
  for(foo = LAMVAL(fun).arglist, i = ac; i; foo = CDR(foo), i--) storevar(CAR(foo), i);
  push_func(evlam1);
  if(spr)
  {
    if(noeval)
      cont = spread;
    else
    {
      push_func(noevarg);
      cont = evlis;
    }
  }
  else
    cont = evalargs;
  return 0;
}

static int spread()
{
respread:
  if(EQ(args, C_NIL))
  {
    cont = pop_func();
  }
  else if((dest[0].var.d_integer) == 1)
  {
    send(args);
    cont = pop_func();
  }
  else
  {
    send(CAR(args));
    next();
    args = CDR(args);
    goto respread;
  }
  return 0;
}

static int ev2()
{
  LISPT foo = call(fun);
  dfree(dest);
  dest = pop_point();
  if(EQ(foo, C_ERROR))
  {
    foo = printwhere();
    xbreak(0, CAR(foo), peval1); /* CAR(_) broken */
  }
  else
  {
    send(foo);
    cont = pop_func();
  }
  return 0;
}

static int ev3()
{
  fun = receive();
  push_func(ev4);
  cont = peval1;
  return 0;
}

static int ev3p()
{
  fun = receive();
  push_func(ev4);
  cont = peval2;
  return 0;
}

static int ev4()
{
  cont = pop_func();
  return 0;
}

static void Link()
{
  dest[0].var.d_environ = env;
  dest[0].type = 2;
  env = (alloc::destblock_t*)&dest[0];
  for(auto i = dest[0].val.d_integer; i > 0; i--)
  {
    LISPT t = SYMVALUE(dest[i].var.d_lisp);
    SYMVALUE(dest[i].var.d_lisp) = dest[i].val.d_lisp;
    dest[i].val.d_lisp = t;
  }
}

static int evlam1()
{
  Link();
  dest = pop_point();
  args = LAMVAL(fun).lambdarep;
  push_func(evlam0);
  cont = evsequence;
  return 0;
}

static void restore_env()
{
  auto* c = env;
  for(auto i = c[0].val.d_integer; i > 0; i--) SYMVALUE(c[i].var.d_lisp) = c[i].val.d_lisp;
}

static int evlam0()
{
  restore_env();
  env = pop_env();
  expression = pop_lisp();
  cont = pop_func();
  return 0;
}

void unwind()
{
  while(env != nullptr)
  {
    restore_env();
    env = env->var.d_environ;
  }
}

static int lookup()
{
  LISPT t = SYMVALUE(expression);
  switch(TYPEOF(t))
  {
    case UNBOUND:
      xbreak(UNBOUND_VARIABLE, expression, lookup);
      return 0;
      break;
    case INDIRECT:
      send(INDIRECTVAL(t));
      break;
    case CVARIABLE:
      send(*CVARVAL(t));
      break;
    default:
      send(t);
      break;
  }
  cont = pop_func();
  return 0;
}

static int evclosure()
{
  LISPT foo;
  int i;

  push_point(env);
  push_point(dest);
  dest = mkdestblock(CLOSVAL(fun).count);
  for(foo = CLOSVAL(fun).closed, i = CLOSVAL(fun).count; i; foo = CDR(foo), i--) storevar(CAR(foo), i);
  for(foo = CLOSVAL(fun).cvalues; !ISNIL(foo); foo = CDR(foo))
  {
    send(CAR(foo));
    next();
  }
  fun = CLOSVAL(fun).cfunction;
  Link();
  dest = pop_point();
  auto envir = pop_point();
  cont = pop_func();
  push_point(envir);
  push_func(evclosure1);
  return 0;
}

static int evclosure1()
{
  restore_env();
  env = pop_env();
  cont = pop_func();
  return 0;
}

static int evsequence()
{
  if(EQ(args, C_NIL))
  {
    cont = pop_func();
  }
  else
  {
    expression = CAR(args);
    cont = evseq1;
  }
  return 0;
}

static int evseq1()
{
  if(EQ(CDR(args), C_NIL))
  {
    cont = peval;
  }
  else
  {
    push_func(evseq3);
    cont = peval;
  }
  return 0;
}

static int evseq3()
{
  args = CDR(args);
  expression = CAR(args);
  cont = evseq1;
  return 0;
}

PRIMITIVE baktrace()
{
  for(int i = toctrl; i >= 0; i--)
  {
    fprintf(primerr, "%d: ", i);
    if(control[i].type == CTRL_LISP && TYPEOF(control[i].u.lisp) != NIL)
      xprint(control[i].u.lisp, C_T);
    else
    {
      if(control[i].u.f_point == ev0)
        fprintf(primerr, "ev0\n");
      else if(control[i].u.f_point == peval)
        fprintf(primerr, "peval\n");
      else if(control[i].u.f_point == peval1)
        fprintf(primerr, "peval1\n");
      else if(control[i].u.f_point == peval2)
        fprintf(primerr, "peval2\n");
      else if(control[i].u.f_point == ev1)
        fprintf(primerr, "ev1\n");
      else if(control[i].u.f_point == ev2)
        fprintf(primerr, "ev2\n");
      else if(control[i].u.f_point == ev3)
        fprintf(primerr, "ev3\n");
      else if(control[i].u.f_point == ev4)
        fprintf(primerr, "ev4\n");
      else if(control[i].u.f_point == evlam1)
        fprintf(primerr, "evlam1\n");
      else if(control[i].u.f_point == evlam0)
        fprintf(primerr, "evlam0\n");
      else if(control[i].u.f_point == ev9)
        fprintf(primerr, "ev9\n");
      else if(control[i].u.f_point == ev11)
        fprintf(primerr, "ev11\n");
      else if(control[i].u.f_point == ev3p)
        fprintf(primerr, "ev3p\n");
      else if(control[i].u.f_point == evalargs)
        fprintf(primerr, "evalargs\n");
      else if(control[i].u.f_point == noevarg)
        fprintf(primerr, "noevarg\n");
      else if(control[i].u.f_point == evlam)
        fprintf(primerr, "evlam\n");
      else if(control[i].u.f_point == spread)
        fprintf(primerr, "spread\n");
      else if(control[i].u.f_point == evlis)
        fprintf(primerr, "evlis\n");
      else if(control[i].u.f_point == evlis1)
        fprintf(primerr, "evlis1\n");
      else if(control[i].u.f_point == evlis2)
        fprintf(primerr, "evlis2\n");
      else if(control[i].u.f_point == evlis3)
        fprintf(primerr, "evlis3\n");
      else if(control[i].u.f_point == evlis4)
        fprintf(primerr, "evlis4\n");
      else if(control[i].u.f_point == noev9)
        fprintf(primerr, "noev9\n");
      else if(control[i].u.f_point == evsequence)
        fprintf(primerr, "evsequence\n");
      else if(control[i].u.f_point == evseq1)
        fprintf(primerr, "evseq1\n");
      else if(control[i].u.f_point == evseq3)
        fprintf(primerr, "evseq3\n");
      else if(control[i].u.f_point == evclosure)
        fprintf(primerr, "evclosure\n");
      else if(control[i].u.f_point == evclosure1)
        fprintf(primerr, "evclosure1\n");
      else if(control[i].u.f_point == eval0)
        fprintf(primerr, "eval0\n");
      else if(control[i].u.f_point == apply0)
        fprintf(primerr, "apply0\n");
      else if(control[i].u.f_point == everr)
        fprintf(primerr, "everr\n");
      else if(control[i].u.f_point == lookup)
        fprintf(primerr, "lookup\n");
      else
        fprintf(stderr, "Unknown control stack element\n");
    }
  }
  return C_NIL;
}

void init_ev()
{
  mkprim(PN_E, eval, 1, FSUBR);
  mkprim(PN_EVAL, eval, 1, SUBR);
  mkprim(PN_APPLY, apply, 2, SUBR);
  mkprim(PN_APPLYSTAR, apply, -2, SUBR);
  mkprim(PN_BAKTRACE, baktrace, 0, SUBR);
}

/*
 * Lips, lisp shell.
 * Copyright 1988, 2020 Krister Joas
 *
 */

#include "libisp.hh"

extern lisp::LISPT findalias(lisp::LISPT);
extern void pputc(int, FILE*);
extern bool brkflg;
extern bool interrupt;

namespace lisp
{
bool evaluator::noeval = 0;
evaluator::continuation_t evaluator::cont = nullptr;
evaluator::breakhook_t evaluator::breakhook = nullptr; // Called before going into break.
evaluator::undefhook_t evaluator::undefhook = nullptr; // Called in case of undefined function.

LISPT evaluator::fun = nullptr;                /* Store current function being evaluated. */
LISPT evaluator::expression = nullptr;         /* Current expression. */
LISPT evaluator::args = nullptr;               /* Current arguments. */
alloc::destblock_t* evaluator::env = nullptr;  /* Current environment. */
alloc::destblock_t* evaluator::dest = nullptr; /* Current destination beeing built. */

evaluator::control_t evaluator::control[]; /* Control-stack. */
int evaluator::toctrl = 0;                 /* Control-stack stack pointer. */

void evaluator::reset()
{
  dzero();
  toctrl = 0;
  fun = C_NIL;
  args = C_NIL;
  env = nullptr;
}

LISPT evaluator::printwhere()
{
  LISPT foo = C_NIL;
  for(int i = toctrl - 1; i; i--) /* Find latest completed call */
  {
    if(control[i].type == CTRL_FUNC && control[i].u.f_point == evlam0)
      for(; i; i--)
      {
        if(control[i].type == CTRL_FUNC && control[i].u.f_point == ev0 && control[i - 1].type == CTRL_LISP
          && (type_of(control[i - 1].u.lisp) == CONS && type_of(control[i - 1].u.lisp->car()) != CONS))
        {
          foo = control[i - 1].u.lisp;
          fprintf(primerr, " [in ");
          prin2(foo->car(), C_T);
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
void evaluator::abort(int m, LISPT v)
{
  error(m, v);
  printwhere();
  unwind();
  throw lisp_error("abort");
}

void evaluator::overflow() { abort(STACK_OVERFLOW, C_NIL); }

/* 
 * These macros handles the control stack.  The control stack stores
 * continuations, destinations, and LISPT objects.  There are two macros 
 * to push and to pop pointers and LISPT objects.
 */
void evaluator::push_lisp(LISPT a)
{
  control[toctrl].type = CTRL_LISP;
  control[toctrl++].u.lisp = a;
  if(toctrl >= CTRLBLKSIZE)
    overflow();
}

void evaluator::push_point(alloc::destblock_t* d)
{
  control[toctrl].type = CTRL_POINT;
  control[toctrl++].u.point = d;
  if(toctrl >= CTRLBLKSIZE)
    overflow();
}

void evaluator::push_func(continuation_t f)
{
  control[toctrl].type = CTRL_FUNC;
  control[toctrl++].u.f_point = f;
  if(toctrl >= CTRLBLKSIZE)
    overflow();
}

LISPT evaluator::pop_lisp() { return control[--toctrl].u.lisp; }

alloc::destblock_t* evaluator::pop_point() { return control[--toctrl].u.point; }

evaluator::continuation_t evaluator::pop_func() { return control[--toctrl].u.f_point; }

/*
 * This function prints an error message, and sets up a call
 * to everr that handles breaks.
 */
void evaluator::xbreak(int mess, LISPT fault, continuation_t next)
{
  if(mess != 0)
  {
    perror(mess, fault);
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
 *               's' and initializes it.
 */
alloc::destblock_t* evaluator::mkdestblock(int s)
{
  auto dest = dalloc(s + 1);
  dest[0].var.d_integer = s;
  dest[0].val.d_integer = s;
  dest[0].type = alloc::block_type::EMPTY;
  return dest;
}

void evaluator::storevar(LISPT v, int i)
{
  dest[i].var.d_lisp = v;
  dest[i].type = alloc::block_type::LISPT;
}

alloc::destblock_t* evaluator::pop_env()
{
  dfree(env);
  return pop_point();
}

void evaluator::send(LISPT a)
{
  if(dest[0].var.d_integer > 0)
    dest[dest[0].var.d_integer].val.d_lisp = a;
}

LISPT evaluator::receive() { return dest[dest[0].var.d_integer].val.d_lisp; }

void evaluator::next()
{
  if(dest[0].var.d_integer > 0)
    --dest[0].var.d_integer;
}

/* 
 * Make a call to the function in parameter `fun'.  It can handle
 * functions with up to three arguments.
 */
LISPT evaluator::call(LISPT fun)
{
  LISPT foo = C_NIL;

  switch(fun->subrval().argcount)
  {
    case 0:
      foo = (*(fun->subrval().function0))();
      break;
    case 1:
    case -1:
      foo = (*(fun->subrval().function1))(dest[1].val.d_lisp);
      break;
    case 2:
    case -2:
      foo = (*(fun->subrval().function2))(dest[2].val.d_lisp, dest[1].val.d_lisp);
      break;
    case 3:
    case -3:
      foo = (*(fun->subrval().function3))(dest[3].val.d_lisp, dest[2].val.d_lisp, dest[1].val.d_lisp);
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
PRIMITIVE evaluator::eval(LISPT expr)
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

bool evaluator::eval0()
{
  dfree(dest);
  return true;
}

/*
Dummy definition for the pretty printer.
PRIMITIVE apply(f, a)
*/
PRIMITIVE evaluator::apply(LISPT f, LISPT a)
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

bool evaluator::apply0()
{
  dfree(dest);
  args = pop_lisp();
  fun = pop_lisp();
  return true;
}

bool evaluator::ev0()
{
  /* 
   * Discard the top of stack (it's the previous expression, see
   * comment in `eval' above) and restore continuation.  The function
   * `ev0' is also used as a placeholder for the beginning of an eval.
   */
  toctrl -= 1;
  cont = pop_func();
  return false;
}

bool evaluator::peval()
{
#ifdef TRACE
  if(trace)
    xprint(expression, C_T);
#endif /* TRACE */
  push_lisp(expression);
  push_func(ev0);
  switch(type_of(expression))
  {
    case CONS:
      push_lisp(fun);
      fun = expression->car();
      push_lisp(args);
      args = expression->cdr();
      push_func(ev1);
      cont = peval1;
      break;
    case SYMBOL:
      cont = lookup;
      break;
    case INDIRECT:
      send(expression->indirectval());
      cont = pop_func();
      break;
    case CVARIABLE:
      send(*expression->cvarval());
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
  return false;
}

bool evaluator::ev1()
{
  args = pop_lisp();
  fun = pop_lisp();
  cont = pop_func();
  return false;
}

bool evaluator::evalhook(LISPT exp)
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
        return false;
        break;
    }
  return true;
}

void evaluator::do_unbound(continuation_t continuation)
{
  /* 
   * If an undefined symbol has the AUTOLOAD property, we try to
   * load the definition from a file.  If that doesn't succeed, then
   * the symbol is undefined.
   */
  LISPT al = getprop(expression->car(), C_AUTOLOAD);
  if(!is_NIL(al))
  {
    push_lisp(expression);
    push_point(dest);
    load(al);
    dest = pop_point();
    expression = pop_lisp();
    fun = expression->car()->symvalue();
    if(type_of(fun) == UNBOUND)
    {
      if(!evalhook(expression))
        xbreak(UNDEF_FUNCTION, expression->car(), continuation);
    }
    else
      cont = continuation;
  }
  else
  {
    expression = findalias(expression);
    if(type_of(expression) == CONS && type_of(expression->car()) == SYMBOL
      && type_of(expression->car()->symvalue()) == UNBOUND)
    {
      if(!evalhook(expression))
        xbreak(UNDEF_FUNCTION, expression->car(), continuation);
    }
    else
    {
      fun = expression->car();
      args = expression->cdr();
      cont = continuation;
    }
  }
}

bool evaluator::do_default(continuation_t continuation)
{
  expression = findalias(expression);
  if(type_of(expression) == CONS && type_of(expression->car()) == SYMBOL
    && type_of(expression->car()->symvalue()) == UNBOUND)
  {
    if(!evalhook(expression))
      xbreak(UNDEF_FUNCTION, expression->car(), continuation);
    return true;
  }
  else
    return false;
}

bool evaluator::peval1()
{
  int foo;

  if(brkflg)
    xbreak(KBD_BREAK, fun, peval1);
  else if(interrupt)
    abort(NO_MESSAGE, C_NIL);
  else
    switch(type_of(fun))
    {
      case CLOSURE:
        push_func(peval1);
        cont = evclosure;
        break;
      case SUBR:
        push_point(dest);
        push_func(ev2);
        if((foo = fun->subrval().argcount) < 0)
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
        if((foo = fun->subrval().argcount) < 0)
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
        fun = fun->symvalue();
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
  return false;
}

bool evaluator::peval2()
{
  int foo;

  if(brkflg)
    xbreak(KBD_BREAK, fun, peval2);
  else
    switch(type_of(fun))
    {
      case CLOSURE:
        push_func(peval2);
        cont = evclosure;
        break;
      case SUBR:
      case FSUBR:
        push_point(dest);
        push_func(ev2);
        if((foo = fun->subrval().argcount) < 0)
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
        fun = fun->symvalue();
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
  return false;
}

/*
 * bt - Prints a backtrace of all expressions on the
 *    	stack.
 */
void evaluator::bt()
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

bool evaluator::everr()
{
  expression = break0(expression);
  cont = pop_func(); /* Discard one continuation. */
  cont = pop_func();
  return false;
}

bool evaluator::noevarg()
{
  args = receive();
  cont = spread;
  return false;
}

bool evaluator::evalargs()
{
  if(is_NIL(args))
  {
    cont = pop_func();
  }
  else
  {
    expression = args->car();
    if(noeval)
      cont = noev9;
    else
      cont = ev9;
  }
  return false;
}

bool evaluator::ev9()
{
  if(is_NIL(args->cdr()))
  {
    cont = peval;
  }
  else
  {
    push_func(ev11);
    cont = peval;
  }
  return false;
}

bool evaluator::ev11()
{
  next();
  args = args->cdr();
  expression = args->car();
  cont = ev9;
  return false;
}

bool evaluator::noev9()
{
nextarg:
  if(is_NIL(args->cdr()))
  {
    send(expression);
    cont = pop_func();
  }
  else
  {
    send(expression);
    next();
    args = args->cdr();
    expression = args->car();
    goto nextarg;
  }
  return false;
}

bool evaluator::evlis()
{
  if(is_NIL(args))
  {
    cont = pop_func();
  }
  else
  {
    expression = args->car();
    cont = evlis1;
  }
  return false;
}

bool evaluator::evlis1()
{
  if(is_NIL(args->cdr()))
  {
    push_func(evlis2);
    cont = peval;
  }
  else
  {
    push_func(evlis3);
    cont = peval;
  }
  return false;
}

bool evaluator::evlis2()
{
  LISPT x = cons(receive(), C_NIL);
  send(x);
  cont = pop_func();
  return false;
}

bool evaluator::evlis3()
{
  push_point(dest);
  dest = mkdestblock(1);
  push_func(evlis4);
  args = args->cdr();
  expression = args->car();
  cont = evlis1;
  return false;
}

bool evaluator::evlis4()
{
  LISPT x = receive();
  dfree(dest);
  dest = pop_point();
  x = cons(receive(), x);
  send(x);
  cont = pop_func();
  return false;
}

bool evaluator::evlam()
{
  int i;
  int ac;
  LISPT foo;

  push_lisp(expression);
  push_point(env);
  push_point(dest);
  int spr = 0;
  if((ac = fun->lamval().argcnt) < 0)
  {
    ac = -ac;
    spr++;
  }
  dest = mkdestblock(ac);
  for(foo = fun->lamval().arglist, i = ac; i; foo = foo->cdr(), i--) storevar(foo->car(), i);
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
  return false;
}

bool evaluator::spread()
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
    send(args->car());
    next();
    args = args->cdr();
    goto respread;
  }
  return false;
}

bool evaluator::ev2()
{
  try
  {
    auto foo = call(fun);
    dfree(dest);
    dest = pop_point();
    send(foo);
    cont = pop_func();
  }
  catch(const lisp_reset&)
  {}
  catch(const lisp_error& ex)
  {
    // fprintf(primerr, "%s ", ex.what());
    auto foo = printwhere();
    if(is_NIL(foo))
      xbreak(0, C_NIL, peval1);
    else
      xbreak(0, foo->car(), peval1); /* CAR(_) broken */
  }
  return false;
}

bool evaluator::ev3()
{
  fun = receive();
  push_func(ev4);
  cont = peval1;
  return false;
}

bool evaluator::ev3p()
{
  fun = receive();
  push_func(ev4);
  cont = peval2;
  return false;
}

bool evaluator::ev4()
{
  cont = pop_func();
  return false;
}

void evaluator::link()
{
  dest[0].var.d_environ = env;
  // dest[0].val.d_integer contains the number of var/val pairs
  dest[0].type = alloc::block_type::ENVIRON;
  env = dest;
  for(auto i = dest[0].val.d_integer; i > 0; i--)
  {
    LISPT t = dest[i].var.d_lisp->symvalue();
    dest[i].var.d_lisp->symvalue(dest[i].val.d_lisp);
    dest[i].val.d_lisp = t;
  }
}

bool evaluator::evlam1()
{
  link();
  dest = pop_point();
  args = fun->lamval().lambdarep;
  push_func(evlam0);
  cont = evsequence;
  return false;
}

void evaluator::restore_env()
{
  auto* c = env;
  for(auto i = c[0].val.d_integer; i > 0; i--) c[i].var.d_lisp->symvalue(c[i].val.d_lisp);
}

bool evaluator::evlam0()
{
  restore_env();
  env = pop_env();
  expression = pop_lisp();
  cont = pop_func();
  return false;
}

void evaluator::unwind()
{
  while(env != nullptr)
  {
    restore_env();
    env = env->var.d_environ;
  }
}

bool evaluator::lookup()
{
  LISPT t = expression->symvalue();
  switch(type_of(t))
  {
    case UNBOUND:
      xbreak(UNBOUND_VARIABLE, expression, lookup);
      return false;
      break;
    case INDIRECT:
      send(t->indirectval());
      break;
    case CVARIABLE:
      send(*t->cvarval());
      break;
    default:
      send(t);
      break;
  }
  cont = pop_func();
  return false;
}

bool evaluator::evclosure()
{
  LISPT foo;
  int i;

  push_point(env);
  push_point(dest);
  dest = mkdestblock(fun->closval().count);
  for(foo = fun->closval().closed, i = fun->closval().count; i; foo = foo->cdr(), i--) storevar(foo->car(), i);
  for(foo = fun->closval().cvalues; !is_NIL(foo); foo = foo->cdr())
  {
    send(foo->car());
    next();
  }
  fun = fun->closval().cfunction;
  link();
  dest = pop_point();
  auto envir = pop_point();
  cont = pop_func();
  push_point(envir);
  push_func(evclosure1);
  return false;
}

bool evaluator::evclosure1()
{
  restore_env();
  env = pop_env();
  cont = pop_func();
  return false;
}

bool evaluator::evsequence()
{
  if(EQ(args, C_NIL))
  {
    cont = pop_func();
  }
  else
  {
    expression = args->car();
    cont = evseq1;
  }
  return false;
}

bool evaluator::evseq1()
{
  if(EQ(args->cdr(), C_NIL))
  {
    cont = peval;
  }
  else
  {
    push_func(evseq3);
    cont = peval;
  }
  return false;
}

bool evaluator::evseq3()
{
  args = args->cdr();
  expression = args->car();
  cont = evseq1;
  return false;
}

PRIMITIVE evaluator::baktrace()
{
  for(int i = toctrl; i >= 0; i--)
  {
    fprintf(primerr, "%d: ", i);
    switch(control[i].type)
    {
      case CTRL_LISP:
        xprint(control[i].u.lisp, C_T);
        break;
      case CTRL_POINT:
        fprintf(primerr, "destblock\n");
        break;
      case CTRL_FUNC:
        if(control[i].u.f_point == ev0)
          fprintf(primerr, "ev0\n");
        else if(control[i].u.f_point == peval)
          fprintf(primerr, "peval\n");
        else if(control[i].u.f_point == peval1)
          fprintf(primerr, "peval1\n");
        else if(control[i].u.f_point == peval2)
          fprintf(primerr, "peval2\n");
        else if(control[i].u.f_point == ev0)
          fprintf(primerr, "ev0\n");
        else if(control[i].u.f_point == ev1)
          fprintf(primerr, "ev1\n");
        else if(control[i].u.f_point == ev2)
          fprintf(primerr, "ev2\n");
        else if(control[i].u.f_point == ev3)
          fprintf(primerr, "ev3\n");
        else if(control[i].u.f_point == ev4)
          fprintf(primerr, "ev4\n");
        else if(control[i].u.f_point == evlam0)
          fprintf(primerr, "evlam0\n");
        else if(control[i].u.f_point == evlam1)
          fprintf(primerr, "evlam1\n");
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
        break;
    }
  }
  return C_NIL;
}

void evaluator::init()
{
  alloc::add_mark_object(&evaluator::fun);
  alloc::add_mark_object(&evaluator::expression);
  alloc::add_mark_object(&evaluator::args);
  mkprim(PN_E, eval, 1, FSUBR);
  mkprim(PN_EVAL, eval, 1, SUBR);
  mkprim(PN_APPLY, apply, 2, SUBR);
  mkprim(PN_APPLYSTAR, apply, -2, SUBR);
  mkprim(PN_BAKTRACE, baktrace, 0, SUBR);
}

} // namespace lisp

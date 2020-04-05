/*
 * Lips, lisp shell.
 * Copyright 1988, Krister Joas
 *
 * $Id$
 *
 */
#include <setjmp.h>
#include "lisp.h"

#ifndef lint
static char rcsid[] = "$Id$";
#endif

extern jmp_buf toplevel;
extern int brkflg;
extern int interrupt;
extern LISPT findalias();

public void (*breakhook)();	/* Called before going into break. */
public int (*undefhook)();	/* Called in case of undefined function. */

/* 
 * These variables are really local to this file but are needed
 * by the garbage collector.
 */
public LISPT fun;		/* Store current function beeing evaluated. */
public LISPT exp;		/* Current expression. */
public LISPT args;		/* Current arguments. */
public struct destblock *env;	/* Current environment. */
public struct destblock *dest;	/* Current destination beeing built. */

public CONTROL control;		/* Control-stack. */
public int toctrl;		/* Control-stack stack pointer. */

private int peval();
private int peval1();
private int peval2();
private int ev0(), ev1(), ev2(), ev3(), ev4(), evlam1(), evlam0();
private int ev9(), ev11(), ev3p();
private int evalargs(), noevarg(), evlam(), spread();
private int evlis(), evlis1(), evlis2(), evlis3(), evlis4();
private int noev9();
private int evsequence(), evseq1(), evseq3();
private int evclosure(), evclosure1();
private int eval0(), apply0();
private int everr();
private int lookup();

private int noeval;		/* Don't evaluate arguments. */
private int (*cont)();		/* Current continuation. */

/*
 * This macro prints an error message, and sets up a call
 * to everr that handles breaks.
 */
#define BREAK(mess, fault, next) \
			{ \
			  if (mess != 0) \
			    { \
			      (void) error(mess, fault); \
			      (void) printwhere(); \
			    } \
			  if (breakhook != NULL) (*breakhook)(); \
			  if (env == NULL) \
			    longjmp(toplevel, 2); \
			  (void) xprint(cons(fault, \
					     cons(C_BROKEN, C_NIL)), C_T); \
			  PUSH_FUNC(next); \
			  cont = everr; \
			}

/*
 * Print errormessage, abort current evaluation, and
 * return to top level.
 */
#define ABORT(m, v) \
			{ \
			  (void) error(m, v); \
			  (void) printwhere(); \
			  unwind(); \
			  longjmp(toplevel, 2); \
			}

/* 
 * These macros handles the control stack.  The control stack stores
 * continuations, destinations, and LISPT objects.  There are two macros 
 * to push and to pop pointers and LISPT objects.
 */
#define PUSH_LISP(a)	control[toctrl].type = CTRL_LISP; \
			control[toctrl++].u.lisp = (a); \
			if (toctrl >= CTRLBLKSIZE) overflow()
#define PUSH_POINT(a)	control[toctrl].type = CTRL_POINT; \
			control[toctrl++].u.point = (a); \
			if (toctrl >= CTRLBLKSIZE) overflow()
#define PUSH_FUNC(a)	control[toctrl].type = CTRL_FUNC; \
			control[toctrl++].u.f_point = (a); \
			if (toctrl >= CTRLBLKSIZE) overflow()
#define POP_LISP	(control[--toctrl].u.lisp)
#define POP_POINT	(control[--toctrl].u.point)
#define POP_FUNC	(control[--toctrl].u.f_point)

/* 
 * The macro MKDESTBLOCK creates a new destination block of size
 * `s' and initializes it.
 */
#define MKDESTBLOCK(s)	dalloc((s)+1); \
			dest[0].var.d_integer = (s); \
			dest[0].val.d_integer = (s); \
			dest[0].type = dest[0].type = 0

#define STOREVAR(v, i)	dest[i].var.d_lisp = (v); dest[i].type = 1
#define UNLINK		dfree(env); \
			env = POP_POINT

#define SEND(a)		if (dest[0].var.d_integer > 0) \
			  dest[dest[0].var.d_integer].val.d_lisp = (a)
#define RECEIVE		dest[dest[0].var.d_integer].val.d_lisp
#define NEXT		if (dest[0].var.d_integer > 0) \
			  dest[0].var.d_integer--

/* Use shallow binding. */
#define SHALLOW

/* Just a convenience macro. */
#define CALL            (*(SUBRVAL(fun).function))

private LISPT printwhere()
{
  LISPT foo;
  int i;

  foo = C_NIL;
  for (i = toctrl-1; i; i--)	/* find latest completed call */
    if (control[i].type == CTRL_FUNC && control[i].u.f_point == evlam0)
      for (; i; i--)
	{
	  if (control[i].type == CTRL_FUNC && control[i].u.f_point == ev0
	      && control[i-1].type == CTRL_LISP
	      && (TYPEOF(control[i-1].u.lisp) == CONS
		  && TYPEOF(CAR(control[i-1].u.lisp)) != CONS))
	    {
	      foo = control[i-1].u.lisp;
	      (void) fprintf(primerr, " [in ");
	      (void) prin2(CAR(foo), C_T);
	      pputc(']', primerr);
	      goto out;
	    }
	}
out:
  pputc('\n', primerr);
  return foo;
}

private overflow()
{ 
  ABORT(STACK_OVERFLOW, C_NIL);
}

/* 
 * Make a call to the function in parameter `fun'.  It can handle
 * functions with up to three arguments.
 */
private LISPT call(fun)
  LISPT fun;
{
  register LISPT foo;

  switch(SUBRVAL(fun).argcount)
    {
    case 0:
      foo = CALL();
      break;
    case 1: case -1:
      foo = CALL(dest[1].val.d_lisp);
      break;
    case 2: case -2:
      foo = CALL(dest[2].val.d_lisp, dest[1].val.d_lisp);
      break;
    case 3: case -3:
      foo = CALL(dest[3].val.d_lisp, dest[2].val.d_lisp, dest[1].val.d_lisp);
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
PRIMITIVE eval(expr)
*/
PRIMITIVE eval(expr)
  LISPT expr;
{
  register LISPT foo;

  /* 
   * Set the current expression to `expr' and push the current
   * destination onto the control stack.  (Why isn't `exp' pushed?)
   */
  exp = expr;
  PUSH_POINT(dest);
  /* 
   * The result of evalutating `expr' is stored in the destination,
   * which is retrieved with the RECEIVE macro.
   */
  dest = MKDESTBLOCK(1);
  /* 
   * This how it works in general: Push the function to be called
   * last, and set the continuation variable `cont' to `peval'.
   * `peval' may push more contiuations onto the stack but eventaully
   * `eval0' is called which returns 1, signalling end of evaluation.
   */
  PUSH_FUNC(eval0);
  cont = peval;
  while (!(*cont)())
    ;
  /* 
   * Retrieve the result of the evaluation and restore the previous
   * destination.
   */
  foo = RECEIVE;
  dest = (struct destblock *) POP_POINT;
  /* 
   * Return the result.
   */
  return foo;
}

private eval0()
{
  dfree(dest);
  return 1;
}

/*
Dummy definition for the pretty printer.
PRIMITIVE apply(f, a)
*/
PRIMITIVE apply(f, a)
  LISPT f, a;
{
  register LISPT foo;

  PUSH_POINT(dest);
  dest = MKDESTBLOCK(1);
  PUSH_LISP(fun);
  fun = f;
  PUSH_LISP(args);
  args = a;
  exp = cons(f, a);
  PUSH_FUNC(apply0);
  cont = peval2;
  while (!(*cont)())
    ;
  foo = RECEIVE;
  dest = (struct destblock *) POP_POINT;
  return foo;
}

private apply0()
{
  dfree(dest);
  args = POP_LISP;
  fun = POP_LISP;
  return 1;
}

private ev0()
{
  /* 
   * Discard the top of stack (it's the previous expression, see
   * comment in `eval' above) and restore continuation.  The function
   * `ev0' is also used as a placeholder for the beginning of an eval.
   */
  toctrl -= 1;
  cont = POP_FUNC;
  return 0;
}

private peval()
{
#ifdef TRACE
  if (trace) xprint(exp, C_T);
#endif /* TRACE */
  PUSH_LISP(exp);
  PUSH_FUNC(ev0);
  switch (TYPEOF(exp))
    {
    case CONS:
      PUSH_LISP(fun);
      fun = CAR(exp);
      PUSH_LISP(args);
      args = CDR(exp);
      PUSH_FUNC(ev1);
      cont = peval1;
      break;
    case SYMBOL:
      cont = lookup;
      break;
    case INDIRECT:
      SEND(INDIRECTVAL(exp));
      cont = POP_FUNC;
      break;
    case CVARIABLE:
      SEND(*CVARVAL(exp));
      cont = POP_FUNC;
      break;
    case FREE:
      ABORT(CORRUPT_DATA, exp);
      break;
    default:
      SEND(exp);
      cont = POP_FUNC;
      break;
    }
  return 0;
}

private ev1()
{
  args = POP_LISP;
  fun = POP_LISP;
  cont = POP_FUNC;
  return 0;
}

private int
evalhook(exp)
  LISPT exp;
{
  LISPT res;

  if (undefhook != NULL)
    switch ((*undefhook)(exp, &res))
      {
      case 1:
	SEND(res);
	cont = POP_FUNC;
	break;
      case -1:
	ABORT(NO_MESSAGE, C_NIL);
	break;
      default:	
	return 0;
	break;
      }
  return 1;
}

void
do_unbound(continuation)
  int (*continuation)();
{
  LISPT al;
  
  /* 
   * If an undefined symbol has the AUTOLOAD property, we try to
   * load the definition from a file.  If that doesn't succeed, then
   * the symbol is undefined.
   */
  al = getprop(CAR(exp), C_AUTOLOAD);
  if (!ISNIL(al))
    {
      PUSH_LISP(exp);
      PUSH_POINT(dest);
      (void) load(al);
      dest = (struct destblock *) POP_POINT;
      exp = POP_LISP;
      fun = SYMVALUE(CAR(exp));
      if (TYPEOF(fun) == UNBOUND)
	{
	  if (!evalhook(exp))
	    BREAK(UNDEF_FUNCTION, CAR(exp), continuation);
	}
      else
        cont = continuation;
    }
  else
    {
      exp = findalias(exp);
      if (EQ(exp, C_ERROR))
        ABORT(NO_MESSAGE, C_NIL);
      if (TYPEOF(exp) == CONS && TYPEOF(CAR(exp)) == SYMBOL
	  && TYPEOF(SYMVALUE(CAR(exp))) == UNBOUND)
	{
	  if (!evalhook(exp))
	    BREAK(UNDEF_FUNCTION, CAR(exp), continuation);
        }
      else
        {
          fun = CAR(exp);
          args = CDR(exp);
          cont = continuation;
        }
    }
}

int do_default(continuation)
  int (*continuation)();
{
  exp = findalias(exp);
  if (EQ(exp, C_ERROR))
    ABORT(NO_MESSAGE, C_NIL);
  if (TYPEOF(exp) == CONS && TYPEOF(CAR(exp)) == SYMBOL
      && TYPEOF(SYMVALUE(CAR(exp))) == UNBOUND)
    {
      if (!evalhook(exp))
	BREAK(UNDEF_FUNCTION, CAR(exp), continuation);
      return 1;
    }
  else return 0;
}

private peval1()
{
  register foo;

  if (brkflg) BREAK(KBD_BREAK, fun, peval1)
  else if (interrupt)
    ABORT(NO_MESSAGE, C_NIL)
  else switch(TYPEOF(fun))
    {
    case CLOSURE:
      PUSH_FUNC(peval1);
      cont = evclosure;
      break;
    case SUBR:
      PUSH_POINT(dest);
      PUSH_FUNC(ev2);
      if ((foo = SUBRVAL(fun).argcount) < 0)
        {
          dest = MKDESTBLOCK(-foo);
          PUSH_FUNC(noevarg);
          cont = evlis;
        }
      else
        {
          dest = MKDESTBLOCK(foo);
          noeval = 0;
          cont = evalargs;
        }
      break;
    case FSUBR:
      PUSH_POINT(dest);
      PUSH_FUNC(ev2);
      if ((foo = SUBRVAL(fun).argcount) < 0)
        {
          dest = MKDESTBLOCK(-foo);
          cont = spread;
        }
      else
        {
          dest = MKDESTBLOCK(foo);
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
      exp = fun;
      PUSH_FUNC(ev3);
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
      if (!evalhook(exp))
	BREAK(ILLEGAL_FUNCTION, fun, peval1);
      break;
    default:
      if (!do_default(peval1))
        BREAK(ILLEGAL_FUNCTION, fun, peval1);
      break;
    }
  return 0;
}

private peval2()
{
  register foo;

  if (brkflg) BREAK(KBD_BREAK, fun, peval2)
  else switch(TYPEOF(fun))
    {
    case CLOSURE:
      PUSH_FUNC(peval2);
      cont = evclosure;
      break;
    case SUBR:
    case FSUBR:
      PUSH_POINT(dest);
      PUSH_FUNC(ev2);
      if ((foo = SUBRVAL(fun).argcount) < 0)
        {
          dest = MKDESTBLOCK(-foo);
          cont = spread;
        }
      else
        {
          dest = MKDESTBLOCK(foo);
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
      exp = fun;
      PUSH_FUNC(ev3p);
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
      if (!evalhook(exp))
	BREAK(ILLEGAL_FUNCTION, fun, peval2);
      break;
    default:
      if (!do_default(peval2))
        BREAK(ILLEGAL_FUNCTION, fun, peval2);
      break;
  }
  return 0;
}

/*
 * bt - Prints a backtrace of all expressions on the
 *    	stack.
 */
public void
bt()
{
  int op;
  int i;

  op = printlevel;
  printlevel = 2;
  for (i = toctrl-1; i; i--)
    {
      if (control[i].type == CTRL_FUNC && control[i].u.f_point == ev0)
	(void) xprint(control[i-1].u.lisp, C_T);
    }
  printlevel = op;
}

private everr()
{
  exp = break0(exp);
  cont = POP_FUNC;		/* Discard one continuation. */
  cont = POP_FUNC;
  return 0;
}

private noevarg()
{
  args = RECEIVE;
  cont = spread;
  return 0;
}

private evalargs()
{
  if (ISNIL(args))
    {
      cont = POP_FUNC;
    }
  else
    {
      exp = CAR(args);
      if (noeval)
        cont = noev9;
      else
        cont = ev9;
    }
  return 0;
}

private ev9()
{
  if (ISNIL(CDR(args)))
    {
      cont = peval;
    }
  else
    {
      PUSH_FUNC(ev11);
      cont = peval;
    }
  return 0;
}

private ev11()
{
  NEXT;
  args = CDR(args);
  exp = CAR(args);
  cont = ev9;
  return 0;
}

private noev9()
{
nextarg:
  if (ISNIL(CDR(args)))
    {
      SEND(exp);
      cont = POP_FUNC;
    }
  else
    {
      SEND(exp);
      NEXT;
      args = CDR(args);
      exp = CAR(args);
      goto nextarg;
    }
  return 0;
}

private evlis()
{
  if (ISNIL(args))
    {
      cont = POP_FUNC;
    }
  else
    {
      exp = CAR(args);
      cont = evlis1;
    }
  return 0;
}

private evlis1()
{
  if (ISNIL(CDR(args)))
    {
      PUSH_FUNC(evlis2);
      cont = peval;
    }
  else
    {
      PUSH_FUNC(evlis3);
      cont = peval;
    }
  return 0;
}

private evlis2()
{
  LISPT x;
  
  x = cons(RECEIVE, C_NIL);
  SEND(x);
  cont = POP_FUNC;
  return 0;
}

private evlis3()
{
  PUSH_POINT(dest);
  dest = MKDESTBLOCK(1);
  PUSH_FUNC(evlis4);
  args = CDR(args);
  exp = CAR(args);
  cont = evlis1;
  return 0;
}

private evlis4()
{
  register LISPT x;
  
  x = RECEIVE;
  dfree(dest);
  dest = (struct destblock *) POP_POINT;
  x = cons(RECEIVE, x);
  SEND(x);
  cont = POP_FUNC;
  return 0;
}

private evlam()
{
  register int i;
  register int ac;
  register int spr;
  register LISPT foo;

  PUSH_LISP(exp);
  PUSH_POINT(env);
  PUSH_POINT(dest);
  spr = 0;
  if ((ac = LAMVAL(fun).argcnt) < 0)
    {
      ac = -ac;
      spr++;
    }
  dest = MKDESTBLOCK(ac);
  for (foo = LAMVAL(fun).arglist, i = ac;
       i; foo = CDR(foo), i--)
    STOREVAR(CAR(foo), i);
  PUSH_FUNC(evlam1);
  if (spr)
    {
      if (noeval)
        cont = spread;
      else
        {
          PUSH_FUNC(noevarg);
          cont = evlis;
        }
    }
  else
    cont = evalargs;
  return 0;
}

private spread()
{
respread:
  if (EQ(args, C_NIL))
    {
      cont = POP_FUNC;
    }
  else if ((dest[0].var.d_integer) == 1)
    {
      SEND(args);
      cont = POP_FUNC;
    }
  else
    {
      SEND(CAR(args));
      NEXT;
      args = CDR(args);
      goto respread;
    }
  return 0;
}

private ev2()
{
  register LISPT foo;

  foo = call(fun);
  dfree(dest);
  dest = (struct destblock *) POP_POINT;
  if (EQ(foo, C_ERROR))
    {
      foo = printwhere();
      BREAK(0, CAR(foo), peval1); /* CAR(_) broken */
    }
  else
    {
      SEND(foo);
      cont = POP_FUNC;
    }
  return 0;
}

private ev3()
{
  fun = RECEIVE;
  PUSH_FUNC(ev4);
  cont = peval1;
  return 0;
}

private ev3p()
{
  fun = RECEIVE;
  PUSH_FUNC(ev4);
  cont = peval2;
  return 0;
}

private ev4()
{
  cont = POP_FUNC;
  return 0;
}

private Link()
{
  register long i;
  register LISPT t;

  dest[0].var.d_environ = env;
  dest[0].type = 2;
  env = (struct destblock *) &dest[0];
  for (i = dest[0].val.d_integer; i>0; i--)
    {
      t = SYMVALUE(dest[i].var.d_lisp);
      SYMVALUE(dest[i].var.d_lisp) = dest[i].val.d_lisp;
      dest[i].val.d_lisp = t;
    }
}

private evlam1()
{
  Link();
  dest = (struct destblock *) POP_POINT;
  args = LAMVAL(fun).lambdarep;
  PUSH_FUNC(evlam0);
  cont = evsequence;
  return 0;
}

private  unLink()
{
  register long i;
  register struct destblock *c;

  c = env;
  for (i = c[0].val.d_integer; i>0; i--)
    SYMVALUE(c[i].var.d_lisp) = c[i].val.d_lisp;
}

private evlam0()
{
  unLink();
  UNLINK;
  exp = POP_LISP;
  cont = POP_FUNC;
  return 0;
}

public void unwind()
{
  while (env != NULL)
    {
      unLink();
      env = env->var.d_environ;
    }
}

private lookup()
{
  register LISPT t;

  t = SYMVALUE(exp);
  switch (TYPEOF(t))
    {
      case UNBOUND:
        BREAK(UNBOUND_VARIABLE, exp, lookup);
        return 0;
        break;
      case INDIRECT:
        SEND(INDIRECTVAL(t));
        break;
      case CVARIABLE:
        SEND(*CVARVAL(t));
        break;
      default:
        SEND(t);
        break;
    }
  cont = POP_FUNC;
  return 0;
}

private evclosure()
{
  register LISPT foo;
  register int i;
  register struct destblock *envir;

  PUSH_POINT(env);
  PUSH_POINT(dest);
  dest = MKDESTBLOCK(CLOSVAL(fun).count);
  for (foo = CLOSVAL(fun).closed, i = CLOSVAL(fun).count;
       i; foo = CDR(foo), i--)
    STOREVAR(CAR(foo), i);
  for (foo = CLOSVAL(fun).cvalues; !ISNIL(foo); foo = CDR(foo))
    {
      SEND(CAR(foo));
      NEXT;
    }
  fun = CLOSVAL(fun).cfunction;
  Link();
  dest = (struct destblock *) POP_POINT;
  envir = (struct destblock *) POP_POINT;
  cont = POP_FUNC;
  PUSH_POINT(envir);
  PUSH_FUNC(evclosure1);
  return 0;
}

private evclosure1()
{
  unLink();
  UNLINK;
  cont = POP_FUNC;
  return 0;
}

private evsequence()
{
  if (EQ(args, C_NIL))
    {
      cont = POP_FUNC;
    }
  else
    {
      exp = CAR(args);
      cont = evseq1;
    }
  return 0;
}

private evseq1()
{
  if (EQ(CDR(args), C_NIL))
    {
      cont = peval;
    }
  else
    {
      PUSH_FUNC(evseq3);
      cont = peval;
    }
  return 0;
}

private evseq3()
{
  args = CDR(args);
  exp = CAR(args);
  cont = evseq1;
  return 0;
}

PRIMITIVE baktrace()
{
  int i;

  for (i=toctrl; i>=0; i--)
    {
      (void) fprintf(primerr, "%d: ", i);
      if (control[i].type == CTRL_LISP && TYPEOF(control[i].u.lisp) != NIL)
        (void) xprint(control[i].u.lisp, C_T);
      else
        {
          if (control[i].u.f_point == ev0)
            (void) fprintf(primerr, "ev0\n");
          else if (control[i].u.f_point == peval)
            (void) fprintf(primerr, "peval\n");
          else if (control[i].u.f_point == peval1)
            (void) fprintf(primerr, "peval1\n");
          else if (control[i].u.f_point == peval2)
            (void) fprintf(primerr, "peval2\n");
          else if (control[i].u.f_point == ev1)
            (void) fprintf(primerr, "ev1\n");
          else if (control[i].u.f_point == ev2)
            (void) fprintf(primerr, "ev2\n");
          else if (control[i].u.f_point == ev3)
            (void) fprintf(primerr, "ev3\n");
          else if (control[i].u.f_point == ev4)
            (void) fprintf(primerr, "ev4\n");
          else if (control[i].u.f_point == evlam1)
            (void) fprintf(primerr, "evlam1\n");
          else if (control[i].u.f_point == evlam0)
            (void) fprintf(primerr, "evlam0\n");
          else if (control[i].u.f_point == ev9)
            (void) fprintf(primerr, "ev9\n");
          else if (control[i].u.f_point == ev11)
            (void) fprintf(primerr, "ev11\n");
          else if (control[i].u.f_point == ev3p)
            (void) fprintf(primerr, "ev3p\n");
          else if (control[i].u.f_point == evalargs)
            (void) fprintf(primerr, "evalargs\n");
          else if (control[i].u.f_point == noevarg)
            (void) fprintf(primerr, "noevarg\n");
          else if (control[i].u.f_point == evlam)
            (void) fprintf(primerr, "evlam\n");
          else if (control[i].u.f_point == spread)
            (void) fprintf(primerr, "spread\n");
          else if (control[i].u.f_point == evlis)
            (void) fprintf(primerr, "evlis\n");
          else if (control[i].u.f_point == evlis1)
            (void) fprintf(primerr, "evlis1\n");
          else if (control[i].u.f_point == evlis2)
            (void) fprintf(primerr, "evlis2\n");
          else if (control[i].u.f_point == evlis3)
            (void) fprintf(primerr, "evlis3\n");
          else if (control[i].u.f_point == evlis4)
            (void) fprintf(primerr, "evlis4\n");
          else if (control[i].u.f_point == noev9)
            (void) fprintf(primerr, "noev9\n");
          else if (control[i].u.f_point == evsequence)
            (void) fprintf(primerr, "evsequence\n");
          else if (control[i].u.f_point == evseq1)
            (void) fprintf(primerr, "evseq1\n");
          else if (control[i].u.f_point == evseq3)
            (void) fprintf(primerr, "evseq3\n");
          else if (control[i].u.f_point == evclosure)
            (void) fprintf(primerr, "evclosure\n");
          else if (control[i].u.f_point == evclosure1)
            (void) fprintf(primerr, "evclosure1\n");
          else if (control[i].u.f_point == eval0)
            (void) fprintf(primerr, "eval0\n");
          else if (control[i].u.f_point == apply0)
            (void) fprintf(primerr, "apply0\n");
          else if (control[i].u.f_point == everr)
            (void) fprintf(primerr, "everr\n");
          else if (control[i].u.f_point == lookup)
            (void) fprintf(primerr, "lookup\n");
          else
            fprintf (stderr, "Unknown control stack element\n");
        }
    }
  return C_NIL;
}

public void init_ev()
{
  mkprim(PN_E,         eval,      1, FSUBR);
  mkprim(PN_EVAL,      eval,      1, SUBR);
  mkprim(PN_APPLY,     apply,     2, SUBR);
  mkprim(PN_APPLYSTAR, apply,    -2, SUBR);
  mkprim(PN_BAKTRACE,  baktrace,  0, SUBR);
}

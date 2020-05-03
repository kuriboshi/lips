/*
 * Lips, lisp shell.
 * Copyright 1988, 2020 Krister Joas
 *
 */

#include "libisp.hh"
#include "except.hh"

// extern lisp::LISPT findalias(lisp::LISPT);
// extern void pputc(int, FILE*);
// extern bool brkflg;
// extern bool interrupt;

namespace lisp
{
void evaluator::reset()
{
  a().dzero();
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
    if(control[i].type == CTRL_FUNC && control[i].u.f_point == &evaluator::evlam0)
      for(; i; i--)
      {
        if(control[i].type == CTRL_FUNC && control[i].u.f_point == &evaluator::ev0 && control[i - 1].type == CTRL_LISP
          && (type_of(control[i - 1].u.lisp) == CONS && type_of(control[i - 1].u.lisp->car()) != CONS))
        {
          foo = control[i - 1].u.lisp;
          _lisp.primerr().printf(" [in ");
          file(_lisp).prin2(foo->car(), C_T);
          _lisp.primerr().putch(']');
          goto out;
        }
      }
  }
out:
  _lisp.primerr().putch('\n');
  return foo;
}

/*
 * Print errormessage, abort current evaluation, and
 * return to top level.
 */
void evaluator::abort(int m, LISPT v)
{
  _lisp.error(m, v);
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
    _lisp.perror(mess, fault);
    printwhere();
  }
  if(breakhook != nullptr)
    (*breakhook)();
  if(env == nullptr)
    throw lisp_error("break");
  file(_lisp).xprint(a().cons(fault, a().cons(C_BROKEN, C_NIL)), C_T);
  push_func(next);
  cont = &evaluator::everr;
}

/* 
 * mkdestblock - creates a new destination block of size
 *               's' and initializes it.
 */
alloc::destblock_t* evaluator::mkdestblock(int s)
{
  auto dest = a().dalloc(s + 1);
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
  a().dfree(env);
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
      foo = (*(fun->subrval().function0))(_lisp);
      break;
    case 1:
    case -1:
      foo = (*(fun->subrval().function1))(_lisp, dest[1].val.d_lisp);
      break;
    case 2:
    case -2:
      foo = (*(fun->subrval().function2))(_lisp, dest[2].val.d_lisp, dest[1].val.d_lisp);
      break;
    case 3:
    case -3:
      foo = (*(fun->subrval().function3))(_lisp, dest[3].val.d_lisp, dest[2].val.d_lisp, dest[1].val.d_lisp);
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
  push_func(&evaluator::eval0);
  cont = &evaluator::peval;
  while(!(this->*cont)())
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
  a().dfree(dest);
  return true;
}

/*
Dummy definition for the pretty printer.
PRIMITIVE apply(f, a)
*/
PRIMITIVE evaluator::apply(LISPT f, LISPT x)
{
  push_point(dest);
  dest = mkdestblock(1);
  push_lisp(fun);
  fun = f;
  push_lisp(args);
  args = x;
  expression = a().cons(f, x);
  push_func(&evaluator::apply0);
  cont = &evaluator::peval2;
  while(!(this->*cont)())
    ;
  LISPT foo = receive();
  dest = pop_point();
  return foo;
}

bool evaluator::apply0()
{
  a().dfree(dest);
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
  if(_trace)
    file(_lisp).xprint(expression, C_T);
#endif
  push_lisp(expression);
  push_func(&evaluator::ev0);
  switch(type_of(expression))
  {
    case CONS:
      push_lisp(fun);
      fun = expression->car();
      push_lisp(args);
      args = expression->cdr();
      push_func(&evaluator::ev1);
      cont = &evaluator::peval1;
      break;
    case SYMBOL:
      cont = &evaluator::lookup;
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
  LISPT al = getprop(_lisp, expression->car(), C_AUTOLOAD);
  if(!is_NIL(al))
  {
    push_lisp(expression);
    push_point(dest);
    file(_lisp).load(al);
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
#if 0
    // TODO:
    expression = findalias(expression);
#endif
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
#if 0
  // TODO:
  expression = findalias(expression);
#endif
  if(type_of(expression) == CONS && type_of(expression->car()) == SYMBOL
    && type_of(expression->car()->symvalue()) == UNBOUND)
  {
    if(!evalhook(expression))
      xbreak(UNDEF_FUNCTION, expression->car(), continuation);
    return true;
  }
  return false;
}

bool evaluator::peval1()
{
  int foo;

  if(brkflg)
    xbreak(KBD_BREAK, fun, &evaluator::peval1);
  else if(interrupt)
    abort(NO_MESSAGE, C_NIL);
  else
    switch(type_of(fun))
    {
      case CLOSURE:
        push_func(&evaluator::peval1);
        cont = &evaluator::evclosure;
        break;
      case SUBR:
        push_point(dest);
        push_func(&evaluator::ev2);
        if((foo = fun->subrval().argcount) < 0)
        {
          dest = mkdestblock(-foo);
          push_func(&evaluator::noevarg);
          cont = &evaluator::evlis;
        }
        else
        {
          dest = mkdestblock(foo);
          noeval = 0;
          cont = &evaluator::evalargs;
        }
        break;
      case FSUBR:
        push_point(dest);
        push_func(&evaluator::ev2);
        if((foo = fun->subrval().argcount) < 0)
        {
          dest = mkdestblock(-foo);
          cont = &evaluator::spread;
        }
        else
        {
          dest = mkdestblock(foo);
          noeval = 1;
          cont = &evaluator::evalargs;
        }
        break;
      case LAMBDA:
        noeval = 0;
        cont = &evaluator::evlam;
        break;
      case NLAMBDA:
        noeval = 1;
        cont = &evaluator::evlam;
        break;
      case CONS:
      case INDIRECT:
        expression = fun;
        push_func(&evaluator::ev3);
        cont = &evaluator::peval;
        break;
      case SYMBOL:
        fun = fun->symvalue();
        cont = &evaluator::peval1;
        break;
      case UNBOUND:
        do_unbound(&evaluator::peval1);
        break;
      case STRING:
        if(!evalhook(expression))
          xbreak(ILLEGAL_FUNCTION, fun, &evaluator::peval1);
        break;
      default:
        if(!do_default(&evaluator::peval1))
          xbreak(ILLEGAL_FUNCTION, fun, &evaluator::peval1);
        break;
    }
  return false;
}

bool evaluator::peval2()
{
  int foo;

  if(brkflg)
    xbreak(KBD_BREAK, fun, &evaluator::peval2);
  else
    switch(type_of(fun))
    {
      case CLOSURE:
        push_func(&evaluator::peval2);
        cont = &evaluator::evclosure;
        break;
      case SUBR:
      case FSUBR:
        push_point(dest);
        push_func(&evaluator::ev2);
        if((foo = fun->subrval().argcount) < 0)
        {
          dest = mkdestblock(-foo);
          cont = &evaluator::spread;
        }
        else
        {
          dest = mkdestblock(foo);
          noeval = 1;
          cont = &evaluator::evalargs;
        }
        break;
      case LAMBDA:
      case NLAMBDA:
        noeval = 1;
        cont = &evaluator::evlam;
        break;
      case CONS:
      case INDIRECT:
        expression = fun;
        push_func(&evaluator::ev3p);
        cont = &evaluator::peval;
        break;
      case SYMBOL:
        fun = fun->symvalue();
        cont = &evaluator::peval2;
        break;
      case UNBOUND:
        do_unbound(&evaluator::peval2);
        break;
      case STRING:
        if(!evalhook(expression))
          xbreak(ILLEGAL_FUNCTION, fun, &evaluator::peval2);
        break;
      default:
        if(!do_default(&evaluator::peval2))
          xbreak(ILLEGAL_FUNCTION, fun, &evaluator::peval2);
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
  int op = _lisp.printlevel;
  _lisp.printlevel = 2;
  for(int i = toctrl - 1; i; i--)
  {
    if(control[i].type == CTRL_FUNC && control[i].u.f_point == &evaluator::ev0)
      file(_lisp).xprint(control[i - 1].u.lisp, C_T);
  }
  _lisp.printlevel = op;
}

bool evaluator::everr()
{
  // expression = break0(expression);
  cont = pop_func(); /* Discard one continuation. */
  cont = pop_func();
  return false;
}

bool evaluator::noevarg()
{
  args = receive();
  cont = &evaluator::spread;
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
      cont = &evaluator::noev9;
    else
      cont = &evaluator::ev9;
  }
  return false;
}

bool evaluator::ev9()
{
  if(is_NIL(args->cdr()))
  {
    cont = &evaluator::peval;
  }
  else
  {
    push_func(&evaluator::ev11);
    cont = &evaluator::peval;
  }
  return false;
}

bool evaluator::ev11()
{
  next();
  args = args->cdr();
  expression = args->car();
  cont = &evaluator::ev9;
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
    cont = &evaluator::evlis1;
  }
  return false;
}

bool evaluator::evlis1()
{
  if(is_NIL(args->cdr()))
  {
    push_func(&evaluator::evlis2);
    cont = &evaluator::peval;
  }
  else
  {
    push_func(&evaluator::evlis3);
    cont = &evaluator::peval;
  }
  return false;
}

bool evaluator::evlis2()
{
  LISPT x = a().cons(receive(), C_NIL);
  send(x);
  cont = pop_func();
  return false;
}

bool evaluator::evlis3()
{
  push_point(dest);
  dest = mkdestblock(1);
  push_func(&evaluator::evlis4);
  args = args->cdr();
  expression = args->car();
  cont = &evaluator::evlis1;
  return false;
}

bool evaluator::evlis4()
{
  LISPT x = receive();
  a().dfree(dest);
  dest = pop_point();
  x = a().cons(receive(), x);
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
  push_func(&evaluator::evlam1);
  if(spr)
  {
    if(noeval)
      cont = &evaluator::spread;
    else
    {
      push_func(&evaluator::noevarg);
      cont = &evaluator::evlis;
    }
  }
  else
    cont = &evaluator::evalargs;
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
    a().dfree(dest);
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
      xbreak(0, C_NIL, &evaluator::peval1);
    else
      xbreak(0, foo->car(), &evaluator::peval1); /* CAR(_) broken */
  }
  return false;
}

bool evaluator::ev3()
{
  fun = receive();
  push_func(&evaluator::ev4);
  cont = &evaluator::peval1;
  return false;
}

bool evaluator::ev3p()
{
  fun = receive();
  push_func(&evaluator::ev4);
  cont = &evaluator::peval2;
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
  push_func(&evaluator::evlam0);
  cont = &evaluator::evsequence;
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
      xbreak(UNBOUND_VARIABLE, expression, &evaluator::lookup);
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
  push_func(&evaluator::evclosure1);
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
    cont = &evaluator::evseq1;
  }
  return false;
}

bool evaluator::evseq1()
{
  if(EQ(args->cdr(), C_NIL))
  {
    cont = &evaluator::peval;
  }
  else
  {
    push_func(&evaluator::evseq3);
    cont = &evaluator::peval;
  }
  return false;
}

bool evaluator::evseq3()
{
  args = args->cdr();
  expression = args->car();
  cont = &evaluator::evseq1;
  return false;
}

PRIMITIVE evaluator::baktrace()
{
  for(int i = toctrl; i >= 0; i--)
  {
    _lisp.primerr().printf("%d: ", i);
    switch(control[i].type)
    {
      case CTRL_LISP:
        file(_lisp).xprint(control[i].u.lisp, C_T);
        break;
      case CTRL_POINT:
        _lisp.primerr().printf("destblock\n");
        break;
      case CTRL_FUNC:
        if(control[i].u.f_point == &evaluator::ev0)
          _lisp.primerr().printf("ev0\n");
        else if(control[i].u.f_point == &evaluator::peval)
          _lisp.primerr().printf("peval\n");
        else if(control[i].u.f_point == &evaluator::peval1)
          _lisp.primerr().printf("peval1\n");
        else if(control[i].u.f_point == &evaluator::peval2)
          _lisp.primerr().printf("peval2\n");
        else if(control[i].u.f_point == &evaluator::ev0)
          _lisp.primerr().printf("ev0\n");
        else if(control[i].u.f_point == &evaluator::ev1)
          _lisp.primerr().printf("ev1\n");
        else if(control[i].u.f_point == &evaluator::ev2)
          _lisp.primerr().printf("ev2\n");
        else if(control[i].u.f_point == &evaluator::ev3)
          _lisp.primerr().printf("ev3\n");
        else if(control[i].u.f_point == &evaluator::ev4)
          _lisp.primerr().printf("ev4\n");
        else if(control[i].u.f_point == &evaluator::evlam0)
          _lisp.primerr().printf("evlam0\n");
        else if(control[i].u.f_point == &evaluator::evlam1)
          _lisp.primerr().printf("evlam1\n");
        else if(control[i].u.f_point == &evaluator::ev9)
          _lisp.primerr().printf("ev9\n");
        else if(control[i].u.f_point == &evaluator::ev11)
          _lisp.primerr().printf("ev11\n");
        else if(control[i].u.f_point == &evaluator::ev3p)
          _lisp.primerr().printf("ev3p\n");
        else if(control[i].u.f_point == &evaluator::evalargs)
          _lisp.primerr().printf("evalargs\n");
        else if(control[i].u.f_point == &evaluator::noevarg)
          _lisp.primerr().printf("noevarg\n");
        else if(control[i].u.f_point == &evaluator::evlam)
          _lisp.primerr().printf("evlam\n");
        else if(control[i].u.f_point == &evaluator::spread)
          _lisp.primerr().printf("spread\n");
        else if(control[i].u.f_point == &evaluator::evlis)
          _lisp.primerr().printf("evlis\n");
        else if(control[i].u.f_point == &evaluator::evlis1)
          _lisp.primerr().printf("evlis1\n");
        else if(control[i].u.f_point == &evaluator::evlis2)
          _lisp.primerr().printf("evlis2\n");
        else if(control[i].u.f_point == &evaluator::evlis3)
          _lisp.primerr().printf("evlis3\n");
        else if(control[i].u.f_point == &evaluator::evlis4)
          _lisp.primerr().printf("evlis4\n");
        else if(control[i].u.f_point == &evaluator::noev9)
          _lisp.primerr().printf("noev9\n");
        else if(control[i].u.f_point == &evaluator::evsequence)
          _lisp.primerr().printf("evsequence\n");
        else if(control[i].u.f_point == &evaluator::evseq1)
          _lisp.primerr().printf("evseq1\n");
        else if(control[i].u.f_point == &evaluator::evseq3)
          _lisp.primerr().printf("evseq3\n");
        else if(control[i].u.f_point == &evaluator::evclosure)
          _lisp.primerr().printf("evclosure\n");
        else if(control[i].u.f_point == &evaluator::evclosure1)
          _lisp.primerr().printf("evclosure1\n");
        else if(control[i].u.f_point == &evaluator::eval0)
          _lisp.primerr().printf("eval0\n");
        else if(control[i].u.f_point == &evaluator::apply0)
          _lisp.primerr().printf("apply0\n");
        else if(control[i].u.f_point == &evaluator::everr)
          _lisp.primerr().printf("everr\n");
        else if(control[i].u.f_point == &evaluator::lookup)
          _lisp.primerr().printf("lookup\n");
        else
          _lisp.stderr().printf("Unknown control stack element\n");
        break;
    }
  }
  return C_NIL;
}

evaluator::evaluator(lisp& lisp) : base(lisp)
{
  add_mark_object(&fun);
  add_mark_object(&expression);
  add_mark_object(&args);
  mkprim(PN_E, ::lisp::eval, 1, FSUBR);
  mkprim(PN_EVAL, ::lisp::eval, 1, SUBR);
  mkprim(PN_APPLY, ::lisp::apply, 2, SUBR);
  mkprim(PN_APPLYSTAR, ::lisp::apply, -2, SUBR);
  mkprim(PN_BAKTRACE, ::lisp::baktrace, 0, SUBR);
}

bool evaluator::brkflg = false;
bool evaluator::interrupt = false;

} // namespace lisp

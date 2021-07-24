/*
 * Lips, lisp shell.
 * Copyright 1988, 2020 Krister Joas
 *
 */

#include "eval.hh"
#include "alloc.hh"
#include "except.hh"
#include "file.hh"
#include "prop.hh"

// extern lisp::LISPT findalias(lisp::LISPT);

using namespace std::literals;

namespace lisp
{
evaluator::evaluator(lisp& lisp): l(lisp), a(lisp.a())
{
  init();
}

void evaluator::reset()
{
  a.dzero();
  toctrl = 0;
  fun = NIL;
  args = NIL;
  env = nullptr;
}

LISPT evaluator::printwhere()
{
  LISPT foo = NIL;
  int i = toctrl - 1;
  for(; i != 0; --i) // Find latest completed call
  {
    if(auto* cont = std::get_if<continuation_t>(&control[i]); cont && *cont == &evaluator::evlam0)
      break;
  }
  for(; i != 0; --i)
  {
    if(auto* func = std::get_if<continuation_t>(&control[i]); func && *func == &evaluator::ev0)
    {
      if(auto* lsp = std::get_if<LISPT>(&control[i - 1]);
        lsp && (type_of(*lsp) == type::CONS && type_of((*lsp)->car()) != type::CONS))
      {
        foo = *lsp;
        l.primerr().format(" [in ");
        file(l).prin2(foo->car(), T);
        l.primerr().putch(']');
        break;
      }
    }
  }
  l.primerr().putch('\n');
  return foo;
}

/*
 * Print errormessage, abort current evaluation, and
 * return to top level.
 */
void evaluator::abort(int m, LISPT v)
{
  l.error(m, v);
  printwhere();
  unwind();
  throw lisp_error("abort");
}

void evaluator::overflow() { abort(STACK_OVERFLOW, NIL); }

/* 
 * These macros handles the control stack.  The control stack stores
 * continuations, destinations, and LISPT objects.  There are two macros 
 * to push and to pop pointers and LISPT objects.
 */
void evaluator::push_lisp(LISPT a)
{
  control[toctrl++] = a;
  if(toctrl >= CTRLBLKSIZE)
    overflow();
}

void evaluator::push_point(destblock_t* d)
{
  control[toctrl++] = d;
  if(toctrl >= CTRLBLKSIZE)
    overflow();
}

void evaluator::push_func(continuation_t f)
{
  control[toctrl++] = f;
  if(toctrl >= CTRLBLKSIZE)
    overflow();
}

LISPT evaluator::pop_lisp() { return std::get<LISPT>(control[--toctrl]); }

destblock_t* evaluator::pop_point() { return std::get<destblock_t*>(control[--toctrl]); }

evaluator::continuation_t evaluator::pop_func() { return std::get<evaluator::continuation_t>(control[--toctrl]); }

/*
 * This function prints an error message, and sets up a call
 * to everr that handles breaks.
 */
void evaluator::xbreak(int mess, LISPT fault, continuation_t next)
{
  if(mess != 0)
  {
    l.perror(mess, fault);
    printwhere();
  }
  if(_breakhook)
    _breakhook();
  if(env == nullptr)
    throw lisp_reset();
  file(l).print(cons(l, fault, cons(l, C_BROKEN, NIL)), T);
  push_func(next);
  cont = &evaluator::everr;
}

/* 
 * mkdestblock - creates a new destination block of size
 *               's' and initializes it.
 */
destblock_t* evaluator::mkdestblock(int s)
{
  return a.dalloc(s);
}

void evaluator::storevar(LISPT v, int i)
{
  dest[i].var(v);
}

destblock_t* evaluator::pop_env()
{
  a.dfree(env);
  return pop_point();
}

void evaluator::send(LISPT a)
{
  if(dest[0].index() > 0)
    dest[dest[0].index()].val(a);
}

LISPT evaluator::receive()
{
  return dest[dest[0].index()].val();
}

void evaluator::next()
{
  dest[0].decr();
}

/* 
 * Make a call to the function in parameter `fun'.  It can handle
 * functions with up to three arguments.
 */
LISPT evaluator::call(LISPT fun)
{
  switch(fun->subrval().argcount())
  {
    case 0:
      return fun->subrval()(l);
    case 1:
      return fun->subrval()(l, dest[1].val());
    case 2:
      return fun->subrval()(l, dest[2].val(), dest[1].val());
    case 3:
      return fun->subrval()(l, dest[3].val(), dest[2].val(), dest[1].val());
  }
  return NIL;
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
  a.dfree(dest);
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
  expression = cons(l, f, x);
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
  a.dfree(dest);
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
    file(l).print(expression, T);
#endif
  push_lisp(expression);
  push_func(&evaluator::ev0);
  switch(type_of(expression))
  {
    case type::CONS:
      push_lisp(fun);
      fun = expression->car();
      push_lisp(args);
      args = expression->cdr();
      push_func(&evaluator::ev1);
      cont = &evaluator::peval1;
      break;
    case type::SYMBOL:
      cont = &evaluator::lookup;
      break;
    case type::INDIRECT:
      send(expression->indirectval());
      cont = pop_func();
      break;
    case type::CVARIABLE:
      send(expression->cvarval());
      cont = pop_func();
      break;
    case type::FREE:
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

  if(_undefhook)
    switch(_undefhook(exp, &res))
    {
      case 1:
        send(res);
        cont = pop_func();
        return true;
        break;
      case -1:
        abort(NO_MESSAGE, NIL);
        break;
      default:
        return false;
        break;
    }
  return false;
}

void evaluator::do_unbound(continuation_t continuation)
{
  /* 
   * If an undefined symbol has the AUTOLOAD property, we try to
   * load the definition from a file.  If that doesn't succeed, then
   * the symbol is undefined.
   */
  LISPT al = getprop(l, expression->car(), C_AUTOLOAD);
  if(!is_NIL(al))
  {
    push_lisp(expression);
    push_point(dest);
    file(l).load(al);
    dest = pop_point();
    expression = pop_lisp();
    fun = expression->car()->symvalue();
    if(type_of(fun) == type::UNBOUND)
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
    if(type_of(expression) == type::CONS && type_of(expression->car()) == type::SYMBOL
      && type_of(expression->car()->symvalue()) == type::UNBOUND)
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
  if(type_of(expression) == type::CONS && type_of(expression->car()) == type::SYMBOL
    && type_of(expression->car()->symvalue()) == type::UNBOUND)
  {
    if(!evalhook(expression))
      xbreak(UNDEF_FUNCTION, expression->car(), continuation);
    return true;
  }
  return false;
}

bool evaluator::peval1()
{
  if(l.brkflg)
    xbreak(KBD_BREAK, fun, &evaluator::peval1);
  else if(l.interrupt)
    abort(NO_MESSAGE, NIL);
  else
    switch(type_of(fun))
    {
      case type::CLOSURE:
        push_func(&evaluator::peval1);
        cont = &evaluator::evclosure;
        break;
      case type::SUBR:
      case type::FSUBR:
        push_point(dest);
        push_func(&evaluator::ev2);
        dest = mkdestblock(fun->subrval().argcount());
        noeval = fun->subrval().subr == subr_t::subr::NOEVAL;
        if(fun->subrval().spread == subr_t::spread::SPREAD)
        {
          if(!noeval)
          {
            push_func(&evaluator::noevarg);
            cont = &evaluator::evlis;
          }
          else
            cont = &evaluator::spread;
        }
        else
          cont = &evaluator::evalargs;
        break;
      case type::LAMBDA:
        noeval = false;
        cont = &evaluator::evlam;
        break;
      case type::NLAMBDA:
        noeval = true;
        cont = &evaluator::evlam;
        break;
      case type::CONS:
      case type::INDIRECT:
        expression = fun;
        push_func(&evaluator::ev3);
        cont = &evaluator::peval;
        break;
      case type::SYMBOL:
        fun = fun->symvalue();
        cont = &evaluator::peval1;
        break;
      case type::UNBOUND:
        do_unbound(&evaluator::peval1);
        break;
      case type::STRING:
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
  if(l.brkflg)
    xbreak(KBD_BREAK, fun, &evaluator::peval2);
  else
    switch(type_of(fun))
    {
      case type::CLOSURE:
        push_func(&evaluator::peval2);
        cont = &evaluator::evclosure;
        break;
      case type::SUBR:
      case type::FSUBR:
        push_point(dest);
        push_func(&evaluator::ev2);
        noeval = fun->subrval().subr == subr_t::subr::NOEVAL;
        dest = mkdestblock(fun->subrval().argcount());
        if(fun->subrval().spread == subr_t::spread::SPREAD)
          cont = &evaluator::spread;
        else
          cont = &evaluator::evalargs;
        break;
      case type::LAMBDA:
      case type::NLAMBDA:
        noeval = true;
        cont = &evaluator::evlam;
        break;
      case type::CONS:
      case type::INDIRECT:
        expression = fun;
        push_func(&evaluator::ev3p);
        cont = &evaluator::peval;
        break;
      case type::SYMBOL:
        fun = fun->symvalue();
        cont = &evaluator::peval2;
        break;
      case type::UNBOUND:
        do_unbound(&evaluator::peval2);
        break;
      case type::STRING:
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
  int op = l.printlevel;
  l.printlevel = 2;
  for(int i = toctrl - 1; i; i--)
  {
    if(auto* cont = std::get_if<continuation_t>(&control[i]); cont && *cont == &evaluator::ev0)
      file(l).print(std::get<LISPT>(control[i - 1]), T);
  }
  l.printlevel = op;
}

bool evaluator::everr()
{
  expression = break0(expression);
  cont = pop_func(); // Discard one continuation.
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
  while(true)
  {
    if(is_NIL(args->cdr()))
    {
      send(expression);
      cont = pop_func();
      break;
    }
    else
    {
      send(expression);
      next();
      args = args->cdr();
      expression = args->car();
    }
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
  LISPT x = cons(l, receive(), NIL);
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
  a.dfree(dest);
  dest = pop_point();
  x = cons(l, receive(), x);
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
  while(true)
  {
    if(EQ(args, NIL))
    {
      cont = pop_func();
      break;
    }
    else if(dest[0].index() == 1)
    {
      send(args);
      cont = pop_func();
      break;
    }
    else
    {
      send(args->car());
      next();
      args = args->cdr();
    }
  }
  return false;
}

bool evaluator::ev2()
{
  try
  {
    auto foo = call(fun);
    a.dfree(dest);
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
      xbreak(0, NIL, &evaluator::peval1);
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
  dest[0].link(env);
  env = dest;
  for(auto i = dest[0].size(); i > 0; i--)
  {
    LISPT t = dest[i].var()->symvalue();
    dest[i].var()->symvalue(dest[i].val());
    dest[i].val(t);
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
  for(auto i = c[0].size(); i > 0; i--)
    c[i].var()->symvalue(c[i].val());
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
    env = env->link();
  }
}

bool evaluator::lookup()
{
  LISPT t = expression->symvalue();
  switch(type_of(t))
  {
    case type::UNBOUND:
      xbreak(UNBOUND_VARIABLE, expression, &evaluator::lookup);
      return false;
      break;
    case type::INDIRECT:
      send(t->indirectval());
      break;
    case type::CVARIABLE:
      send(t->cvarval());
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
  if(EQ(args, NIL))
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
  if(EQ(args->cdr(), NIL))
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
    l.primerr().format("{}: ", i);
    std::visit(
      [this](auto&& arg) {
        using ArgType = std::decay_t<decltype(arg)>;
        if constexpr(std::is_same_v<ArgType, LISPT>)
        {
          file(l).print(arg, T);
        }
        else if constexpr(std::is_same_v<ArgType, destblock_t*>)
        {
          l.primerr().format("destblock\n");
        }
        else if constexpr(std::is_same_v<ArgType, continuation_t>)
        {
          if(arg == &evaluator::ev0)
            l.primerr().format("ev0\n");
          else if(arg == &evaluator::peval)
            l.primerr().format("peval\n");
          else if(arg == &evaluator::peval1)
            l.primerr().format("peval1\n");
          else if(arg == &evaluator::peval2)
            l.primerr().format("peval2\n");
          else if(arg == &evaluator::ev0)
            l.primerr().format("ev0\n");
          else if(arg == &evaluator::ev1)
            l.primerr().format("ev1\n");
          else if(arg == &evaluator::ev2)
            l.primerr().format("ev2\n");
          else if(arg == &evaluator::ev3)
            l.primerr().format("ev3\n");
          else if(arg == &evaluator::ev4)
            l.primerr().format("ev4\n");
          else if(arg == &evaluator::evlam0)
            l.primerr().format("evlam0\n");
          else if(arg == &evaluator::evlam1)
            l.primerr().format("evlam1\n");
          else if(arg == &evaluator::ev9)
            l.primerr().format("ev9\n");
          else if(arg == &evaluator::ev11)
            l.primerr().format("ev11\n");
          else if(arg == &evaluator::ev3p)
            l.primerr().format("ev3p\n");
          else if(arg == &evaluator::evalargs)
            l.primerr().format("evalargs\n");
          else if(arg == &evaluator::noevarg)
            l.primerr().format("noevarg\n");
          else if(arg == &evaluator::evlam)
            l.primerr().format("evlam\n");
          else if(arg == &evaluator::spread)
            l.primerr().format("spread\n");
          else if(arg == &evaluator::evlis)
            l.primerr().format("evlis\n");
          else if(arg == &evaluator::evlis1)
            l.primerr().format("evlis1\n");
          else if(arg == &evaluator::evlis2)
            l.primerr().format("evlis2\n");
          else if(arg == &evaluator::evlis3)
            l.primerr().format("evlis3\n");
          else if(arg == &evaluator::evlis4)
            l.primerr().format("evlis4\n");
          else if(arg == &evaluator::noev9)
            l.primerr().format("noev9\n");
          else if(arg == &evaluator::evsequence)
            l.primerr().format("evsequence\n");
          else if(arg == &evaluator::evseq1)
            l.primerr().format("evseq1\n");
          else if(arg == &evaluator::evseq3)
            l.primerr().format("evseq3\n");
          else if(arg == &evaluator::evclosure)
            l.primerr().format("evclosure\n");
          else if(arg == &evaluator::evclosure1)
            l.primerr().format("evclosure1\n");
          else if(arg == &evaluator::eval0)
            l.primerr().format("eval0\n");
          else if(arg == &evaluator::apply0)
            l.primerr().format("apply0\n");
          else if(arg == &evaluator::everr)
            l.primerr().format("everr\n");
          else if(arg == &evaluator::lookup)
            l.primerr().format("lookup\n");
          else
            l.stderr().format("Unknown control stack element\n");
        }
        else
          ; // Do nothing for monostate
      },
      control[i]);
  }
  return NIL;
}

PRIMITIVE evaluator::topofstack()
{
  auto x = a.getobject();
  x->settype(type::ENVIRON);
  x->envval(l.e().environment());
  return x;
}

PRIMITIVE evaluator::envget(LISPT e, LISPT n)
{
  LISPT foo = nullptr;
#if 0
  l.check(e, type::ENVIRON);
  l.check(n, type::INTEGER);
  if(n->intval() <= 0)
    foo = cons(l, e->envval()->car(), mknumber(l, e->envval()->cdr()));
  else
    if(n->intval() <= e->cdr()->intval())
      foo = cons(l, e->envval()->car() + n->intval(),
        e->envval()->cdr() + n->intval());
    else
      foo = NIL;
#endif
  return foo;
}

namespace pn
{
inline constexpr auto E = "e";                   // noeval version of eval
inline constexpr auto EVAL = "eval";             // evaluate exp
inline constexpr auto APPLY = "apply";           // apply function on args
inline constexpr auto APPLYSTAR = "apply*";      // apply nospread
inline constexpr auto BAKTRACE = "baktrace";     // control stack backtrace
inline constexpr auto TOPOFSTACK = "topofstack"; // return top of value stack
inline constexpr auto ENVGET = "envget";         // examine environment
} // namespace pn

void evaluator::init()
{
  // clang-format off
  mkprim(pn::E,          ::lisp::eval,       subr_t::subr::NOEVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::EVAL,       ::lisp::eval,       subr_t::subr::EVAL,   subr_t::spread::NOSPREAD);
  mkprim(pn::APPLY,      ::lisp::apply,      subr_t::subr::EVAL,   subr_t::spread::NOSPREAD);
  mkprim(pn::APPLYSTAR,  ::lisp::apply,      subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  mkprim(pn::BAKTRACE,   ::lisp::baktrace,   subr_t::subr::EVAL,   subr_t::spread::NOSPREAD);
  mkprim(pn::TOPOFSTACK, ::lisp::topofstack, subr_t::subr::EVAL,   subr_t::spread::NOSPREAD);
  mkprim(pn::ENVGET,     ::lisp::envget,     subr_t::subr::EVAL,   subr_t::spread::NOSPREAD);
  // clang-format on
}

} // namespace lisp

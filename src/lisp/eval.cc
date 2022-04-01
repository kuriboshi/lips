//
// Lips, lisp shell.
// Copyright 1988, 2020-2022 Krister Joas
//

#include "eval.hh"
#include "alloc.hh"
#include "except.hh"
#include "file.hh"
#include "prim.hh"
#include "prop.hh"

// extern lisp::LISPT findalias(lisp::LISPT);

using namespace std::literals;

namespace lisp
{
evaluator::evaluator(lisp& lisp): l(lisp), a(lisp.a()) {}

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
        file::prin2(l, foo->car(), T);
        l.primerr().putch(']');
        break;
      }
    }
  }
  l.primerr().putch('\n');
  return foo;
}

/// @brief Print error message, abort current evaluation, and return to top
/// level.
void evaluator::abort(int m, LISPT v)
{
  l.error(m, v);
  printwhere();
  unwind();
  throw lisp_error("abort");
}

void evaluator::overflow() { abort(STACK_OVERFLOW, NIL); }

/// @brief This function prints an error message, and sets up a call to everr
/// that handles breaks.
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
  file::print(l, cons(l, fault, cons(l, C_BROKEN, NIL)), T);
  push(next);
  cont = &evaluator::everr;
}

/// @brief Creates a new destination block of size 's' and initializes it.
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
  return pop<destblock_t*>();
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

/// @brief Make a call to the function in parameter `fun'.
///
/// @details It can handle functions with up to three arguments.
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

/// @brief This is the LISP evaluator.
///
/// @details The evaluator allocates a destination slot for the result and
/// starts munching continuations.
LISPT evaluator::eval(LISPT expr)
{
  //
  // Set the current expression to `expr' and push the current destination onto
  // the control stack.
  //
  expression = expr;
  push(dest);
  //
  // The result of evalutating `expr' is stored in the destination, which is
  // retrieved with the receive function.
  //
  dest = mkdestblock(1);
  //
  // This how it works in general: Push the function to be called last, and set
  // the continuation variable `cont' to `peval'. `peval' may push more
  // contiuations onto the stack but eventaully `eval0' is called which returns
  // true, signalling end of evaluation.
  //
  push(&evaluator::eval0);
  cont = &evaluator::peval;
  while(!(this->*cont)())
    ;
  //
  // Retrieve the result of the evaluation and restore the previous
  // destination.
  //
  LISPT foo = receive();
  dest = pop<destblock_t*>();
  // Return the result.
  return foo;
}

bool evaluator::eval0()
{
  a.dfree(dest);
  // Signal the end of the evaluation.
  return true;
}

LISPT evaluator::apply(LISPT f, LISPT x)
{
  push(dest);
  dest = mkdestblock(1);
  push(fun);
  fun = f;
  push(args);
  args = x;
  expression = cons(l, f, x);
  push(&evaluator::apply0);
  cont = &evaluator::peval2;
  while(!(this->*cont)())
    ;
  LISPT foo = receive();
  dest = pop<destblock_t*>();
  return foo;
}

bool evaluator::apply0()
{
  a.dfree(dest);
  args = pop<LISPT>();
  fun = pop<LISPT>();
  return true;
}

bool evaluator::ev0()
{
  //
  // Discard the top of stack (it's the previous expression, see comment in
  // `eval' above) and restore continuation.  The function `ev0' is also used
  // as a placeholder for the beginning of an eval.
  //
  toctrl -= 1;
  cont = pop<continuation_t>();
  return false;
}

bool evaluator::peval()
{
#ifdef TRACE
  if(_trace)
    file::print(l, expression, T);
#endif
  push(expression);
  push(&evaluator::ev0);
  switch(type_of(expression))
  {
    case type::CONS:
      push(fun);
      fun = expression->car();
      push(args);
      args = expression->cdr();
      push(&evaluator::ev1);
      cont = &evaluator::peval1;
      break;
    case type::SYMBOL:
      cont = &evaluator::lookup;
      break;
    case type::INDIRECT:
      send(expression->indirectval());
      cont = pop<continuation_t>();
      break;
    case type::CVARIABLE:
      send(expression->cvarval());
      cont = pop<continuation_t>();
      break;
    case type::FREE:
      abort(CORRUPT_DATA, expression);
      break;
    default:
      send(expression);
      cont = pop<continuation_t>();
      break;
  }
  return false;
}

bool evaluator::ev1()
{
  args = pop<LISPT>();
  fun = pop<LISPT>();
  cont = pop<continuation_t>();
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
        cont = pop<continuation_t>();
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
  //
  // If an undefined symbol has the AUTOLOAD property, we try to load the
  // definition from a file. If that doesn't succeed, then the symbol is
  // undefined.
  //
  LISPT al = getprop(l, expression->car(), C_AUTOLOAD);
  if(!is_NIL(al))
  {
    push(expression);
    push(dest);
    file::load(l, al);
    dest = pop<destblock_t*>();
    expression = pop<LISPT>();
    fun = expression->car()->value();
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
      && type_of(expression->car()->value()) == type::UNBOUND)
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
    && type_of(expression->car()->value()) == type::UNBOUND)
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
        push(&evaluator::peval1);
        cont = &evaluator::evclosure;
        break;
      case type::SUBR:
      case type::FSUBR:
        push(dest);
        push(&evaluator::ev2);
        dest = mkdestblock(fun->subrval().argcount());
        noeval = fun->subrval().subr == subr_t::subr::NOEVAL;
        if(fun->subrval().spread == subr_t::spread::NOSPREAD)
        {
          if(!noeval)
          {
            push(&evaluator::noevarg);
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
        push(&evaluator::ev3);
        cont = &evaluator::peval;
        break;
      case type::SYMBOL:
        fun = fun->value();
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
        push(&evaluator::peval2);
        cont = &evaluator::evclosure;
        break;
      case type::SUBR:
      case type::FSUBR:
        push(dest);
        push(&evaluator::ev2);
        noeval = fun->subrval().subr == subr_t::subr::NOEVAL;
        dest = mkdestblock(fun->subrval().argcount());
        if(fun->subrval().spread == subr_t::spread::NOSPREAD)
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
        push(&evaluator::ev3p);
        cont = &evaluator::peval;
        break;
      case type::SYMBOL:
        fun = fun->value();
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

/// @brief Prints a backtrace of all expressions on the stack.
void evaluator::bt()
{
  int op = l.printlevel;
  l.printlevel = 2;
  for(int i = toctrl - 1; i; i--)
  {
    if(auto* cont = std::get_if<continuation_t>(&control[i]); cont && *cont == &evaluator::ev0)
      file::print(l, std::get<LISPT>(control[i - 1]), T);
  }
  l.printlevel = op;
}

bool evaluator::everr()
{
  send(break0(expression));
  cont = pop<continuation_t>(); // Discard one continuation.
  cont = pop<continuation_t>();
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
    cont = pop<continuation_t>();
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
    push(&evaluator::ev11);
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
      cont = pop<continuation_t>();
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
    cont = pop<continuation_t>();
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
    push(&evaluator::evlis2);
    cont = &evaluator::peval;
  }
  else
  {
    push(&evaluator::evlis3);
    cont = &evaluator::peval;
  }
  return false;
}

bool evaluator::evlis2()
{
  LISPT x = cons(l, receive(), NIL);
  send(x);
  cont = pop<continuation_t>();
  return false;
}

bool evaluator::evlis3()
{
  push(dest);
  dest = mkdestblock(1);
  push(&evaluator::evlis4);
  args = args->cdr();
  expression = args->car();
  cont = &evaluator::evlis1;
  return false;
}

bool evaluator::evlis4()
{
  LISPT x = receive();
  a.dfree(dest);
  dest = pop<destblock_t*>();
  x = cons(l, receive(), x);
  send(x);
  cont = pop<continuation_t>();
  return false;
}

bool evaluator::evlam()
{
  push(expression);
  push(env);
  push(dest);
  int ac = 0;
  auto spr = false;
  if((ac = fun->lambda().count) < 0)
  {
    ac = -ac;
    spr = true;
  }
  dest = mkdestblock(ac);
  auto i = ac;
  for(auto foo = fun->lambda().args; i; foo = foo->cdr(), i--) storevar(foo->car(), i);
  push(&evaluator::evlam1);
  if(spr)
  {
    if(noeval)
      cont = &evaluator::spread;
    else
    {
      push(&evaluator::noevarg);
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
    if(is_NIL(args))
    {
      cont = pop<continuation_t>();
      break;
    }
    else if(dest[0].index() == 1)
    {
      send(args);
      cont = pop<continuation_t>();
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
    dest = pop<destblock_t*>();
    send(foo);
    cont = pop<continuation_t>();
  }
  catch(const lisp_reset&)
  {}
  catch(const lisp_error& ex)
  {
    if(!_interactive)
      throw;
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
  push(&evaluator::ev4);
  cont = &evaluator::peval1;
  return false;
}

bool evaluator::ev3p()
{
  fun = receive();
  push(&evaluator::ev4);
  cont = &evaluator::peval2;
  return false;
}

bool evaluator::ev4()
{
  cont = pop<continuation_t>();
  return false;
}

void evaluator::link()
{
  dest[0].link(env);
  env = dest;
  for(auto i = dest[0].size(); i > 0; i--)
  {
    LISPT t = dest[i].var()->value();
    dest[i].var()->value(dest[i].val());
    dest[i].val(t);
  }
}

bool evaluator::evlam1()
{
  link();
  dest = pop<destblock_t*>();
  args = fun->lambda().body;
  push(&evaluator::evlam0);
  cont = &evaluator::evsequence;
  return false;
}

void evaluator::restore_env()
{
  auto* c = env;
  for(auto i = c[0].size(); i > 0; i--)
    c[i].var()->value(c[i].val());
}

bool evaluator::evlam0()
{
  restore_env();
  env = pop_env();
  expression = pop<LISPT>();
  cont = pop<continuation_t>();
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
  LISPT t = expression->value();
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
  cont = pop<continuation_t>();
  return false;
}

bool evaluator::evclosure()
{
  LISPT foo;
  int i;

  push(env);
  push(dest);
  dest = mkdestblock(fun->closure().count);
  for(foo = fun->closure().closed, i = fun->closure().count; i; foo = foo->cdr(), i--) storevar(foo->car(), i);
  for(foo = fun->closure().cvalues; !is_NIL(foo); foo = foo->cdr())
  {
    send(foo->car());
    next();
  }
  fun = fun->closure().cfunction;
  link();
  dest = pop<destblock_t*>();
  auto envir = pop<destblock_t*>();
  cont = pop<continuation_t>();
  push(envir);
  push(&evaluator::evclosure1);
  return false;
}

bool evaluator::evclosure1()
{
  restore_env();
  env = pop_env();
  cont = pop<continuation_t>();
  return false;
}

bool evaluator::evsequence()
{
  if(is_NIL(args))
  {
    cont = pop<continuation_t>();
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
  if(is_NIL(args->cdr()))
  {
    cont = &evaluator::peval;
  }
  else
  {
    push(&evaluator::evseq3);
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

LISPT evaluator::destblock(lisp& l, const destblock_t* block)
{
  if(block == nullptr)
    return NIL;
  LISPT foo = tconc(l, NIL, mknumber(l, block->size()));
  for(int i = 0; i != block->size(); ++i)
  {
    foo = tconc(l, foo, cons(l, (block + i + 1)->var(), (block + i + 1)->val()));
  }
  return car(l, foo);
}

LISPT evaluator::baktrace()
{
  for(int i = toctrl; i >= 0; i--)
  {
    l.primerr().format("{}: ", i);
    std::visit(
      [this](auto&& arg) {
        using ArgType = std::decay_t<decltype(arg)>;
        if constexpr(std::is_same_v<ArgType, LISPT>)
          print(l, arg, T);
        else if constexpr(std::is_same_v<ArgType, destblock_t*>)
        {
          if(arg != nullptr)
          {
            prin1(l, "destblock_t: "_l, T);
            print(l, destblock(l, arg), T);
          }
          else
          {
            prin1(l, "destblock_t: nullptr"_l, T);
            terpri(l, T);
          }
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

LISPT evaluator::topofstack()
{
  auto x = a.getobject();
  x->settype(type::ENVIRON);
  x->set(l.e().environment());
  return x;
}

LISPT evaluator::destblock(LISPT e)
{
  check(e, type::ENVIRON);
  return destblock(l, e->envval());
}

} // namespace lisp

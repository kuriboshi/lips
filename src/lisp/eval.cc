//
// Lips, lisp shell.
// Copyright 1988, 2020-2022 Krister Joas
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//

#include "lisp/eval.hh"

using namespace std::literals;

namespace lisp
{
evaluator::evaluator(context& ctx)
  : _ctx(ctx)
{}

void evaluator::reset()
{
  dzero();
  _toctrl = 0;
  _fun = NIL;
  _args = NIL;
  _env = nullptr;
}

LISPT evaluator::printwhere()
{
  LISPT foo = NIL;
  int i = _toctrl - 1;
  for(; i != 0; --i) // Find latest completed call
  {
    if(auto* cont = std::get_if<continuation_t>(&_control[i]); (cont != nullptr) && *cont == &evaluator::evlam0)
      break;
  }
  for(; i != 0; --i)
  {
    if(auto* func = std::get_if<continuation_t>(&_control[i]); (func != nullptr) && *func == &evaluator::ev0)
    {
      if(auto* lsp = std::get_if<LISPT>(&_control[i - 1]);
         lsp != nullptr && (type_of(*lsp) == type::Cons && type_of((*lsp)->car()) != type::Cons))
      {
        foo = *lsp;
        _ctx.primerr()->format("[in ");
        details::file::prin2(_ctx, foo->car(), T);
        _ctx.primerr()->putch(']');
        break;
      }
    }
  }
  _ctx.primerr()->putch('\n');
  return foo;
}

/// @brief Print error message, abort current evaluation, and return to top
/// level.
void evaluator::abort(std::error_code error, LISPT v)
{
  _ctx.perror(error, v);
  printwhere();
  unwind();
  throw lisp_error("abort");
}

void evaluator::overflow() { abort(error_errc::stack_overflow, C_EMPTY); }

/// @brief This function prints an error message, and sets up a call to everr
/// that handles breaks.
void evaluator::xbreak(std::error_code code, LISPT fault, continuation_t next)
{
  if(code)
  {
    _ctx.perror(code, fault);
    printwhere();
  }
  if(_breakhook)
    _breakhook();
  if(_env == nullptr)
    throw lisp_reset();
  details::file::print(_ctx, cons(fault, cons(C_BROKEN, NIL)), T);
  push(next);
  _cont = &evaluator::everr;
}

/// @brief Creates a new destination block of size 's' and initializes it.
destblock_t* evaluator::mkdestblock(int s) { return dalloc(s); }

void evaluator::storevar(LISPT v, int i) { _dest[i].var(v); }

void evaluator::pop_env()
{
  dfree(_env);
  pop(_env);
}

void evaluator::send(LISPT a)
{
  if(_dest[0].index() > 0)
    _dest[_dest[0].index()].val(a);
}

LISPT evaluator::receive() { return _dest[_dest[0].index()].val(); }

void evaluator::next() { _dest[0].decr(); }

/// @brief Make a call to the function in parameter `fun'.
///
/// @details It can handle functions with up to three arguments.
LISPT evaluator::call(LISPT fun)
{
  switch(fun->subrval().argcount())
  {
    case 0:
      return fun->subrval()(_ctx);
    case 1:
      return fun->subrval()(_ctx, _dest[1].val());
    case 2:
      return fun->subrval()(_ctx, _dest[2].val(), _dest[1].val());
    case 3:
      return fun->subrval()(_ctx, _dest[3].val(), _dest[2].val(), _dest[1].val());
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
  _expression = expr;
  push(_dest);
  //
  // The result of evalutating `expr' is stored in the destination, which is
  // retrieved with the receive function.
  //
  _dest = mkdestblock(1);
  //
  // This how it works in general: Push the function to be called last, and set
  // the continuation variable `cont' to `peval'. `peval' may push more
  // contiuations onto the stack but eventaully `eval0' is called which returns
  // true, signalling end of evaluation.
  //
  push(&evaluator::eval0);
  _cont = &evaluator::peval;
  try
  {
    while(!(this->*_cont)())
      ;
  }
  catch(const lisp_finish& ex)
  {
    unwind();
    throw;
  }
  //
  // Retrieve the result of the evaluation and restore the previous
  // destination.
  //
  LISPT foo = receive();
  pop(_dest);
  // Return the result.
  return foo;
}

bool evaluator::eval0()
{
  dfree(_dest);
  // Signal the end of the evaluation.
  return true;
}

LISPT evaluator::apply(LISPT f, LISPT x)
{
  push(_dest);
  _dest = mkdestblock(1);
  push(_fun);
  _fun = f;
  push(_args);
  _args = x;
  _expression = cons(f, x);
  push(&evaluator::apply0);
  _cont = &evaluator::peval2;
  try
  {
    while(!(this->*_cont)())
      ;
  }
  catch(const lisp_finish& ex)
  {
    unwind();
    throw;
  }
  LISPT foo = receive();
  pop(_dest);
  return foo;
}

bool evaluator::apply0()
{
  dfree(_dest);
  pop(_args);
  pop(_fun);
  return true;
}

bool evaluator::ev0()
{
  //
  // Discard the top of stack (it's the previous expression, see comment in
  // `eval' above) and restore continuation.  The function `ev0' is also used
  // as a placeholder for the beginning of an eval.
  //
  _toctrl -= 1;
  pop(_cont);
  return false;
}

bool evaluator::peval()
{
#ifdef TRACE
  if(_trace)
    details::file::print(l, expression, T);
#endif
  push(_expression);
  push(&evaluator::ev0);
  switch(type_of(_expression))
  {
    case type::Cons:
      push(_fun);
      _fun = _expression->car();
      push(_args);
      _args = _expression->cdr();
      push(&evaluator::ev1);
      _cont = &evaluator::peval1;
      break;
    case type::Symbol:
      _cont = &evaluator::lookup;
      break;
    case type::Indirect:
      send(_expression->indirectval());
      pop(_cont);
      break;
    case type::Cvariable:
      send(_expression->cvarval());
      pop(_cont);
      break;
    case type::Free:
      abort(error_errc::corrupt_data, _expression);
      break;
    default:
      send(_expression);
      pop(_cont);
      break;
  }
  return false;
}

bool evaluator::ev1()
{
  pop(_args);
  pop(_fun);
  pop(_cont);
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
        pop(_cont);
        return true;
        break;
      case -1:
        abort(error_errc::no_message, NIL);
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
  LISPT al = getprop(_expression->car(), C_AUTOLOAD);
  if(!is_NIL(al))
  {
    push(_expression);
    push(_dest);
    details::file::load(_ctx, al);
    pop(_dest);
    pop(_expression);
    _fun = _expression->car()->value();
    if(type_of(_fun) == type::Unbound)
    {
      if(!evalhook(_expression))
        xbreak(error_errc::undef_function, _expression->car(), continuation);
    }
    else
      _cont = continuation;
  }
  else
  {
    if(type_of(_expression) == type::Cons && type_of(_expression->car()) == type::Symbol
      && type_of(_expression->car()->value()) == type::Unbound)
    {
      if(!evalhook(_expression))
        xbreak(error_errc::undef_function, _expression->car(), continuation);
    }
    else
    {
      _fun = _expression->car();
      _args = _expression->cdr();
      _cont = continuation;
    }
  }
}

bool evaluator::do_default(continuation_t continuation)
{
  if(type_of(_expression) == type::Cons && type_of(_expression->car()) == type::Symbol
    && type_of(_expression->car()->value()) == type::Unbound)
  {
    if(!evalhook(_expression))
      xbreak(error_errc::undef_function, _expression->car(), continuation);
    return true;
  }
  return false;
}

bool evaluator::peval1()
{
  if(_ctx.brkflg)
    xbreak(error_errc::kbd_break, _fun, &evaluator::peval1);
  else if(_ctx.interrupt)
    abort(error_errc::no_message, NIL);
  else
    switch(type_of(_fun))
    {
      case type::Closure:
        push(&evaluator::peval1);
        _cont = &evaluator::evclosure;
        break;
      case type::Subr:
      case type::Fsubr:
        push(_dest);
        push(&evaluator::ev2);
        _dest = mkdestblock(static_cast<int>(_fun->subrval().argcount()));
        _noeval = _fun->subrval().subr == subr_t::subr::NOEVAL;
        if(_fun->subrval().spread == subr_t::spread::NOSPREAD)
        {
          if(!_noeval)
          {
            push(&evaluator::noevarg);
            _cont = &evaluator::evlis;
          }
          else
            _cont = &evaluator::spread;
        }
        else
          _cont = &evaluator::evalargs;
        break;
      case type::Lambda:
        _noeval = false;
        _cont = &evaluator::evlam;
        break;
      case type::Nlambda:
        _noeval = true;
        _cont = &evaluator::evlam;
        break;
      case type::Cons:
      case type::Indirect:
        _expression = _fun;
        push(&evaluator::ev3);
        _cont = &evaluator::peval;
        break;
      case type::Symbol:
        _fun = _fun->value();
        _cont = &evaluator::peval1;
        break;
      case type::Unbound:
        do_unbound(&evaluator::peval1);
        break;
      case type::String:
        if(!evalhook(_expression))
          xbreak(error_errc::illegal_function, _fun, &evaluator::peval1);
        break;
      default:
        if(!do_default(&evaluator::peval1))
          xbreak(error_errc::illegal_function, _fun, &evaluator::peval1);
        break;
    }
  return false;
}

bool evaluator::peval2()
{
  if(_ctx.brkflg)
    xbreak(error_errc::kbd_break, _fun, &evaluator::peval2);
  else
    switch(type_of(_fun))
    {
      case type::Closure:
        push(&evaluator::peval2);
        _cont = &evaluator::evclosure;
        break;
      case type::Subr:
      case type::Fsubr:
        push(_dest);
        push(&evaluator::ev2);
        _dest = mkdestblock(static_cast<int>(_fun->subrval().argcount()));
        _noeval = true;
        if(_fun->subrval().spread == subr_t::spread::NOSPREAD)
          _cont = &evaluator::spread;
        else
          _cont = &evaluator::evalargs;
        break;
      case type::Lambda:
      case type::Nlambda:
        _noeval = true;
        _cont = &evaluator::evlam;
        break;
      case type::Cons:
      case type::Indirect:
        _expression = _fun;
        push(&evaluator::ev3p);
        _cont = &evaluator::peval;
        break;
      case type::Symbol:
        _fun = _fun->value();
        _cont = &evaluator::peval2;
        break;
      case type::Unbound:
        do_unbound(&evaluator::peval2);
        break;
      case type::String:
        if(!evalhook(_expression))
          xbreak(error_errc::illegal_function, _fun, &evaluator::peval2);
        break;
      default:
        if(!do_default(&evaluator::peval2))
          xbreak(error_errc::illegal_function, _fun, &evaluator::peval2);
        break;
    }
  return false;
}

/// @brief Prints a backtrace of all expressions on the stack.
void evaluator::bt()
{
  int op = _ctx.printlevel;
  _ctx.printlevel = 2;
  for(int i = _toctrl - 1; i != 0; i--)
  {
    if(auto* cont = std::get_if<continuation_t>(&_control[i]); (cont != nullptr) && *cont == &evaluator::ev0)
      details::file::print(_ctx, std::get<LISPT>(_control[i - 1]), T);
  }
  _ctx.printlevel = op;
}

bool evaluator::everr()
{
  send(break0(_expression));
  pop(_cont); // Discard one continuation.
  pop(_cont);
  return false;
}

bool evaluator::noevarg()
{
  _args = receive();
  _cont = &evaluator::spread;
  return false;
}

bool evaluator::evalargs()
{
  if(is_NIL(_args))
  {
    pop(_cont);
  }
  else
  {
    _expression = _args->car();
    if(_noeval)
      _cont = &evaluator::noev9;
    else
      _cont = &evaluator::ev9;
  }
  return false;
}

bool evaluator::ev9()
{
  if(is_NIL(_args->cdr()))
  {
    _cont = &evaluator::peval;
  }
  else
  {
    push(&evaluator::ev11);
    _cont = &evaluator::peval;
  }
  return false;
}

bool evaluator::ev11()
{
  next();
  _args = _args->cdr();
  _expression = _args->car();
  _cont = &evaluator::ev9;
  return false;
}

bool evaluator::noev9()
{
  while(true)
  {
    if(is_NIL(_args->cdr()))
    {
      send(_expression);
      pop(_cont);
      break;
    }
    send(_expression);
    next();
    _args = _args->cdr();
    _expression = _args->car();
  }
  return false;
}

bool evaluator::evlis()
{
  if(is_NIL(_args))
  {
    pop(_cont);
  }
  else
  {
    _expression = _args->car();
    _cont = &evaluator::evlis1;
  }
  return false;
}

bool evaluator::evlis1()
{
  if(is_NIL(_args->cdr()))
  {
    push(&evaluator::evlis2);
    _cont = &evaluator::peval;
  }
  else
  {
    push(&evaluator::evlis3);
    _cont = &evaluator::peval;
  }
  return false;
}

bool evaluator::evlis2()
{
  LISPT x = cons(receive(), NIL);
  send(x);
  pop(_cont);
  return false;
}

bool evaluator::evlis3()
{
  push(_dest);
  _dest = mkdestblock(1);
  push(&evaluator::evlis4);
  _args = _args->cdr();
  _expression = _args->car();
  _cont = &evaluator::evlis1;
  return false;
}

bool evaluator::evlis4()
{
  LISPT x = receive();
  dfree(_dest);
  pop(_dest);
  x = cons(receive(), x);
  send(x);
  pop(_cont);
  return false;
}

bool evaluator::evlam()
{
  push(_expression);
  push(_env);
  push(_dest);
  int ac = 0;
  auto spr = false;
  if((ac = _fun->lambda().count) < 0)
  {
    ac = -ac;
    spr = true;
  }
  _dest = mkdestblock(ac);
  auto i = ac;
  for(auto foo = _fun->lambda().args; i != 0; foo = foo->cdr(), i--)
    storevar(foo->car(), i);
  push(&evaluator::evlam1);
  if(spr)
  {
    if(_noeval)
      _cont = &evaluator::spread;
    else
    {
      push(&evaluator::noevarg);
      _cont = &evaluator::evlis;
    }
  }
  else
    _cont = &evaluator::evalargs;
  return false;
}

bool evaluator::spread()
{
  while(true)
  {
    if(is_NIL(_args))
    {
      pop(_cont);
      break;
    }
    if(_dest[0].index() == 1)
    {
      send(_args);
      pop(_cont);
      break;
    }
    send(_args->car());
    next();
    _args = _args->cdr();
  }
  return false;
}

bool evaluator::ev2()
{
  try
  {
    auto foo = call(_fun);
    dfree(_dest);
    pop(_dest);
    send(foo);
    pop(_cont);
  }
  catch(const lisp_reset&)
  {}
  catch(const lisp_error& ex)
  {
    if(!_interactive)
      throw;
    auto foo = printwhere();
    if(is_NIL(foo))
      xbreak({}, NIL, &evaluator::peval1);
    else
      xbreak({}, foo->car(), &evaluator::peval1); /* CAR(_) broken */
  }
  return false;
}

bool evaluator::ev3()
{
  _fun = receive();
  push(&evaluator::ev4);
  _cont = &evaluator::peval1;
  return false;
}

bool evaluator::ev3p()
{
  _fun = receive();
  push(&evaluator::ev4);
  _cont = &evaluator::peval2;
  return false;
}

bool evaluator::ev4()
{
  pop(_cont);
  return false;
}

void evaluator::link()
{
  _dest[0].link(_env);
  _env = _dest;
  for(auto i = _dest[0].size(); i > 0; i--)
  {
    LISPT t = _dest[i].var()->value();
    _dest[i].var()->value(_dest[i].val());
    _dest[i].val(t);
  }
}

bool evaluator::evlam1()
{
  link();
  pop(_dest);
  _args = _fun->lambda().body;
  push(&evaluator::evlam0);
  _cont = &evaluator::evsequence;
  return false;
}

void evaluator::restore_env()
{
  auto* c = _env;
  for(auto i = c[0].size(); i > 0; i--)
    c[i].var()->value(c[i].val());
}

bool evaluator::evlam0()
{
  restore_env();
  pop_env();
  pop(_expression);
  pop(_cont);
  return false;
}

void evaluator::unwind()
{
  while(_env != nullptr)
  {
    restore_env();
    _env = _env->link();
  }
  reset();
}

bool evaluator::lookup()
{
  LISPT t = _expression->value();
  switch(type_of(t))
  {
    case type::Unbound:
      xbreak(error_errc::unbound_variable, _expression, &evaluator::lookup);
      return false;
      break;
    case type::Indirect:
      send(t->indirectval());
      break;
    case type::Cvariable:
      send(t->cvarval());
      break;
    default:
      send(t);
      break;
  }
  pop(_cont);
  return false;
}

bool evaluator::evclosure()
{
  push(_env);
  push(_dest);
  _dest = mkdestblock(_fun->closure()->count);
  {
    auto foo = _fun->closure()->closed;
    auto i = _fun->closure()->count;
    for(; i != 0; foo = foo->cdr(), i--)
      storevar(foo->car(), i);
  }
  for(auto foo = _fun->closure()->cvalues; !is_NIL(foo); foo = foo->cdr())
  {
    send(foo->car());
    next();
  }
  _fun = _fun->closure()->cfunction;
  link();
  pop(_dest);
  destblock_t* envir = nullptr;
  pop(envir);
  pop(_cont);
  push(envir);
  push(&evaluator::evclosure1);
  return false;
}

bool evaluator::evclosure1()
{
  restore_env();
  pop_env();
  pop(_cont);
  return false;
}

bool evaluator::evsequence()
{
  if(is_NIL(_args))
  {
    pop(_cont);
  }
  else
  {
    _expression = _args->car();
    _cont = &evaluator::evseq1;
  }
  return false;
}

bool evaluator::evseq1()
{
  if(is_NIL(_args->cdr()))
    _cont = &evaluator::peval;
  else
  {
    push(&evaluator::evseq3);
    _cont = &evaluator::peval;
  }
  return false;
}

bool evaluator::evseq3()
{
  _args = _args->cdr();
  _expression = _args->car();
  _cont = &evaluator::evseq1;
  return false;
}

LISPT evaluator::destblock(const destblock_t* block)
{
  if(block == nullptr)
    return NIL;
  LISPT foo = tconc(NIL, mknumber(block->size()));
  for(int i = 0; i != block->size(); ++i)
  {
    foo = tconc(foo, cons((block + i + 1)->var(), (block + i + 1)->val()));
  }
  return car(foo);
}

// NOLINTNEXTLINE(readability-function-cognitive-complexity)
LISPT evaluator::baktrace()
{
  for(int i = _toctrl; i >= 0; i--)
  {
    _ctx.primerr()->format("{}: ", i);
    std::visit(
      // NOLINTNEXTLINE(readability-function-cognitive-complexity)
      [this](auto&& arg) {
        using ArgType = std::decay_t<decltype(arg)>;
        if constexpr(std::is_same_v<ArgType, LISPT>)
          print(arg, T);
        else if constexpr(std::is_same_v<ArgType, destblock_t*>)
        {
          if(arg != nullptr)
          {
            prin1("destblock_t: "_l, T);
            print(destblock(arg), T);
          }
          else
          {
            prin1("destblock_t: nullptr"_l, T);
            terpri(T);
          }
        }
        else if constexpr(std::is_same_v<ArgType, continuation_t>)
        {
          if(arg == &evaluator::ev0)
            _ctx.primerr()->format("ev0\n");
          else if(arg == &evaluator::peval)
            _ctx.primerr()->format("peval\n");
          else if(arg == &evaluator::peval1)
            _ctx.primerr()->format("peval1\n");
          else if(arg == &evaluator::peval2)
            _ctx.primerr()->format("peval2\n");
          else if(arg == &evaluator::ev0)
            _ctx.primerr()->format("ev0\n");
          else if(arg == &evaluator::ev1)
            _ctx.primerr()->format("ev1\n");
          else if(arg == &evaluator::ev2)
            _ctx.primerr()->format("ev2\n");
          else if(arg == &evaluator::ev3)
            _ctx.primerr()->format("ev3\n");
          else if(arg == &evaluator::ev4)
            _ctx.primerr()->format("ev4\n");
          else if(arg == &evaluator::evlam0)
            _ctx.primerr()->format("evlam0\n");
          else if(arg == &evaluator::evlam1)
            _ctx.primerr()->format("evlam1\n");
          else if(arg == &evaluator::ev9)
            _ctx.primerr()->format("ev9\n");
          else if(arg == &evaluator::ev11)
            _ctx.primerr()->format("ev11\n");
          else if(arg == &evaluator::ev3p)
            _ctx.primerr()->format("ev3p\n");
          else if(arg == &evaluator::evalargs)
            _ctx.primerr()->format("evalargs\n");
          else if(arg == &evaluator::noevarg)
            _ctx.primerr()->format("noevarg\n");
          else if(arg == &evaluator::evlam)
            _ctx.primerr()->format("evlam\n");
          else if(arg == &evaluator::spread)
            _ctx.primerr()->format("spread\n");
          else if(arg == &evaluator::evlis)
            _ctx.primerr()->format("evlis\n");
          else if(arg == &evaluator::evlis1)
            _ctx.primerr()->format("evlis1\n");
          else if(arg == &evaluator::evlis2)
            _ctx.primerr()->format("evlis2\n");
          else if(arg == &evaluator::evlis3)
            _ctx.primerr()->format("evlis3\n");
          else if(arg == &evaluator::evlis4)
            _ctx.primerr()->format("evlis4\n");
          else if(arg == &evaluator::noev9)
            _ctx.primerr()->format("noev9\n");
          else if(arg == &evaluator::evsequence)
            _ctx.primerr()->format("evsequence\n");
          else if(arg == &evaluator::evseq1)
            _ctx.primerr()->format("evseq1\n");
          else if(arg == &evaluator::evseq3)
            _ctx.primerr()->format("evseq3\n");
          else if(arg == &evaluator::evclosure)
            _ctx.primerr()->format("evclosure\n");
          else if(arg == &evaluator::evclosure1)
            _ctx.primerr()->format("evclosure1\n");
          else if(arg == &evaluator::eval0)
            _ctx.primerr()->format("eval0\n");
          else if(arg == &evaluator::apply0)
            _ctx.primerr()->format("apply0\n");
          else if(arg == &evaluator::everr)
            _ctx.primerr()->format("everr\n");
          else if(arg == &evaluator::lookup)
            _ctx.primerr()->format("lookup\n");
          else
            _ctx.stderr()->format("Unknown control stack element\n");
        }
        else
          ; // Do nothing for monostate
      },
      _control[i]);
  }
  return NIL;
}

LISPT evaluator::topofstack()
{
  auto x = alloc::getobject();
  x->settype(type::Environ);
  x->set(_ctx.e().environment());
  return x;
}

LISPT evaluator::destblock(LISPT e)
{
  check(e, type::Environ);
  return destblock(e->envval());
}

///
/// @brief Allocates a destination block of size size.
///
/// @param size The size of the destination block.
/// @returns A destblock or nullptr if no more space available.
destblock_t* evaluator::dalloc(int size)
{
  if(size <= DESTBLOCKSIZE - _destblockused - 1)
  {
    auto* dest = &_destblock[_destblockused];
    _destblockused += size + 1;
    dest->num(static_cast<std::int8_t>(size));
    for(int i = 1; i <= size; ++i)
      _destblock[_destblockused - i].reset();
    return dest;
  }
  return nullptr;
}

/// @brief Free a destination block.
///
/// @param ptr The destination block to free.
void evaluator::dfree(destblock_t* block) { _destblockused -= block->size() + 1; }

/// @brief Frees all destination blocks.
void evaluator::dzero() { _destblockused = 0; }

} // namespace lisp

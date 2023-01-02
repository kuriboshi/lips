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

#include "alloc.hh"
#include "check.hh"
#include "eval.hh"
#include "file.hh"
#include "prim.hh"
#include "prop.hh"

using namespace std::literals;

namespace lisp::details::vm
{

namespace pn
{
inline constexpr std::string_view E = "e";                   // noeval version of eval
inline constexpr std::string_view EVAL = "eval";             // evaluate exp
inline constexpr std::string_view APPLY = "apply";           // apply function on args
inline constexpr std::string_view APPLYSTAR = "apply*";      // apply nospread
inline constexpr std::string_view BACKTRACE = "backtrace";   // control stack backtrace
inline constexpr std::string_view TOPOFSTACK = "topofstack"; // return top of value stack
inline constexpr std::string_view DESTBLOCK = "destblock";   // convert environment to list
} // namespace pn

lisp_t eval(context& ctx, lisp_t expr) { return ctx.vm().eval(expr); }
lisp_t apply(context& ctx, lisp_t fun, lisp_t args) { return ctx.vm().apply(fun, args); }
lisp_t backtrace(context& ctx) { return ctx.vm().backtrace(); }
lisp_t topofstack(context& ctx) { return ctx.vm().topofstack(); }
lisp_t destblock(context& ctx, lisp_t a) { return ctx.vm().destblock(a); }

void init()
{
  // clang-format off
  mkprim(pn::E,          eval,       subr_t::subr::NOEVAL, subr_t::spread::SPREAD);
  mkprim(pn::EVAL,       eval,       subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  mkprim(pn::APPLY,      apply,      subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  mkprim(pn::APPLYSTAR,  apply,      subr_t::subr::EVAL,   subr_t::spread::NOSPREAD);
  mkprim(pn::BACKTRACE,  backtrace,  subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  mkprim(pn::TOPOFSTACK, topofstack, subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  mkprim(pn::DESTBLOCK,  destblock,  subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  // clang-format on
}

} // namespace lisp::details::vm

namespace lisp
{
vm::vm(context& ctx)
  : _ctx(ctx)
{}

void vm::reset()
{
  _destblockused = 0;
  _toctrl = 0;
  _fun = nil;
  _args = nil;
  _dest = nullptr;
  _env = nullptr;
}

lisp_t vm::printwhere()
{
  int i = _toctrl - 1;
  for(; i != 0; --i) // Find latest completed call
  {
    if(auto* cont = std::get_if<continuation_t>(&_control[i]); (cont != nullptr) && *cont == &vm::evlam0)
      break;
  }
  lisp_t foo;
  for(; i != 0; --i)
  {
    if(auto* func = std::get_if<continuation_t>(&_control[i]); (func != nullptr) && *func == &vm::eval_end)
    {
      if(auto* lsp = std::get_if<lisp_t>(&_control[i - 1]);
         lsp != nullptr && (type_of(*lsp) == object::type::Cons && type_of((*lsp)->car()) != object::type::Cons))
      {
        foo = *lsp;
        _ctx.primerr()->format("[in ");
        prin2(foo->car(), T);
        _ctx.primerr()->putch(']');
        break;
      }
    }
  }
  _ctx.primerr()->putch('\n');
  return foo;
}

void vm::abort(std::error_code error)
{
  _ctx.perror(error);
  printwhere();
  unwind();
  throw lisp_error("abort");
}

void vm::overflow() { abort(error_errc::stack_overflow); }

void vm::xbreak(std::error_code code, lisp_t fault, continuation_t next)
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
  print(cons(fault, cons(C_BROKEN, nil)), T);
  push(next);
  _cont = &vm::everr;
}

void vm::storevar(lisp_t v, int i) { _dest[i].var(v); }

void vm::pop_env()
{
  free(_env);
  pop(_env);
}

void vm::send(lisp_t a)
{
  if(_dest[0].index() > 0)
    _dest[_dest[0].index()].val(a);
}

lisp_t vm::receive() { return _dest[_dest[0].index()].val(); }

void vm::next() { _dest[0].decr(); }

lisp_t vm::call(lisp_t fun)
{
  return fun->subr()(_ctx, _dest);
}

lisp_t vm::eval(lisp_t expr)
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
  // the continuation variable `cont' to `eval_expr'. `eval_expr' may push more
  // contiuations onto the stack but eventaully `eval0' is called which returns
  // true, signalling end of evaluation.
  //
  push(&vm::eval0);
  _cont = &vm::eval_expr;
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
  catch(const lisp_reset& ex)
  {
    unwind();
    return C_ERROR;
  }
  //
  // Retrieve the result of the evaluation and restore the previous
  // destination.
  //
  lisp_t foo = receive();
  pop(_dest);
  // Return the result.
  return foo;
}

bool vm::eval0()
{
  free(_dest);
  // Signal the end of the evaluation.
  return true;
}

lisp_t vm::apply(lisp_t f, lisp_t x)
{
  push(_dest);
  _dest = mkdestblock(1);
  push(_fun);
  _fun = f;
  push(_args);
  _args = x;
  _expression = cons(f, x);
  push(&vm::apply0);
  _cont = &vm::eval_apply;
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
  lisp_t foo = receive();
  pop(_dest);
  return foo;
}

bool vm::apply0()
{
  free(_dest);
  pop(_args);
  pop(_fun);
  return true;
}

bool vm::eval_expr()
{
#ifdef TRACE
  if(_trace)
    details::file::print(l, expression, T);
#endif
  push(_expression);
  push(&vm::eval_end);
  switch(type_of(_expression))
  {
    case object::type::Cons:
      push(_fun);
      _fun = _expression->car();
      push(_args);
      _args = _expression->cdr();
      push(&vm::ev1);
      _cont = &vm::eval_func;
      break;
    case object::type::Symbol:
      _cont = &vm::eval_lookup;
      break;
    case object::type::Indirect:
      send(_expression->indirect());
      pop(_cont);
      break;
    default:
      send(_expression);
      pop(_cont);
      break;
  }
  return false;
}

bool vm::eval_end()
{
  //
  // Discard the top of stack (it's the previous expression, see comment in
  // `eval' above) and restore continuation.  The function `eval_end' is also
  // used as a placeholder for the beginning of an eval.
  //
  _toctrl -= 1;
  pop(_cont);
  return false;
}

bool vm::ev1()
{
  pop(_args);
  pop(_fun);
  pop(_cont);
  return false;
}

bool vm::evalhook(lisp_t exp)
{
  lisp_t res;

  if(_undefhook)
    switch(_undefhook(exp, &res))
    {
      case 1:
        send(res);
        pop(_cont);
        return true;
      case -1:
        abort(error_errc::no_message);
      default:
        return false;
    }
  return false;
}

void vm::do_unbound(continuation_t continuation)
{
  //
  // If an undefined symbol has the AUTOLOAD property, we try to load the
  // definition from a file. If that doesn't succeed, then the symbol is
  // undefined.
  //
  lisp_t al = getprop(_expression->car(), C_AUTOLOAD);
  if(!is_nil(al))
  {
    push(_expression);
    push(_dest);
    // al has to be a string or a symbol
    load(al);
    pop(_dest);
    pop(_expression);
    _fun = _expression->car()->value();
    if(_fun == C_UNBOUND)
    {
      if(!evalhook(_expression))
        xbreak(error_errc::undef_function, _expression->car(), continuation);
    }
    else
      _cont = continuation;
  }
  else
  {
    if(!evalhook(_expression))
      xbreak(error_errc::undef_function, _expression->car(), continuation);
  }
}

bool vm::eval_func()
{
  if(_ctx.brkflg)
    xbreak(error_errc::kbd_break, _fun, &vm::eval_func);
  else if(_ctx.interrupt)
    abort(error_errc::no_message);
  else
    switch(type_of(_fun))
    {
      case object::type::Closure:
        push(&vm::eval_func);
        _cont = &vm::eval_closure;
        break;
      case object::type::Subr:
        push(_dest);
        push(&vm::eval_prim);
        _dest = mkdestblock(static_cast<int>(_fun->subr().argcount()));
        _noeval = _fun->subr().subr == subr_t::subr::NOEVAL;
        if(_fun->subr().spread == subr_t::spread::NOSPREAD)
        {
          if(!_noeval)
          {
            push(&vm::noevarg);
            _cont = &vm::eval_list;
          }
          else
            _cont = &vm::spread;
        }
        else
          _cont = &vm::eval_args;
        break;
      case object::type::Lambda:
        _noeval = !_fun->lambda().eval;
        _cont = &vm::eval_lambda;
        break;
      case object::type::Cons:
      case object::type::Indirect:
        _expression = _fun;
        push(&vm::ev3);
        _cont = &vm::eval_expr;
        break;
      case object::type::Symbol:
        _fun = _fun->value();
        if(_fun == C_UNBOUND)
          do_unbound(&vm::eval_func);
        else
          _cont = &vm::eval_func;
        break;
      case object::type::String:
        if(!evalhook(_expression))
          xbreak(error_errc::illegal_function, _fun, &vm::eval_func);
        break;
      case object::type::Cvariable:
        _fun = _fun->cvariable();
        break;
      default:
        xbreak(error_errc::illegal_function, _fun, &vm::eval_func);
        break;
    }
  return false;
}

bool vm::eval_apply()
{
  if(_ctx.brkflg)
    xbreak(error_errc::kbd_break, _fun, &vm::eval_apply);
  else
    switch(type_of(_fun))
    {
      case object::type::Closure:
        push(&vm::eval_apply);
        _cont = &vm::eval_closure;
        break;
      case object::type::Subr:
        push(_dest);
        push(&vm::eval_prim);
        _dest = mkdestblock(static_cast<int>(_fun->subr().argcount()));
        _noeval = true;
        if(_fun->subr().spread == subr_t::spread::NOSPREAD)
          _cont = &vm::spread;
        else
          _cont = &vm::eval_args;
        break;
      case object::type::Lambda:
        _noeval = true;
        _cont = &vm::eval_lambda;
        break;
      case object::type::Cons:
      case object::type::Indirect:
        _expression = _fun;
        push(&vm::ev3p);
        _cont = &vm::eval_expr;
        break;
      case object::type::Symbol:
        _fun = _fun->value();
        if(_fun == C_UNBOUND)
          do_unbound(&vm::eval_apply);
        else
          _cont = &vm::eval_apply;
        break;
      case object::type::String:
        if(!evalhook(_expression))
          xbreak(error_errc::illegal_function, _fun, &vm::eval_apply);
        break;
      default:
        xbreak(error_errc::illegal_function, _fun, &vm::eval_apply);
        break;
    }
  return false;
}

void vm::bt()
{
  int op = _ctx.printlevel;
  _ctx.printlevel = 2;
  for(int i = _toctrl - 1; i != 0; i--)
  {
    if(auto* cont = std::get_if<continuation_t>(&_control[i]); (cont != nullptr) && *cont == &vm::eval_end)
      print(std::get<lisp_t>(_control[i - 1]), T);
  }
  _ctx.printlevel = op;
}

bool vm::everr()
{
  auto b = break0(_expression);
  if(b == C_EOF)
    throw lisp_reset();
  send(b);
  pop(_cont); // Discard one continuation.
  pop(_cont);
  return false;
}

bool vm::noevarg()
{
  _args = receive();
  _cont = &vm::spread;
  return false;
}

// Evaluate a sequence of expressions. If there are no arguments then there is
// nothing to do. Otherwise evaluate the first expression and move to
// eval_args2.
bool vm::eval_args()
{
  if(is_nil(_args))
    pop(_cont);
  else
  {
    _expression = _args->car();
    if(_noeval)
      _cont = &vm::noeval_args1;
    else
      _cont = &vm::eval_args1;
  }
  return false;
}

// If we're at the end of the list we evaluate the expression and finish.
bool vm::eval_args1()
{
  if(is_nil(_args->cdr()))
  {
    _cont = &vm::eval_expr;
  }
  else
  {
    push(&vm::eval_args2);
    _cont = &vm::eval_expr;
  }
  return false;
}

// Move to the next expression in the sequence.
bool vm::eval_args2()
{
  next();
  _args = _args->cdr();
  _expression = _args->car();
  _cont = &vm::eval_args1;
  return false;
}

// Process expressions without evaluating them.
bool vm::noeval_args1()
{
  while(true)
  {
    if(is_nil(_args->cdr()))
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

// Evaluate a list of expressions and collect result in another list
bool vm::eval_list()
{
  if(is_nil(_args))
    pop(_cont);
  else
  {
    _expression = _args->car();
    _cont = &vm::evlis1;
  }
  return false;
}

bool vm::evlis1()
{
  if(is_nil(_args->cdr()))
  {
    push(&vm::eval_list_end);
    _cont = &vm::eval_expr;
  }
  else
  {
    push(&vm::evlis3);
    _cont = &vm::eval_expr;
  }
  return false;
}

// Done with the list of expressions.
bool vm::eval_list_end()
{
  lisp_t x = cons(receive(), nil);
  send(x);
  pop(_cont);
  return false;
}

bool vm::evlis3()
{
  push(_dest);
  _dest = mkdestblock(1);
  push(&vm::evlis4);
  _args = _args->cdr();
  _expression = _args->car();
  _cont = &vm::evlis1;
  return false;
}

bool vm::evlis4()
{
  lisp_t x = receive();
  free(_dest);
  pop(_dest);
  x = cons(receive(), x);
  send(x);
  pop(_cont);
  return false;
}

bool vm::eval_lambda()
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
  push(&vm::evlam1);
  if(spr)
  {
    if(_noeval)
      _cont = &vm::spread;
    else
    {
      push(&vm::noevarg);
      _cont = &vm::eval_list;
    }
  }
  else
    _cont = &vm::eval_args;
  return false;
}

bool vm::spread()
{
  while(true)
  {
    if(is_nil(_args))
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

bool vm::eval_prim()
{
  try
  {
    auto foo = call(_fun);
    free(_dest);
    pop(_dest);
    send(foo);
    pop(_cont);
  }
  catch(const lisp_error& ex)
  {
    if(!_interactive)
      throw;
    auto foo = printwhere();
    if(is_nil(foo))
      xbreak({}, nil, &vm::eval_func);
    else
      xbreak({}, foo->car(), &vm::eval_func); /* CAR(_) broken */
  }
  return false;
}

bool vm::ev3()
{
  _fun = receive();
  push(&vm::ev4);
  _cont = &vm::eval_func;
  return false;
}

bool vm::ev3p()
{
  _fun = receive();
  push(&vm::ev4);
  _cont = &vm::eval_apply;
  return false;
}

bool vm::ev4()
{
  pop(_cont);
  return false;
}

void vm::link()
{
  _dest[0].link(_env);
  _env = _dest;
  for(auto i = _dest[0].size(); i > 0; i--)
  {
    lisp_t t = _dest[i].var()->value();
    _dest[i].var()->value(_dest[i].val());
    _dest[i].val(t);
  }
}

bool vm::evlam1()
{
  link();
  pop(_dest);
  _args = _fun->lambda().body;
  push(&vm::evlam0);
  _cont = &vm::eval_sequence;
  return false;
}

void vm::restore_env()
{
  auto* c = _env;
  for(auto i = c[0].size(); i > 0; i--)
    c[i].var()->value(c[i].val());
}

bool vm::evlam0()
{
  restore_env();
  pop_env();
  pop(_expression);
  pop(_cont);
  return false;
}

void vm::unwind()
{
  while(_env != nullptr)
  {
    restore_env();
    _env = _env->link();
  }
  reset();
}

bool vm::eval_lookup()
{
  lisp_t t = _expression->value();
  switch(type_of(t))
  {
    case object::type::Indirect:
      send(t->indirect());
      break;
    case object::type::Cvariable:
      send(t->cvariable());
      break;
    default:
      if(t == C_UNBOUND)
      {
        xbreak(error_errc::unbound_variable, _expression, &vm::eval_lookup);
        return false;
      }
      send(t);
      break;
  }
  pop(_cont);
  return false;
}

bool vm::eval_closure()
{
  push(_env);
  push(_dest);
  _dest = mkdestblock(_fun->closure().count);
  {
    auto foo = _fun->closure().closed;
    auto i = _fun->closure().count;
    for(; i != 0; foo = foo->cdr(), i--)
      storevar(foo->car(), i);
  }
  for(auto foo = _fun->closure().cvalues; !is_nil(foo); foo = foo->cdr())
  {
    send(foo->car());
    next();
  }
  _fun = _fun->closure().cfunction;
  link();
  pop(_dest);
  destblock_t* envir = nullptr;
  pop(envir);
  pop(_cont);
  push(envir);
  push(&vm::eval_closure1);
  return false;
}

bool vm::eval_closure1()
{
  restore_env();
  pop_env();
  pop(_cont);
  return false;
}

// Evaluate a sequence of expressions in _args.
bool vm::eval_sequence()
{
  if(is_nil(_args))
    pop(_cont);
  else
  {
    _expression = _args->car();
    _cont = &vm::eval_seq1;
  }
  return false;
}

// Evaluate current expression and stop if we've reached the end of the list.
bool vm::eval_seq1()
{
  if(is_nil(_args->cdr()))
    _cont = &vm::eval_expr;
  else
  {
    push(&vm::eval_seq2);
    _cont = &vm::eval_expr;
  }
  return false;
}

// Move to the next expression to evaluate and continue with eval_seq1.
bool vm::eval_seq2()
{
  _args = _args->cdr();
  _expression = _args->car();
  _cont = &vm::eval_seq1;
  return false;
}

lisp_t vm::destblock(const destblock_t* block)
{
  if(block == nullptr)
    return nil;
  lisp_t foo = tconc(nil, mknumber(block->size()));
  for(int i = 0; i != block->size(); ++i)
  {
    foo = tconc(foo, cons((block + i + 1)->var(), (block + i + 1)->val()));
  }
  return car(foo);
}

// NOLINTNEXTLINE(readability-function-cognitive-complexity)
lisp_t vm::backtrace()
{
  for(int i = _toctrl; i >= 0; i--)
  {
    _ctx.primerr()->format("{}: ", i);
    std::visit(
      // NOLINTNEXTLINE(readability-function-cognitive-complexity)
      [this](auto&& arg) {
        using ArgType = std::decay_t<decltype(arg)>;
        if constexpr(std::is_same_v<ArgType, lisp_t>)
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
          if(arg == &vm::eval_end)
            _ctx.primerr()->format("eval_end\n");
          else if(arg == &vm::eval_expr)
            _ctx.primerr()->format("eval_expr\n");
          else if(arg == &vm::eval_func)
            _ctx.primerr()->format("eval_func");
          else if(arg == &vm::eval_apply)
            _ctx.primerr()->format("eval_apply\n");
          else if(arg == &vm::ev1)
            _ctx.primerr()->format("ev1\n");
          else if(arg == &vm::eval_prim)
            _ctx.primerr()->format("eval_prim\n");
          else if(arg == &vm::ev3)
            _ctx.primerr()->format("ev3\n");
          else if(arg == &vm::ev4)
            _ctx.primerr()->format("ev4\n");
          else if(arg == &vm::evlam0)
            _ctx.primerr()->format("evlam0\n");
          else if(arg == &vm::evlam1)
            _ctx.primerr()->format("evlam1\n");
          else if(arg == &vm::eval_args1)
            _ctx.primerr()->format("eval_args1\n");
          else if(arg == &vm::eval_args2)
            _ctx.primerr()->format("eval_args2\n");
          else if(arg == &vm::ev3p)
            _ctx.primerr()->format("ev3p\n");
          else if(arg == &vm::eval_args)
            _ctx.primerr()->format("eval_args\n");
          else if(arg == &vm::noevarg)
            _ctx.primerr()->format("noevarg\n");
          else if(arg == &vm::eval_lambda)
            _ctx.primerr()->format("eval_lambda\n");
          else if(arg == &vm::spread)
            _ctx.primerr()->format("spread\n");
          else if(arg == &vm::eval_list)
            _ctx.primerr()->format("eval_list\n");
          else if(arg == &vm::evlis1)
            _ctx.primerr()->format("evlis1\n");
          else if(arg == &vm::eval_list_end)
            _ctx.primerr()->format("eval_list_end\n");
          else if(arg == &vm::evlis3)
            _ctx.primerr()->format("evlis3\n");
          else if(arg == &vm::evlis4)
            _ctx.primerr()->format("evlis4\n");
          else if(arg == &vm::noeval_args1)
            _ctx.primerr()->format("noeval_args1\n");
          else if(arg == &vm::eval_sequence)
            _ctx.primerr()->format("eval_sequence\n");
          else if(arg == &vm::eval_seq1)
            _ctx.primerr()->format("eval_seq1\n");
          else if(arg == &vm::eval_seq2)
            _ctx.primerr()->format("eval_seq2\n");
          else if(arg == &vm::eval_closure)
            _ctx.primerr()->format("eval_closure\n");
          else if(arg == &vm::eval_closure1)
            _ctx.primerr()->format("eval_closure1\n");
          else if(arg == &vm::eval0)
            _ctx.primerr()->format("eval0\n");
          else if(arg == &vm::apply0)
            _ctx.primerr()->format("apply0\n");
          else if(arg == &vm::everr)
            _ctx.primerr()->format("everr\n");
          else if(arg == &vm::eval_lookup)
            _ctx.primerr()->format("eval_lookup\n");
          else
            _ctx.stderr()->format("Unknown control stack element\n");
        }
        else
          ; // Do nothing for monostate
      },
      _control[i]);
  }
  return nil;
}

lisp_t vm::topofstack() const { return getobject(environment()); }

lisp_t vm::destblock(lisp_t e)
{
  check(e, object::type::Environ);
  return destblock(e->environ());
}

destblock_t* vm::mkdestblock(int size)
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

void vm::free(destblock_t* block) { _destblockused -= block->size() + 1; }

} // namespace lisp

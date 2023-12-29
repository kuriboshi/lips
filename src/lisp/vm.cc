//
// Lips, lisp shell.
// Copyright 1988, 2020-2023 Krister Joas
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

#include "vm.hh"

#include <string_view>

#include "alloc.hh"
#include "check.hh"
#include "details/arith.hh"
#include "details/debug.hh"
#include "details/logic.hh"
#include "details/low.hh"
#include "details/map.hh"
#include "details/predicate.hh"
#include "details/string.hh"
#include "details/user.hh"
#include "file.hh"
#include "list.hh"
#include "property.hh"
#include "rtable.hh"
#include "version.hh"

namespace
{
lisp::lisp_t make_symbol(std::string_view sym, lisp::lisp_t value = lisp::nil)
{
  auto symbol = lisp::details::alloc::intern(sym);
  symbol->value(value);
  return symbol;
}

lisp::lisp_t make_const(std::string_view sym, lisp::lisp_t value = lisp::nil)
{
  auto symbol = lisp::details::alloc::intern(sym);
  symbol->value(value);
  symbol->as_symbol()->constant = true;
  return symbol;
}

lisp::lisp_t make_t()
{
  auto symbol = make_symbol("t");
  symbol->value(symbol);
  symbol->as_symbol()->constant = true;
  return symbol;
}
} // namespace

namespace lisp::details::vm
{

/// @brief Make an indirect pointer to the object OBJ.
///
lisp_t mkindirect(lisp_t obj)
{
  // If already an indirect type object, return it.  We want all symbols that
  // we include in closures on the same level to refer to the same value.
  if(type_of(obj) == object::type::Indirect)
    return obj;
  // If it's a new object, cons up the storage for it wasting the car part.
  return getobject(indirect_t{obj});
}

/// @brief Builds a list of indirect pointers to the values of the symbols in
/// the list VARS. Used to construct a closure.
///
lisp_t closobj(lisp_t vars)
{
  if(is_nil(vars))
    return nil;
  check(vars, object::type::Cons);
  check(vars->car(), object::type::Symbol);
  return cons(mkindirect(vars->car()->value()), closobj(vars->cdr()));
}

lisp_t quote(lisp_t x) { return x; }

lisp_t lambda(lisp_t x, lisp_t f) { return alloc::mklambda(x, f, true); }

lisp_t nlambda(lisp_t x, lisp_t f) { return alloc::mklambda(x, f, false); }

lisp_t closure(lisp_t fun, lisp_t vars)
{
  auto c = ref_closure_t::create();
  c->cfunction = fun;
  c->closed = vars;
  auto f = length(vars);
  c->count = f->as_integer();
  f = closobj(vars);
  if(f == C_ERROR)
    return f;
  c->cvalues = f;
  return getobject(c);
}

lisp_t error(lisp_t mess)
{
  check(mess, object::type::String);
  return error(error_errc::user_error, mess);
}

lisp_t exit(lisp_t status)
{
  if(is_nil(status))
    throw lisp_finish("exit called", 0);
  check(status, object::type::Integer);
  throw lisp_finish("exit called", status->as_integer());
}

namespace pn
{
inline constexpr std::string_view E = "e";                   // noeval version of eval
inline constexpr std::string_view EVAL = "eval";             // evaluate exp
inline constexpr std::string_view APPLY = "apply";           // apply function on args
inline constexpr std::string_view APPLYSTAR = "apply*";      // apply nospread
inline constexpr std::string_view CLOSURE = "closure";       // create static environment
inline constexpr std::string_view LAMBDA = "lambda";         // create lambda object
inline constexpr std::string_view NLAMBDA = "nlambda";       // make nlambda object
inline constexpr std::string_view QUOTE = "quote";           // don't eval arg
inline constexpr std::string_view BACKTRACE = "backtrace";   // control stack backtrace
inline constexpr std::string_view TOPOFSTACK = "topofstack"; // return top of value stack
inline constexpr std::string_view DESTBLOCK = "destblock";   // convert environment to list
inline constexpr std::string_view ERROR = "error";           // error
inline constexpr std::string_view EXIT = "exit";             // exit lips
} // namespace pn

inline lisp_t eval(lisp_t expr) { return lisp::vm::get().eval(expr); }
inline lisp_t apply(lisp_t fun, lisp_t args) { return lisp::vm::get().apply(fun, args); }
inline lisp_t backtrace() { return lisp::vm::get().backtrace(); }
inline lisp_t topofstack() { return lisp::vm::get().topofstack(); }
inline lisp_t destblock(lisp_t a) { return lisp::vm::destblock(a); }

void init()
{
  // clang-format off
  mkprim(pn::E,          eval,       subr_t::subr::NOEVAL, subr_t::spread::SPREAD);
  mkprim(pn::EVAL,       eval,       subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  mkprim(pn::APPLY,      apply,      subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  mkprim(pn::APPLYSTAR,  apply,      subr_t::subr::EVAL,   subr_t::spread::NOSPREAD);
  mkprim(pn::CLOSURE,    closure,    subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  mkprim(pn::LAMBDA,     lambda,     subr_t::subr::NOEVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::NLAMBDA,    nlambda,    subr_t::subr::NOEVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::QUOTE,      quote,      subr_t::subr::NOEVAL, subr_t::spread::SPREAD);
  mkprim(pn::BACKTRACE,  backtrace,  subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  mkprim(pn::TOPOFSTACK, topofstack, subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  mkprim(pn::DESTBLOCK,  destblock,  subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  mkprim(pn::ERROR,      error,      subr_t::subr::EVAL,   subr_t::spread::NOSPREAD);
  mkprim(pn::EXIT,       exit,       subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  // clang-format on
}

} // namespace lisp::details::vm

namespace lisp
{
const lisp_t C_UNBOUND = make_const("unbound");
const lisp_t T = make_t();
const lisp_t NIL = make_const("nil");
const lisp_t C_APPEND = make_symbol("append");
const lisp_t C_AUTOLOAD = make_symbol("autoload");
const lisp_t C_BROKEN = make_symbol("broken");
const lisp_t C_BT = make_symbol("bt");
const lisp_t C_CLOSURE = make_symbol("closure");
const lisp_t C_CONS = make_symbol("cons");
const lisp_t C_CVARIABLE = make_symbol("cvariable");
const lisp_t C_DOT = make_symbol(".");
const lisp_t C_ENDOFFILE = make_symbol("endoffile");
const lisp_t C_ENVIRON = make_symbol("environ");
const lisp_t C_EOF = make_symbol("eof");
const lisp_t C_ERROR = make_symbol("error");
const lisp_t C_FILE = make_symbol("file");
const lisp_t C_FLOAT = make_symbol("float");
const lisp_t C_FSUBR = make_symbol("fsubr");
const lisp_t C_GO = make_symbol("go");
const lisp_t C_INDIRECT = make_symbol("indirect");
const lisp_t C_INTEGER = make_symbol("integer");
const lisp_t C_LAMBDA = make_symbol("lambda");
const lisp_t C_NLAMBDA = make_symbol("nlambda");
const lisp_t C_OLDDEF = make_symbol("olddef");
const lisp_t C_QUOTE = make_symbol("quote");
const lisp_t C_READ = make_symbol("read");
const lisp_t C_REDEFINED = make_symbol("redefined");
const lisp_t C_RESET = make_symbol("reset");
const lisp_t C_RETURN = make_symbol("return");
const lisp_t C_STRING = make_symbol("string");
const lisp_t C_SUBR = make_symbol("subr");
const lisp_t C_SYMBOL = make_symbol("symbol");
const lisp_t C_VERSION = make_symbol("version", mkstring(version()));
const lisp_t C_WRITE = make_symbol("write");

class init
{
public:
  init()
  {
    auto intern = [](const auto s) { return details::alloc::intern(s); };

    details::alloc::init();
    details::arith::init();
    details::debug::init();
    details::file::init();
    details::list::init();
    details::logic::init();
    details::low::init();
    details::map::init();
    details::predicate::init();
    details::property::init();
    details::string::init();
    details::user::init();
    details::vm::init();

    rtable::init();
  }
};

vm* vm::set(vm* ptr)
{
  static const init init;
  static vm* current{nullptr}; // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)
  if(current == nullptr)
    current = ptr;
  return current;
}

lisp_t vm::printwhere()
{
  int i = _toctrl - 1;
  for(; i != 0; --i) // Find latest completed call
  {
    if(auto* cont = std::get_if<continuation_t>(&_control.at(i)); (cont != nullptr) && *cont == &vm::evlam0)
      break;
  }
  lisp_t foo;
  for(; i != 0; --i)
  {
    if(auto* func = std::get_if<continuation_t>(&_control.at(i)); (func != nullptr) && *func == &vm::eval_end)
    {
      if(auto* lsp = std::get_if<expr_t>(&_control.at(i - 1));
         lsp != nullptr && (type_of(*lsp) == object::type::Cons && type_of((*lsp)->car()) != object::type::Cons))
      {
        foo = *lsp;
        primerr()->format("[in ");
        prin2(foo->car(), T);
        primerr()->putch(']');
        break;
      }
    }
  }
  primerr()->putch('\n');
  return foo;
}

void vm::abort(std::error_code error)
{
  perror(error);
  printwhere();
  unwind();
  throw lisp_error(error_errc::abort);
}

void vm::overflow() { abort(error_errc::stack_overflow); }

void vm::xbreak(std::error_code code, lisp_t fault, continuation_t next)
{
  if(code)
  {
    perror(code, fault);
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

lisp_t vm::call(lisp_t fun) { return fun->subr()(_dest); }

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
  // contiuations onto the stack but eventually `eval0' is called which returns
  // true, signalling end of evaluation.
  //
  push(&vm::eval0);
  _cont = &vm::eval_expr;
  try
  {
    while(!(this->*_cont)())
      ;
  }
  catch(const lisp_reset& ex)
  {
    unwind();
    throw;
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

lisp_t vm::eval(const std::string& expr) { return eval(lispread(expr)); }

bool vm::eval0()
{
  free(_dest);
  // Signal the end of the evaluation.
  return true;
}

lisp_t vm::apply(lisp_t fun, lisp_t args)
{
  push(_dest);
  _dest = mkdestblock(1);
  push(_fun);
  _fun = fun;
  push(_args);
  _args = args;
  _expression = cons(fun, args);
  push(&vm::apply0);
  _cont = &vm::eval_apply;
  try
  {
    while(!(this->*_cont)())
      ;
  }
  catch(const lisp_reset& ex)
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
#ifdef LIPS_ENABLE_TRACE
  if(_trace)
    print(_expression, T);
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
  --_toctrl;
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
  const lisp_t al = getprop(_expression->car(), C_AUTOLOAD);
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
  if(brkflg)
    xbreak(error_errc::kbd_break, _fun, &vm::eval_func);
  else if(interrupt)
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
        if(_fun == _fun->value())
          xbreak(error_errc::illegal_function, _fun, &vm::eval_func);
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
  if(brkflg)
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
  auto op = printlevel();
  printlevel(2);
  for(auto i = _toctrl - 1; i != 0; i--)
  {
    if(auto* cont = std::get_if<continuation_t>(&_control.at(i)); (cont != nullptr) && *cont == &vm::eval_end)
    {
      if(auto* e = std::get_if<expr_t>(&_control.at(i - 1)))
        print(*e, T);
      else if(auto* f = std::get_if<fun_t>(&_control.at(i - 1)))
        print(*f, T);
      else if(auto* a = std::get_if<args_t>(&_control.at(i - 1)))
        print(*a, T);
    }
  }
  printlevel(op);
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
  const lisp_t x = cons(receive(), nil);
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
  int ac = _fun->lambda().count;
  auto spr = false;
  if(ac < 0)
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
      break;
    if(_dest[0].index() == 1)
    {
      send(_args);
      break;
    }
    send(_args->car());
    next();
    _args = _args->cdr();
  }
  pop(_cont);
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
  catch(const lisp_reset& ex)
  {
    throw;
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
    const lisp_t t = _dest[i].var()->value();
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

void vm::reset()
{
  _destblockused = 0;
  _toctrl = 0;
  _fun = nil;
  _args = nil;
  _dest = nullptr;
  _env = nullptr;
}

bool vm::eval_lookup()
{
  const lisp_t t = _expression->value();
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
    foo = tconc(foo, cons((block + i + 1)->var(), (block + i + 1)->val()));
  return car(foo);
}

std::string vm::to_string(const lisp_t& item)
{
  std::ostringstream os;
  os << item;
  return os.str();
}

std::string vm::to_string(const destblock_t* block)
{
  std::ostringstream s;
  if(block != nullptr)
  {
    s << block->size();
    for(int i = 0; i != block->size(); ++i)
    {
      s << " (";
      if((block + i + 1)->var() == nil)
        s << "nil";
      else
        s << (block + i + 1)->var()->as_symbol()->pname;
      s << " . " << (block + i + 1)->val() << ')';
    }
  }
  else
    s << "nil";
  return s.str();
}

// NOLINTNEXTLINE(readability-function-cognitive-complexity)
std::string vm::to_string(continuation_t cont)
{
  if(cont == &vm::eval_end)
    return "eval_end";
  if(cont == &vm::eval_expr)
    return "eval_expr";
  if(cont == &vm::eval_func)
    return "eval_func";
  if(cont == &vm::eval_apply)
    return "eval_apply";
  if(cont == &vm::ev1)
    return "ev1";
  if(cont == &vm::eval_prim)
    return "eval_prim";
  if(cont == &vm::ev3)
    return "ev3";
  if(cont == &vm::ev4)
    return "ev4";
  if(cont == &vm::evlam0)
    return "evlam0";
  if(cont == &vm::evlam1)
    return "evlam1";
  if(cont == &vm::eval_args1)
    return "eval_args1";
  if(cont == &vm::eval_args2)
    return "eval_args2";
  if(cont == &vm::ev3p)
    return "ev3p";
  if(cont == &vm::eval_args)
    return "eval_args";
  if(cont == &vm::noevarg)
    return "noevarg";
  if(cont == &vm::eval_lambda)
    return "eval_lambda";
  if(cont == &vm::spread)
    return "spread";
  if(cont == &vm::eval_list)
    return "eval_list";
  if(cont == &vm::evlis1)
    return "evlis1";
  if(cont == &vm::eval_list_end)
    return "eval_list_end";
  if(cont == &vm::evlis3)
    return "evlis3";
  if(cont == &vm::evlis4)
    return "evlis4";
  if(cont == &vm::noeval_args1)
    return "noeval_args1";
  if(cont == &vm::eval_sequence)
    return "eval_sequence";
  if(cont == &vm::eval_seq1)
    return "eval_seq1";
  if(cont == &vm::eval_seq2)
    return "eval_seq2";
  if(cont == &vm::eval_closure)
    return "eval_closure";
  if(cont == &vm::eval_closure1)
    return "eval_closure1";
  if(cont == &vm::eval0)
    return "eval0";
  if(cont == &vm::apply0)
    return "apply0";
  if(cont == &vm::everr)
    return "everr";
  if(cont == &vm::eval_lookup)
    return "eval_lookup";
  return "Unknown control stack element";
}

lisp_t vm::backtrace()
{
  for(int i = _toctrl - 1; i >= 0; i--)
  {
    primerr()->format("{}: ", i);
    std::visit(
      [this](auto&& arg) {
        using T = std::decay_t<decltype(arg)>;
        if constexpr(std::is_same_v<T, std::monostate>)
          ; // Do nothing for monostate
        else if constexpr(std::is_same_v<T, destblock_t*>)
          primerr()->format("destblock_t: {}\n", to_string(arg));
        else if constexpr(std::is_same_v<T, continuation_t>)
          primerr()->format("cont: {}\n", to_string(arg));
        else
          primerr()->format("{}\n", to_string(arg));
      },
      _control.at(i));
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
    auto* dest = &_destblock.at(_destblockused);
    _destblockused += size + 1;
    dest->num(static_cast<std::int8_t>(size));
    for(int i = 1; i <= size; ++i)
      _destblock.at(_destblockused - i).reset();
    return dest;
  }
  return nullptr;
}

void vm::free(destblock_t* block) { _destblockused -= block->size() + 1; }

lisp_t vm::break0(lisp_t exp) const { return repl(exp); }

} // namespace lisp

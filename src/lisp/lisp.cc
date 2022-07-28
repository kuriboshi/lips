//
// Lips, lisp shell.
// Copyright 2020-2022 Krister Joas
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

#include <cstring> // For strerror
#include <cerrno>  // For errno
#include <iostream>
#include "lisp.hh"
#include "syntax.hh"

namespace lisp
{
/// @brief Returns the lisp_t object to the list of free objects.
///
/// @details Instead of new/delete lisp_t objects are allocated from the list
/// of free objects. When an object is deleted it's returned to the list of
/// free lisp_t objects.
///
/// @param object A pointer to the lisp_t object to "delete".
void ref_deleter(lisp_t* object)
{
  // Set the type to FREE to make it possible to detect use of objects already
  // freed.
  object->set();
  object->settype(type::Free);
  // This will return the object to the memory pool.
  delete object; // NOLINT
}

namespace pn
{
inline constexpr auto E = "e";                   // noeval version of eval
inline constexpr auto EVAL = "eval";             // evaluate exp
inline constexpr auto APPLY = "apply";           // apply function on args
inline constexpr auto APPLYSTAR = "apply*";      // apply nospread
inline constexpr auto BAKTRACE = "baktrace";     // control stack backtrace
inline constexpr auto TOPOFSTACK = "topofstack"; // return top of value stack
inline constexpr auto DESTBLOCK = "destblock";   // convert environment to list
} // namespace pn

class lisp::impl
{
public:
  impl(evaluator& e)
    : _eval(e),
      _currentbase(initcvar("base", 10_l)),
      _verbose(initcvar("verbose", NIL)),
      _loadpath(initcvar("loadpath", mklist(mkstring("."))))
  {
    _syntax = std::make_unique<syntax>();

    _primout = new file_t(std::cout); // NOLINT
    _primerr = new file_t(std::cerr); // NOLINT
    _primin = new file_t(std::cin);   // NOLINT
    _stdout = new file_t(std::cout);  // NOLINT
    _stderr = new file_t(std::cerr);  // NOLINT
    _stdin = new file_t(std::cin);    // NOLINT
  }
  evaluator& _eval;
  std::unique_ptr<syntax> _syntax;

  ref_file_t _primout;
  ref_file_t _primerr;
  ref_file_t _primin;
  ref_file_t _stdout;
  ref_file_t _stderr;
  ref_file_t _stdin;

  cvariable_t& _currentbase;
  cvariable_t& _verbose;
  cvariable_t& _loadpath;

};

lisp::lisp()
  : _pimpl(std::make_unique<impl>(e()))
{
  if(_current == nullptr)
    _current = this;
  else
    throw std::runtime_error("lisp::lisp called twice ");

  static auto global_set = false;
  if(!global_set)
  {
    global_set = true;

    auto intern = [this](const auto s) { return alloc::intern(s); };

    // Must be early since it's used by symbol_store_t to initialize new
    // symbols.
    C_UNBOUND = intern("unbound");
    C_UNBOUND->symbol().constant = true;
    C_UNBOUND->set();
    C_UNBOUND->settype(type::Unbound);

    auto nil = intern("nil");
    nil->value(NIL);
    nil->symbol().constant = true;

    auto t = intern("t");
    T = t;
    t->value(T);
    t->settype(type::T);
    t->symbol().constant = true;

    C_EMPTY = intern("empty");
    C_EMPTY->symbol().constant = true;
    C_EMPTY->set(nullptr);

    C_AUTOLOAD = intern("autoload");
    C_BROKEN = intern("broken");
    C_BT = intern("bt");
    C_CLOSURE = intern("closure");
    //C_CONS = intern(pn::CONS);
    C_DOT = intern(".");
    C_ENDOFFILE = intern("endoffile");
    C_ENVIRON = intern("environ");
    C_EOF = intern("eof");
    C_EOF->settype(type::Eof);
    C_FILE = intern("file");
    C_FLOAT = intern("float");
    C_FREE = intern("free");
    C_FSUBR = intern("fsubr");
    C_GO = intern("go");
    C_INDIRECT = intern("indirect");
    C_INTEGER = intern("integer");
    C_OLDDEF = intern("olddef");
    C_REDEFINED = intern("redefined");
    C_RESET = intern("reset");
    C_RETURN = intern("return");
    C_STRING = intern("string");
    C_SUBR = intern("subr");
    C_SYMBOL = intern("symbol");
    C_READ = intern("read");
    C_WRITE = intern("write");
    C_APPEND = intern("append");

    C_VERSION = intern("version");
    C_VERSION->value(mkstring(VERSION));
    C_VERSION->symbol().constant = true;

    e().undefhook(nullptr);
    e().breakhook(nullptr);

    alloc::init();
    details::arith::init();
    details::debug::init();
    details::file::init();
    details::logic::init();
    details::low::init();
    details::map::init();
    details::pred::init();
    details::prim::init();
    details::prop::init();
    details::string::init();
    details::user::init();

    rtable::init();

    // clang-format off
    mkprim(pn::E,          eval,       subr_t::subr::NOEVAL, subr_t::spread::SPREAD);
    mkprim(pn::EVAL,       eval,       subr_t::subr::EVAL,   subr_t::spread::SPREAD);
    mkprim(pn::APPLY,      apply,      subr_t::subr::EVAL,   subr_t::spread::SPREAD);
    mkprim(pn::APPLYSTAR,  apply,      subr_t::subr::EVAL,   subr_t::spread::NOSPREAD);
    mkprim(pn::BAKTRACE,   baktrace,   subr_t::subr::EVAL,   subr_t::spread::SPREAD);
    mkprim(pn::TOPOFSTACK, topofstack, subr_t::subr::EVAL,   subr_t::spread::SPREAD);
    mkprim(pn::DESTBLOCK,  destblock,  subr_t::subr::EVAL,   subr_t::spread::SPREAD);
    // clang-format on
  }
}

lisp::~lisp()
{
  if(_current == this)
    _current = nullptr;
}

evaluator& lisp::e()
{
  static evaluator e_(*this);
  return e_;
}

lisp& lisp::current() { return *_current; }
void lisp::current(lisp& lisp) { _current = &lisp; }

syntax& lisp::read_table() { return *_pimpl->_syntax; }
void lisp::read_table(std::unique_ptr<syntax> syntax) { _pimpl->_syntax = std::move(syntax); }

LISPT syntax::macro(lisp& lisp, ref_file_t source, std::uint8_t index)
{
  auto fn = _macro[index];
  LISPT f{new lisp_t};
  f->set(source);
  if(fn != NIL)
    return apply(fn, cons(f, NIL));
  return NIL;
}

closure_t::pool_t closure_t::_pool;
lisp_t::pool_t lisp_t::_pool;

LISPT lisp::eval(lisp& l, LISPT expr) { return l.e().eval(expr); }
LISPT lisp::apply(lisp& l, LISPT fun, LISPT args) { return l.e().apply(fun, args); }
LISPT lisp::baktrace(lisp& l) { return l.e().baktrace(); }
LISPT lisp::topofstack(lisp& l) { return l.e().topofstack(); }
LISPT lisp::destblock(lisp& l, LISPT a) { return l.e().destblock(a); }

LISPT lisp::obarray(lisp& l) { return alloc::obarray(l); }
LISPT lisp::freecount(lisp& l) { return alloc::freecount(l); }

ref_file_t lisp::primout() const { return _pimpl->_primout; }

ref_file_t lisp::primerr() const { return _pimpl->_primerr; }

ref_file_t lisp::primin() const { return _pimpl->_primin; }

ref_file_t lisp::primout(ref_file_t f)
{
  auto p = std::move(_pimpl->_primout);
  _pimpl->_primout = std::move(f);
  return p;
}

ref_file_t lisp::primerr(ref_file_t f)
{
  auto p = std::move(_pimpl->_primerr);
  _pimpl->_primerr = std::move(f);
  return p;
}

ref_file_t lisp::primin(ref_file_t f)
{
  auto p = std::move(_pimpl->_primin);
  _pimpl->_primin = std::move(f);
  return p;
}

ref_file_t lisp::stdout() const { return _pimpl->_stdout; }

ref_file_t lisp::stderr() const { return _pimpl->_stderr; }

ref_file_t lisp::stdin() const { return _pimpl->_stdin; }

LISPT lisp::perror(std::error_code error, LISPT arg)
{
  primerr()->format("{} ", error.message());
  if(type_of(arg) != type::Empty)
    prin2(arg, T);
  return NIL;
}

LISPT lisp::error(std::error_code error, LISPT arg)
{
  perror(error, arg);
  throw lisp_error(error.message());
}

void lisp::fatal(std::error_code error) { throw lisp_error(error.message()); }

LISPT lisp::syserr(LISPT fault)
{
  if(!is_NIL(fault))
  {
    prin2(fault, T);
    primerr()->format(": ");
  }
  primerr()->format("{}", strerror(errno));
  return C_ERROR;
}

LISPT lisp::break0(LISPT exp) const { return repl(exp); }

cvariable_t& lisp::currentbase() { return _pimpl->_currentbase; }
cvariable_t& lisp::verbose() { return _pimpl->_verbose; }
cvariable_t& lisp::loadpath() { return _pimpl->_loadpath; }
void lisp::loadpath(LISPT newpath) { _pimpl->_loadpath = newpath; }

lisp* lisp::_current = nullptr;
std::unordered_map<std::string, subr_t::subr_index> subr_t::subr_map;
subr_t::subr_vector subr_t::subr_store;

LISPT eval(lisp& l, const std::string& expr)
{
  auto in = ref_file_t::create(expr);
  auto e = lispread(in);
  return lisp::eval(l, e);
}

LISPT eval(const std::string& expr) { return eval(lisp::current(), expr); }

//
// All lisp constants needed internally.
//
LISPT T;
LISPT C_EMPTY;
LISPT C_AUTOLOAD;
LISPT C_BROKEN;
LISPT C_BT;
LISPT C_CLOSURE;
LISPT C_CONS;
LISPT C_DOT;
LISPT C_ENDOFFILE;
LISPT C_ENVIRON;
LISPT C_EOF;
LISPT C_FILE;
LISPT C_FLOAT;
LISPT C_FREE;
LISPT C_FSUBR;
LISPT C_GO;
LISPT C_INDIRECT;
LISPT C_INTEGER;
LISPT C_OLDDEF;
LISPT C_REDEFINED;
LISPT C_RESET;
LISPT C_RETURN;
LISPT C_STRING;
LISPT C_SUBR;
LISPT C_SYMBOL;
LISPT C_UNBOUND;
LISPT C_READ;
LISPT C_WRITE;
LISPT C_APPEND;
LISPT C_VERSION;

} // namespace lisp

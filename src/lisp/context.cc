//
// Lips, lisp shell.
// Copyright 2022 Krister Joas
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
#include "arith.hh"
#include "context.hh"
#include "debug.hh"
#include "file.hh"
#include "logic.hh"
#include "low.hh"
#include "map.hh"
#include "pred.hh"
#include "prim.hh"
#include "prop.hh"
#include "rtable.hh"
#include "string.hh"
#include "syntax.hh"
#include "user.hh"
#include "version.hh"

#include <iostream>

namespace lisp
{
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

class context::impl
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

context::context()
{
  if(_current == nullptr)
    _current = this;
  else
    throw std::runtime_error("context::context called twice");

  _pimpl = std::make_unique<impl>(e());

  static auto global_set = false;
  if(!global_set)
  {
    global_set = true;

    auto intern = [this](const auto s) { return details::alloc::intern(s); };

    // Must be early since it's used by symbol_store_t to initialize new
    // symbols.
    C_UNBOUND = intern("unbound");
    C_UNBOUND->symbol().constant = true;
    C_UNBOUND->settype(type::Unbound);

    auto nil = intern("nil");
    nil->symbol().constant = true;

    auto t = intern("t");
    T = t;
    t->value(T);
    t->settype(type::T);
    t->symbol().constant = true;

    C_AUTOLOAD = intern("autoload");
    C_BROKEN = intern("broken");
    C_BT = intern("bt");
    C_CLOSURE = intern("closure");
    C_DOT = intern(".");
    C_ENDOFFILE = intern("endoffile");
    C_ENVIRON = intern("environ");
    C_EOF = intern("eof");
    C_EOF->settype(type::Eof);
    C_FILE = intern("file");
    C_FLOAT = intern("float");
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

    details::alloc::init();
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

context::~context()
{
  if(_current == this)
    _current = nullptr;
}

evaluator& context::e()
{
  static evaluator e_(*this);
  return e_;
}

context& context::current() { return *_current; }

syntax& context::read_table() { return *_pimpl->_syntax; }
void context::read_table(std::unique_ptr<syntax> syntax) { _pimpl->_syntax = std::move(syntax); }

LISPT context::eval(context& ctx, LISPT expr) { return ctx.e().eval(expr); }
LISPT context::apply(context& ctx, LISPT fun, LISPT args) { return ctx.e().apply(fun, args); }
LISPT context::baktrace(context& ctx) { return ctx.e().baktrace(); }
LISPT context::topofstack(context& ctx) { return ctx.e().topofstack(); }
LISPT context::destblock(context& ctx, LISPT a) { return ctx.e().destblock(a); }

ref_file_t context::primout() const { return _pimpl->_primout; }

ref_file_t context::primerr() const { return _pimpl->_primerr; }

ref_file_t context::primin() const { return _pimpl->_primin; }

ref_file_t context::primout(ref_file_t f)
{
  auto p = std::move(_pimpl->_primout);
  _pimpl->_primout = std::move(f);
  return p;
}

ref_file_t context::primerr(ref_file_t f)
{
  auto p = std::move(_pimpl->_primerr);
  _pimpl->_primerr = std::move(f);
  return p;
}

ref_file_t context::primin(ref_file_t f)
{
  auto p = std::move(_pimpl->_primin);
  _pimpl->_primin = std::move(f);
  return p;
}

ref_file_t context::stdout() const { return _pimpl->_stdout; }

ref_file_t context::stderr() const { return _pimpl->_stderr; }

ref_file_t context::stdin() const { return _pimpl->_stdin; }

LISPT context::perror(std::error_code error)
{
  primerr()->format("{} ", error.message());
  return NIL;
}

LISPT context::perror(std::error_code error, LISPT arg)
{
  primerr()->format("{} ", error.message());
  prin2(arg, T);
  return NIL;
}

LISPT context::error(std::error_code error, LISPT arg)
{
  perror(error, arg);
  throw lisp_error(error.message());
}

LISPT context::break0(LISPT exp) const { return repl(exp); }

cvariable_t& context::currentbase() { return _pimpl->_currentbase; }
cvariable_t& context::verbose() { return _pimpl->_verbose; }
cvariable_t& context::loadpath() { return _pimpl->_loadpath; }
void context::loadpath(LISPT newpath) { _pimpl->_loadpath = newpath; }

context* context::_current = nullptr;

}

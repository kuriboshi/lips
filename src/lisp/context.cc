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

class context::impl
{
public:
  impl(class vm& vm)
    : _vm(vm),
      _currentbase(initcvar("base", 10_l)),
      _verbose(initcvar("verbose", nil)),
      _loadpath(initcvar("loadpath", mklist(mkstring("."))))
  {
    _syntax = std::make_unique<syntax>();

    _primout = ref_file_t::create(std::cout); // NOLINT
    _primerr = ref_file_t::create(std::cerr); // NOLINT
    _primin = ref_file_t::create(std::cin);   // NOLINT
    _stdout = ref_file_t::create(std::cout);  // NOLINT
    _stderr = ref_file_t::create(std::cerr);  // NOLINT
    _stdin = ref_file_t::create(std::cin);    // NOLINT
  }
  class vm& _vm;
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

class init
{
public:
  init()
  {
    auto intern = [this](const auto s) { return details::alloc::intern(s); };

    // Must be early since it's used by symbol_store_t to initialize new
    // symbols.
    C_UNBOUND = intern("unbound");
    C_UNBOUND->symbol()->constant = true;
    C_UNBOUND->settype(type::Unbound);

    auto nil = intern("nil");
    nil->value(nil);
    nil->symbol()->constant = true;

    auto t = intern("t");
    T = t;
    t->value(T);
    t->symbol()->constant = true;

    C_AUTOLOAD = intern("autoload");
    C_BROKEN = intern("broken");
    C_BT = intern("bt");
    C_CLOSURE = intern("closure");
    C_CONS = intern("cons");
    C_DOT = intern(".");
    C_ENDOFFILE = intern("endoffile");
    C_ENVIRON = intern("environ");
    C_EOF = intern("eof");
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
    C_CVARIABLE = intern("cvariable");

    C_VERSION = intern("version");
    C_VERSION->value(mkstring(VERSION));
    C_VERSION->symbol()->constant = true;

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
    details::vm::init();

    rtable::init();
  }
};

context::context()
{
  if(_current == nullptr)
    _current = this;
  else
    throw std::runtime_error("context::context called twice");

  _pimpl = std::make_unique<impl>(vm());

  static init init;
}

context::~context() = default;

class vm& context::vm()
{
  static class vm vm_(*this);
  return vm_;
}

context& context::current() { return *_current; }

syntax& context::read_table() { return *_pimpl->_syntax; }
void context::read_table(std::unique_ptr<syntax> syntax) { _pimpl->_syntax = std::move(syntax); }

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

lisp_t context::perror(std::error_code error) const
{
  primerr()->format("{} ", error.message());
  return nil;
}

lisp_t context::perror(std::error_code error, lisp_t arg) const
{
  primerr()->format("{} ", error.message());
  prin2(arg, T);
  return nil;
}

lisp_t context::error(std::error_code error, lisp_t arg) const
{
  perror(error, arg);
  throw lisp_error(error.message());
}

lisp_t context::break0(lisp_t exp) const { return repl(exp); }

cvariable_t& context::currentbase() { return _pimpl->_currentbase; }
cvariable_t& context::verbose() { return _pimpl->_verbose; }
cvariable_t& context::loadpath() { return _pimpl->_loadpath; }
void context::loadpath(lisp_t newpath) { _pimpl->_loadpath = newpath; }

context* context::_current = nullptr;

} // namespace lisp

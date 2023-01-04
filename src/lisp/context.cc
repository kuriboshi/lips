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

#include "context.hh"
#include "alloc.hh"
#include "file.hh"
#include "syntax.hh"

#include <iostream>

namespace lisp
{

class context::impl
{
public:
  impl()
    : _currentbase(initcvar("base", 10_l)),
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

  _pimpl = std::make_unique<impl>();
}

context::~context()
{
  _current = nullptr;
}

context& context::current()
{
  if(_current == nullptr)
    throw std::runtime_error("lisp::context has not been created");
  return *_current;
}

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

cvariable_t& context::currentbase() { return _pimpl->_currentbase; }
cvariable_t& context::verbose() { return _pimpl->_verbose; }
cvariable_t& context::loadpath() { return _pimpl->_loadpath; }
void context::loadpath(lisp_t newpath) { _pimpl->_loadpath = newpath; }

context* context::_current = nullptr;

} // namespace lisp

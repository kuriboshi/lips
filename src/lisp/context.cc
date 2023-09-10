//
// Lips, lisp shell.
// Copyright 2020-2023 Krister Joas
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

#include <iostream>

#include "alloc.hh"
#include "file.hh"
#include "syntax.hh"

namespace lisp
{

class context_t::impl final
{
public:
  impl()
    : _currentbase(makecvar("base", mknumber(default_base))),
      _verbose(makecvar("verbose", nil)),
      _loadpath(makecvar("loadpath", mklist(mkstring("."))))
  {
    _syntax = std::make_unique<syntax>();

    _primout = ref_file_t::create(std::cout);
    _primerr = ref_file_t::create(std::cerr);
    _primin = ref_file_t::create(std::cin);
    _stdout = ref_file_t::create(std::cout);
    _stderr = ref_file_t::create(std::cerr);
    _stdin = ref_file_t::create(std::cin);
  }
  std::unique_ptr<syntax> _syntax;

  ref_file_t _primout;
  ref_file_t _primerr;
  ref_file_t _primin;
  ref_file_t _stdout;
  ref_file_t _stderr;
  ref_file_t _stdin;

  lisp_t _currentbase;
  lisp_t _verbose;
  lisp_t _loadpath;
};

context_t::context_t()
{
  _pimpl = std::make_unique<impl>();
}

context_t::~context_t() = default;

syntax& context_t::read_table() { return *_pimpl->_syntax; }
void context_t::read_table(std::unique_ptr<syntax> syntax) { _pimpl->_syntax = std::move(syntax); }

ref_file_t context_t::primout() const { return _pimpl->_primout; }

ref_file_t context_t::primerr() const { return _pimpl->_primerr; }

ref_file_t context_t::primin() const { return _pimpl->_primin; }

ref_file_t context_t::primout(ref_file_t f)
{
  auto p = std::move(_pimpl->_primout);
  _pimpl->_primout = std::move(f);
  return p;
}

ref_file_t context_t::primerr(ref_file_t f)
{
  auto p = std::move(_pimpl->_primerr);
  _pimpl->_primerr = std::move(f);
  return p;
}

ref_file_t context_t::primin(ref_file_t f)
{
  auto p = std::move(_pimpl->_primin);
  _pimpl->_primin = std::move(f);
  return p;
}

ref_file_t context_t::stdout() const { return _pimpl->_stdout; }

ref_file_t context_t::stderr() const { return _pimpl->_stderr; }

ref_file_t context_t::stdin() const { return _pimpl->_stdin; }

lisp_t context_t::perror(std::error_code error) const
{
  _pimpl->_primerr->format("{} ", error.message());
  return nil;
}

lisp_t context_t::perror(std::error_code error, lisp_t arg) const
{
  _pimpl->_primerr->format("{} ", error.message());
  prin2(arg, T);
  return nil;
}

lisp_t context_t::error(std::error_code error, lisp_t arg) const
{
  perror(error, arg);
  throw lisp_error(error);
}

const cvariable_t& context_t::currentbase() const { return _pimpl->_currentbase->cvariable(); }
const cvariable_t& context_t::verbose() const { return _pimpl->_verbose->cvariable(); }
const cvariable_t& context_t::loadpath() const { return _pimpl->_loadpath->cvariable(); }
void context_t::loadpath(lisp_t newpath) { _pimpl->_loadpath->cvariable() = newpath; }

} // namespace lisp

//
// Lips, lisp shell.
// Copyright 1989, 2020-2025 Krister Joas
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

#pragma once

#include <iostream>
#include <system_error>
#include <memory>

#include "ref_ptr.hh"
#include "syntax.hh"
#include "types.hh"

// The C standard says these are macros and depending on the implementation
// they may be defined as something we can't use as identifiers. Undefine them
// since we don't need the original definitions.
#undef stdin
#undef stdout
#undef stderr

namespace lisp
{

class context_t final
{
public:
  context_t();
  ~context_t();

  context_t(const context_t&) = delete;
  context_t(context_t&&) = delete;
  context_t& operator=(const context_t&) = delete;
  context_t& operator=(context_t&&) = delete;

  syntax& read_table();
  void read_table(std::unique_ptr<syntax>);

  ref_file_t primout() const;
  ref_file_t primerr() const;
  ref_file_t primin() const;
  ref_file_t primout(ref_file_t);
  ref_file_t primerr(ref_file_t);
  ref_file_t primin(ref_file_t);
  ref_file_t stdout() const;
  ref_file_t stderr() const;
  ref_file_t stdin() const;

  // Used by lisp::io
  integer_t::value_type printlevel() const { return _printlevel; }
  void printlevel(integer_t::value_type pl) { _printlevel = pl; }

  const cvariable_t& currentbase() const;
  const cvariable_t& verbose() const;
  const cvariable_t& loadpath() const;
  void loadpath(lisp_t);

  lisp_t perror(std::error_code) const;
  lisp_t perror(std::error_code, const lisp_t&) const;
  lisp_t error(std::error_code, const lisp_t&) const;
  static lisp_t fatal(std::error_code error) { throw lisp_error(error); }
  static lisp_t fatal(std::error_code error, const std::string& a) { throw lisp_error(error, a); }

private:
  class impl;
  std::unique_ptr<impl> _pimpl;
  integer_t::value_type _printlevel{0};

  static const constexpr integer_t::value_type default_base{10};
};

} // namespace lisp

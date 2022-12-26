//
// Lips, lisp shell.
// Copyright 1989, 2020-2022 Krister Joas
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

#ifndef LISP_CONTEXT_HH
#define LISP_CONTEXT_HH

#include <system_error>
#include <memory>

#include "except.hh"
#include "ref_ptr.hh"
#include "types.hh"

namespace lisp
{

class syntax;
class context;
class vm;
class cvariable_t;
class file_t;
using ref_file_t = ref_ptr<file_t>;
class object;
using lisp_t = ref_ptr<object>;

extern lisp_t C_VERSION;

class context
{
public:
  context();
  ~context();
  class vm& vm();
  static context& current();

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

  lisp_t perror(std::error_code) const;
  lisp_t perror(std::error_code, lisp_t) const;
  lisp_t error(std::error_code, lisp_t) const;

  void fatal(std::error_code error) { throw lisp_error(error.message()); }

  template<typename... Ts>
  void fatal(std::error_code error, const Ts&... args)
  {
    throw lisp_error(error.message() + ": " + cat(args...));
  }

  lisp_t break0(lisp_t) const;

  enum class break_return
  {
    RETURN,  // Return from recursive repl
    PROCEED, // Proceed with repl
    SKIP,    // Skip eval
  };
  using repl_fun_t = std::function<lisp_t(lisp_t)>;
  repl_fun_t repl;

  // Used by lisp::io
  int printlevel = 0;
  int thisplevel = 0;
  bool echoline = false;

  // Used by the interpreter
  bool brkflg = false;
  bool interrupt = false;

  cvariable_t& currentbase();
  cvariable_t& verbose();
  cvariable_t& loadpath();
  void loadpath(lisp_t);
  std::string version() const { return C_VERSION->value()->getstr(); }

private:
  template<typename T>
  std::string cat(const T& arg)
  {
    std::ostringstream os;
    os << arg;
    return os.str();
  }

  template<typename T, typename... Ts>
  std::string cat(const T& first, const Ts&... args)
  {
    std::ostringstream os;
    os << first << " " << cat(args...);
    return os.str();
  }

  class impl;
  std::unique_ptr<impl> _pimpl;
  static context* _current;
};

inline lisp_t perror(std::error_code code, lisp_t a) { return context::current().perror(code, a); }
inline lisp_t error(std::error_code code, lisp_t a) { return context::current().error(code, a); }
inline lisp_t break0(lisp_t a) { return context::current().break0(a); }

} // namespace lisp

#endif

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

#ifndef LISP_ERROR_HH
#define LISP_ERROR_HH

#include <string>
#include <system_error>

#include "util.hh"

namespace lisp
{
enum class type_errc
{
  not_nil = 1,
  not_t,
  not_empty,
  not_symbol,
  not_integer,
  not_float,
  not_indirect,
  not_cons,
  not_string,
  not_subr,
  not_lambda,
  not_closure,
  not_unbound,
  not_environ,
  not_filet,
  not_free,
  not_endoffile,
  not_error,
  not_cvariable
};

class type_category: public std::error_category
{
public:
  const char* name() const noexcept override { return "type"; }
  std::string message(int condition) const override
  {
    switch(static_cast<type_errc>(condition))
    {
      case type_errc::not_nil:
        return "Not NIL";
      case type_errc::not_t:
        return "Not T";
      case type_errc::not_empty:
        return "Not empty";
      case type_errc::not_symbol:
        return "Not a symbol";
      case type_errc::not_integer:
        return "Not an integer";
      case type_errc::not_float:
        return "Not a float";
      case type_errc::not_indirect:
        return "Not indirect";
      case type_errc::not_cons:
        return "Not a cons cell";
      case type_errc::not_string:
        return "Not a string";
      case type_errc::not_subr:
        return "Not SUBR";
      case type_errc::not_lambda:
        return "Not LAMBDA";
      case type_errc::not_closure:
        return "Not a closure";
      case type_errc::not_unbound:
        return "Not unbound";
      case type_errc::not_environ:
        return "Not an environment";
      case type_errc::not_filet:
        return "Not a file pointer";
      case type_errc::not_free:
        return "Not free";
      case type_errc::not_endoffile:
        return "Not EOF";
      case type_errc::not_error:
        return "Not an ERROR";
      case type_errc::not_cvariable:
        return "Not a c-variable";
    }
    return "";
  }
  static const std::error_category& category()
  {
    static type_category instance;
    return instance;
  }
};

inline std::error_code make_error_code(type_errc e)
{
  return std::error_code(to_underlying(e), type_category::category());
}

inline std::error_condition make_error_condition(type_errc e)
{
  return std::error_condition(to_underlying(e), type_category::category());
}

enum class error_errc
{
  attempt_to_clobber = 1,
  cant_load,
  cant_open,
  corrupt_data,
  divide_by_zero,
  illegal_arg,
  illegal_function,
  kbd_break,
  no_message,
  stack_overflow,
  unbound_variable,
  undef_function,
  unknown_request,
  user_error
};

class error_category: public std::error_category
{
public:
  const char* name() const noexcept override { return "error"; }
  std::string message(int condition) const override
  {
    switch(static_cast<error_errc>(condition))
    {
      case error_errc::attempt_to_clobber:
        return "Attempt to clobber constant";
      case error_errc::cant_load:
        return "Can't load file";
      case error_errc::cant_open:
        return "Can't open file";
      case error_errc::corrupt_data:
        return "Bug: corrupt data";
      case error_errc::divide_by_zero:
        return "Divide by zero";
      case error_errc::illegal_arg:
        return "Illegal argument";
      case error_errc::illegal_function:
        return "Illegal function";
      case error_errc::kbd_break:
        return "Break";
      case error_errc::no_message:
        return "No message";
      case error_errc::stack_overflow:
        return "Stack overflow";
      case error_errc::unbound_variable:
        return "Unbound variable";
      case error_errc::undef_function:
        return "Undefined function";
      case error_errc::unknown_request:
        return "Unknown request";
      case error_errc::user_error:
        return "User error";
    }
    return "";
  }
  static const std::error_category& category()
  {
    static error_category instance;
    return instance;
  }
};

inline std::error_code make_error_code(error_errc e)
{
  return std::error_code(to_underlying(e), error_category::category());
}

inline std::error_condition make_error_condition(error_errc e)
{
  return std::error_condition(to_underlying(e), error_category::category());
}

} // namespace lisp

namespace std
{
template<>
struct is_error_code_enum<lisp::type_errc>: public true_type
{};

template<>
struct is_error_code_enum<lisp::error_errc>: public true_type
{};
} // namespace std

#endif

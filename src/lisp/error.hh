//
// Lips, lisp shell.
// Copyright 1989, 2020-2023, 2025 Krister Joas
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

#include <string>
#include <system_error>

#include "util.hh"

namespace lisp
{
/// @brief Errors used when doing type checking.
enum class type_errc
{
  not_nil = 1,
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
  not_cvariable
};

/// @brief Error category for type checking.
class type_category final: public std::error_category
{
public:
  /// @brief Returns the constant string "type".
  const char* name() const noexcept override { return "type"; }
  /// @brief Translate error code to error message.
  std::string message(int condition) const override
  {
    switch(static_cast<type_errc>(condition))
    {
      case type_errc::not_nil:
        break;
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
      case type_errc::not_cvariable:
        return "Not a c-variable";
    }
    return "Not nil";
  }
  /// @brief Returns the category.
  static const std::error_category& category()
  {
    static const type_category instance;
    return instance;
  }
};

/// @brief Creates an std::error_code from a type_errc value.
inline std::error_code make_error_code(type_errc e) { return {to_underlying(e), type_category::category()}; }

/// @brief Creates an std::error_condition from a type_errc value.
inline std::error_condition make_error_condition(type_errc e) { return {to_underlying(e), type_category::category()}; }

/// @brief General lisp error codes.
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
  redefinition_of_subr,
  unknown_request,
  no_source,
  no_sink,
  abort,
  reset,
  user_error
};

/// @brief General lisp error category.
class error_category final: public std::error_category
{
public:
  /// @brief Returns the constant string "error".
  const char* name() const noexcept override { return "error"; }
  /// @brief Translate error code to error message.
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
      case error_errc::redefinition_of_subr:
        return "Redefinition of subr not allowed";
      case error_errc::unknown_request:
        return "Unknown request";
      case error_errc::no_source:
        return "No source";
      case error_errc::no_sink:
        return "No sink";
      case error_errc::abort:
        return "Abort";
      case error_errc::reset:
        return "Reset";
      case error_errc::user_error:
        break;
    }
    return "User error";
  }
  /// @brief Returns the category.
  static const std::error_category& category()
  {
    static const error_category instance;
    return instance;
  }
};

/// @brief Creates an std::error_code from a error_errc value.
inline std::error_code make_error_code(error_errc e) { return {to_underlying(e), error_category::category()}; }

/// @brief Creates an std::error_condition from a error_errc value.
inline std::error_condition make_error_condition(error_errc e)
{
  return {to_underlying(e), error_category::category()};
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

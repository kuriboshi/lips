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

#include <catch2/catch.hpp>

#include "error.hh"

namespace lisp
{

TEST_CASE("error: type error messages")
{
  CHECK(make_error_code(type_errc::not_nil).message() == "Not NIL");
  CHECK(make_error_code(type_errc::not_t).message() == "Not T");
  CHECK(make_error_code(type_errc::not_empty).message() == "Not empty");
  CHECK(make_error_code(type_errc::not_symbol).message() == "Not a symbol");
  CHECK(make_error_code(type_errc::not_integer).message() == "Not an integer");
  CHECK(make_error_code(type_errc::not_float).message() == "Not a float");
  CHECK(make_error_code(type_errc::not_indirect).message() == "Not indirect");
  CHECK(make_error_code(type_errc::not_cons).message() == "Not a cons cell");
  CHECK(make_error_code(type_errc::not_string).message() == "Not a string");
  CHECK(make_error_code(type_errc::not_subr).message() == "Not SUBR");
  CHECK(make_error_code(type_errc::not_lambda).message() == "Not LAMBDA");
  CHECK(make_error_code(type_errc::not_closure).message() == "Not a closure");
  CHECK(make_error_code(type_errc::not_unbound).message() == "Not unbound");
  CHECK(make_error_code(type_errc::not_environ).message() == "Not an environment");
  CHECK(make_error_code(type_errc::not_filet).message() == "Not a file pointer");
  CHECK(make_error_code(type_errc::not_free).message() == "Not free");
  CHECK(make_error_code(type_errc::not_endoffile).message() == "Not EOF");
  CHECK(make_error_code(type_errc::not_error).message() == "Not an ERROR");
  CHECK(make_error_code(type_errc::not_cvariable).message() == "Not a c-variable");
}

TEST_CASE("error: error messages")
{
  CHECK(make_error_code(error_errc::attempt_to_clobber).message() == "Attempt to clobber constant");
  CHECK(make_error_code(error_errc::cant_load).message() == "Can't load file");
  CHECK(make_error_code(error_errc::cant_open).message() == "Can't open file");
  CHECK(make_error_code(error_errc::corrupt_data).message() == "Bug: corrupt data");
  CHECK(make_error_code(error_errc::divide_by_zero).message() == "Divide by zero");
  CHECK(make_error_code(error_errc::illegal_arg).message() == "Illegal argument");
  CHECK(make_error_code(error_errc::illegal_function).message() == "Illegal function");
  CHECK(make_error_code(error_errc::kbd_break).message() == "Break");
  CHECK(make_error_code(error_errc::no_message).message() == "No message");
  CHECK(make_error_code(error_errc::stack_overflow).message() == "Stack overflow");
  CHECK(make_error_code(error_errc::unbound_variable).message() == "Unbound variable");
  CHECK(make_error_code(error_errc::undef_function).message() == "Undefined function");
  CHECK(make_error_code(error_errc::unknown_request).message() == "Unknown request");
  CHECK(make_error_code(error_errc::user_error).message() == "User error");
}

TEST_CASE("error: type error name")
{
  CHECK(type_category::category().name() == std::string("type"));
  CHECK(error_category::category().name() == std::string("error"));
}

TEST_CASE("error: error_condition")
{
  CHECK(make_error_condition(error_errc::no_message));
  CHECK(make_error_condition(type_errc::not_nil));
}

} // namespace lisp

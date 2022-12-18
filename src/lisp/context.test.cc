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

#include "context.hh"
#include "io.hh"
#include "syntax.hh"

#include <catch2/catch.hpp>

namespace lisp
{

TEST_CASE("context: fatal error")
{
  CHECK_THROWS(context::current().fatal(error_errc::user_error, "user", "error"));
  CHECK_THROWS(context::current().fatal(error_errc::user_error));
  CHECK(perror(error_errc::user_error, NIL) == NIL);
}

TEST_CASE("context: stdin, stdout, stderr")
{
  CHECK(context::current().stdin() != nullptr);
  CHECK(context::current().stdout() != nullptr);
  CHECK(context::current().stderr() != nullptr);
}

TEST_CASE("context: syntax")
{
  context::current().read_table(std::make_unique<syntax>());
}

}

//
// Lips, lisp shell.
// Copyright 2020-2024 Krister Joas
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

#include "io.hh"
#include "syntax.hh"
#include "vm.hh"

#include <catch2/catch_test_macros.hpp>

namespace lisp
{

TEST_CASE("context: fatal error")
{
  CHECK_THROWS(vm::fatal(error_errc::user_error, "user", "error"));
  CHECK_THROWS(vm::fatal(error_errc::user_error));
  CHECK_THROWS(fatal(error_errc::user_error, "user", "error"));
  CHECK_THROWS(fatal(error_errc::user_error));
  CHECK(perror(error_errc::user_error, nil) == nil);
}

TEST_CASE("context: stdin, stdout, stderr")
{
  CHECK(vm::stdin() != nullptr);
  CHECK(vm::stdout() != nullptr);
  CHECK(vm::stderr() != nullptr);
}

TEST_CASE("context: syntax") { vm::read_table(std::make_unique<syntax>()); }

} // namespace lisp

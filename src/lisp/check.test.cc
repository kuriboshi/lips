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

#include <catch2/catch_test_macros.hpp>

#include "alloc.hh"
#include "check.hh"
#include "io.hh"

namespace lisp
{

TEST_CASE("check: type checking")
{
  CHECK_THROWS(check(T, type::Nil));
  CHECK_THROWS(check(nil, type::Symbol));
  CHECK_THROWS(check(nil, type::Integer));
  CHECK_THROWS(check(nil, type::Float));
  CHECK_THROWS(check(nil, type::Indirect));
  CHECK_THROWS(check(nil, type::Cons));
  CHECK_THROWS(check(nil, type::String));
  CHECK_THROWS(check(nil, type::Subr));
  CHECK_THROWS(check(nil, type::Lambda));
  CHECK_THROWS(check(nil, type::Closure));
  CHECK_THROWS(check(nil, type::Unbound));
  CHECK_THROWS(check(nil, type::Environ));
  CHECK_THROWS(check(nil, type::File));
  CHECK_THROWS(check(nil, type::Cvariable));
}

} // namespace lisp

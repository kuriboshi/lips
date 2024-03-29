//
// Lips, lisp shell.
// Copyright 2021-2023 Krister Joas
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
#include "debug.hh"
#include "vm.hh"

namespace lisp
{

TEST_CASE("debug: debug functions")
{
  auto t = vm::get().trace();
  CHECK(!t);
  evaltrace(T);
  t = vm::get().trace();
  CHECK(t);
  evaltrace(nil);
  t = vm::get().trace();
  CHECK(!t);
  auto r = evaltrace(T);
  CHECK(type_of(r) == object::type::Nil);
  r = evaltrace(nil);
  CHECK(is_T(r));
}

} // namespace lisp

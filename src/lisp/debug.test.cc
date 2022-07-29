//
// Lips, lisp shell.
// Copyright 2021-2022 Krister Joas
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

#include "alloc.hh"
#include "debug.hh"
#include "eval.hh"

namespace lisp
{

TEST_CASE("Debug functions")
{
  auto& ctx = context::current();
  auto t = ctx.e().trace();
  CHECK(t == 0);
  evaltrace(mknumber(1));
  t = ctx.e().trace();
  CHECK(t == 1);
  evaltrace(mknumber(0));
  t = ctx.e().trace();
  CHECK(t == 0);
}

} // namespace lisp

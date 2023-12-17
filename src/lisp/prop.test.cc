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
#include "prim.hh"
#include "prop.hh"

namespace lisp
{

TEST_CASE("prop: property functions")
{
  auto sym = mkatom("sym");
  auto prop0 = mkatom("prop0");
  auto value0 = mkstring("value0");

  CHECK(putprop(sym, prop0, value0) == value0);
  auto p0 = getprop(sym, prop0);
  CHECK(p0 == value0);

  auto prop1 = mkatom("prop1");
  auto value1 = mkstring("value1");
  CHECK(putprop(sym, prop1, value1) == value1);
  auto plist = getplist(sym);
  CHECK(length(plist)->as_integer() == 4);

  CHECK(putprop(sym, prop1, value0) == value0);
  plist = getplist(sym);
  CHECK(length(plist)->as_integer() == 4);

  CHECK(remprop(sym, prop0) == value0);
  // plist is changed in place
  CHECK(length(plist)->as_integer() == 2);
  CHECK(length(getplist(sym))->as_integer() == 2);

  setplist(sym, mklist(prop0, value0, prop1, value1));
  CHECK(length(getplist(sym))->as_integer() == 4);
  CHECK(getprop(sym, prop1) == value1);
  CHECK(getprop(sym, "prop2"_a) == nil);
  CHECK_THROWS(getprop(1_l, "prop3"_a));
}

} // namespace lisp

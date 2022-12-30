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

#include <iostream>

#include <catch2/catch_test_macros.hpp>

#include "io.hh"
#include "types.hh"

namespace lisp::symbol
{

TEST_CASE("symbol: new symbol store")
{
  auto* sym0 = symbol_t::intern("hello");
  CHECK(sym0->pname == "hello");
  CHECK(sym0->value == C_UNBOUND);
  auto* sym1 = symbol_t::intern("hello");
  CHECK(sym0 == sym1);

  CHECK(symbol_t::exists("hello"));
  CHECK(!symbol_t::exists("world0"));
}

TEST_CASE("symbol: print sizes")
{
#ifdef ENABLE_OBJECT_SIZES
  std::cout << "==========\n";
  std::cout << "sizeof symbol_t: " << sizeof(symbol_t) << std::endl;
#endif
}

template<class T>
void pool_test()
{
  new T(pool_test_t());
}

TEST_CASE("symbol: pool")
{
  auto c0 = symbol_t::freecount();
  CHECK_THROWS(pool_test<symbol_t>());
  CHECK(c0 == symbol_t::freecount());
}

TEST_CASE("symbol: unintern")
{
  auto* sym = symbol_t::intern("xxx");
  CHECK(symbol_t::exists("xxx"));
  symbol_t::unintern("xxx");
  CHECK(!symbol_t::exists("xxx"));
}

} // namespace lisp::symbol

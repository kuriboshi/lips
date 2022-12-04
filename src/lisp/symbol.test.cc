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

#include <catch2/catch.hpp>

#include "io.hh"
#include "types.hh"

namespace lisp::symbol
{

TEST_CASE("symbol: new symbol store")
{
  symbol_collection& all_symbols = lisp_t::symbol_collection();
  auto& syms = all_symbols.create();

  auto& sym0 = syms.get("hello");
  CHECK(sym0.pname == "hello");
  CHECK(sym0.value == C_UNBOUND);
  auto& sym1 = syms.get("hello");
  CHECK(&sym0 == &sym1);
  auto& sym2 = syms.get(sym0.id.index);
  CHECK(sym2.pname == "hello");
  CHECK(&sym0 == &sym2);

  CHECK(all_symbols.exists(0, "hello"));
  CHECK(!all_symbols.exists(0, "world0"));

  CHECK_THROWS(all_symbols.exists(1000, "hello"));
  CHECK_THROWS(all_symbols.get(1000, "hello"));
  symbol_id pname{1000, 0};
  CHECK_THROWS(all_symbols.get(pname));
  CHECK_THROWS(all_symbols.get(1000, 99));

  SECTION("symbol: symbol_collection")
  {
    symbol_collection collection;
    CHECK_THROWS(collection.symbol_store(2));
    auto& sym0 = collection.get(0, "hello");
    auto& sym1 = collection.get(0, sym0.id.index);
  }

  SECTION("symbol: symbol_store_t")
  {
    symbol_store_t store0{100};
    auto store1 = std::move(store0);
    store0 = std::move(store1);
  }
}

TEST_CASE("symbol: print sizes")
{
#ifdef ENABLE_OBJECT_SIZES
  std::cout << "==========\n";
  std::cout << "sizeof symbol_id: " << sizeof(symbol_id) << std::endl;
  std::cout << "sizeof store_t: " << sizeof(store_t) << std::endl;
  std::cout << "sizeof symbol_store_t: " << sizeof(symbol_store_t) << std::endl;
  std::cout << "sizeof symbol_index: " << sizeof(symbol_index) << std::endl;
  std::cout << "sizeof symbol_t: " << sizeof(symbol_t) << std::endl;
#endif
}

} // namespace lisp::symbol

#include "lisp.hh"
#include "alloc.hh"
#include "io.hh"
#include "symbol.hh"
#include <iostream>
#include <doctest/doctest.h>

namespace lisp::symbol
{

TEST_CASE("New symbol store")
{
  symbol_collection all_symbols;
  auto id = all_symbols.create();
  std::cout << "==========\n";
  std::cout << "sizeof print_name: " << sizeof(print_name) << std::endl;
  std::cout << "sizeof store_t: " << sizeof(store_t) << std::endl;
  std::cout << "sizeof symbol_store_t: " << sizeof(symbol_store_t) << std::endl;
  std::cout << "sizeof symbol_index: " << sizeof(symbol_index) << std::endl;
  std::cout << "sizeof symbol_t: " << sizeof(symbol_t) << std::endl;

  auto& sym0 = all_symbols.get(id, "hello");
  CHECK(sym0.pname.name == "hello");
  CHECK(sym0.value == C_UNBOUND);
  auto& sym1 = all_symbols.get(id, "hello");
  CHECK(&sym0 == &sym1);
  auto& sym2 = all_symbols.get(id, sym0.pname.index);
  CHECK(sym2.pname.name == "hello");
  CHECK(&sym0 == &sym2);
  auto& sym3 = all_symbols.get(sym0.pname);
  CHECK(sym3.pname.name == "hello");
  CHECK(&sym0 == &sym3);

  CHECK(all_symbols.exists(id, "hello"));
  CHECK(!all_symbols.exists(id, "world"));

  CHECK_THROWS(all_symbols.exists(1000, "hello"));
  CHECK_THROWS(all_symbols.get(1000, "hello"));
  print_name pname{1000, 0, "hello"};
  CHECK_THROWS(all_symbols.get(pname));
  CHECK_THROWS(all_symbols.get(1000, 99));
}

}

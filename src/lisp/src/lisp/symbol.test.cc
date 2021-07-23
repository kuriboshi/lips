#include <iostream>
#include <doctest/doctest.h>

#include "alloc.hh"
#include "lisp.hh"
#include "io.hh"
#include "symbol.hh"

namespace lisp::symbol
{

TEST_CASE("New symbol store")
{
  symbol_collection& all_symbols = lisp_t::symbol_collection();
  auto& syms = all_symbols.create();
  std::cout << "==========\n";
  std::cout << "sizeof print_name: " << sizeof(print_name) << std::endl;
  std::cout << "sizeof store_t: " << sizeof(store_t) << std::endl;
  std::cout << "sizeof symbol_store_t: " << sizeof(symbol_store_t) << std::endl;
  std::cout << "sizeof symbol_index: " << sizeof(symbol_index) << std::endl;
  std::cout << "sizeof symbol_t: " << sizeof(symbol_t) << std::endl;

  auto& sym0 = syms.get("hello");
  CHECK(sym0.pname.name == "hello");
  CHECK(sym0.value == C_UNBOUND);
  auto& sym1 = syms.get("hello");
  CHECK(&sym0 == &sym1);
  auto& sym2 = syms.get(sym0.pname.index);
  CHECK(sym2.pname.name == "hello");
  CHECK(&sym0 == &sym2);

  CHECK(all_symbols.exists(0, "hello"));
  CHECK(!all_symbols.exists(0, "world"));

  CHECK_THROWS(all_symbols.exists(1000, "hello"));
  CHECK_THROWS(all_symbols.get(1000, "hello"));
  print_name pname{1000, 0, "hello"};
  CHECK_THROWS(all_symbols.get(pname));
  CHECK_THROWS(all_symbols.get(1000, 99));
}

}

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
#include <memory>

#include <catch2/catch.hpp>

#include "alloc.hh"
#include "low.hh"
#include "prim.hh"

namespace lisp
{

TEST_CASE("alloc: create lisp objects")
{
  SECTION("Multiple calls to intern should return the same object for the same string")
  {
    auto hello0 = intern("hello");
    auto hello1 = intern("hello");
    CHECK(hello0 == hello1);
  }

  SECTION("Check constants are the same as interned strings")
  {
    auto lambda = intern("lambda");
    CHECK(lambda == C_LAMBDA);
  }

  SECTION("Check constants are the same as a local atom")
  {
    auto lambda = mkatom("lambda");
    CHECK(lambda == C_LAMBDA);
  }

  SECTION("Set variable")
  {
    auto i = mkatom("i");
    auto j = mkatom("j");
    auto a = mkstring("a");
    auto b = mkstring("b");

    set(i, a);
    set(j, b);
    CHECK(i != j);
    set(j, a);
    CHECK(i != j);

    file_t out0(std::make_unique<io::string_sink>());
    prin0(i, out0);
    CHECK(to_string(out0.sink()) == std::string(i->getstr()));

    file_t out1(std::make_unique<io::string_sink>());
    prin0(j, out1);
    CHECK(to_string(out1.sink()) == std::string(j->getstr()));

    std::string s_hello{"(hello)"};
    auto in = ref_file_t::create(s_hello);
    auto hello = lispread(in);
    file_t out2(std::make_unique<io::string_sink>());
    prin0(hello, out2);
    CHECK(to_string(out2.sink()) == s_hello);
  }
}

TEST_CASE("alloc: c variables")
{
  auto& cvar = initcvar("cvar", 123_l);
  auto a = eval(cvar);
  CHECK(eq(cvar, 123_l));
  cvar = 321_l;
  CHECK(eq(cvar, 321_l));

  auto r0 = eval(cvar);
  CHECK(r0->intval() == 321);

  auto r1 = eval("(setq cvar 444)");
  CHECK(r1->intval() == 444);
  CHECK(cvar->intval() == 444);

  auto r2 = eval("cvar");
  CHECK(r2->intval() == 444);

  cvar = mkstring("hello");
  CHECK(cvar->getstr() == "hello");

  auto& xvar = initcvar("xvar", "hello"_l);
  eval("(setq xvar \"world\")");
  CHECK(xvar->getstr() == "world");

  auto& yvar = initcvar("yvar", 0_l);
  eval("(setq yvar \"string\")");
  CHECK(yvar->getstr() == "string");

  auto& zvar = initcvar("zvar", 22_l);
  CHECK(zvar->intval() == 22);
  eval("(setq zvar \"foo\")");
  CHECK(zvar->getstr() == "foo");

  SECTION("move assignment")
  {
    auto& move0 = initcvar("move0", 321_l);
    auto& move1 = initcvar("move1", 432_l);
    CHECK(move0->intval() == 321);
    CHECK(move1->intval() == 432);
    move0 = std::move(move1);
    CHECK(move0->intval() == 432);
    CHECK(move1->intval() == 432);
  }
}

TEST_CASE("alloc: obarray")
{
  auto obs = obarray();
  auto cur = length(obs)->intval();
  auto a0 = mkatom("foo");
  obs = obarray();
  // The reason this is not 1 is that there are already two symbols in the
  // local symbol table: base and verbose.
  CHECK(length(obs)->intval() == cur + 1);
  auto a1 = mkatom("bar");
  obs = obarray();
  CHECK(length(obs)->intval() == cur + 2);

  // Test calling from lisp
  obs = eval("(obarray)");
  CHECK(length(obs)->intval() == cur + 2);
}

TEST_CASE("alloc: freecount")
{
  auto free = freecount();
  CHECK(free != nil);
}

#ifdef ENABLE_OBJECT_SIZES
TEST_CASE("alloc: object sizes")
{
  std::cout << "==========\n";
  std::cout << "sizeof object: " << sizeof(object) << std::endl;
  std::cout << "==========\n";
  std::cout << "std::monostate: " << sizeof(std::monostate) << std::endl;
  std::cout << "symbol::symbol_id: " << sizeof(symbol::symbol_id) << std::endl;
  std::cout << "int: " << sizeof(int) << std::endl;
  std::cout << "double: " << sizeof(double) << std::endl;
  std::cout << "indirect_t: " << sizeof(indirect_t) << std::endl;
  std::cout << "cons_t: " << sizeof(cons_t) << std::endl;
  std::cout << "std::string: " << sizeof(std::string) << std::endl;
  std::cout << "subr_index: " << sizeof(subr_index) << std::endl;
  std::cout << "ref_lambda_t: " << sizeof(ref_lambda_t) << std::endl;
  std::cout << "ref_closure_t: " << sizeof(ref_closure_t) << std::endl;
  std::cout << "destblock_t*: " << sizeof(destblock_t*) << std::endl;
  std::cout << "ref_file_t: " << sizeof(ref_file_t) << std::endl;
  std::cout << "cvariable_t: " << sizeof(cvariable_t) << std::endl;
  std::cout << "==========\n";
  std::cout << "subr_t: " << sizeof(subr_t) << std::endl;
  std::cout << "symbol::symbol_t: " << sizeof(symbol::symbol_t) << std::endl;
  std::cout << "closure_t: " << sizeof(closure_t) << std::endl;
}
#endif

} // namespace lisp

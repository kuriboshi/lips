//
// Lips, lisp shell.
// Copyright 2021-2022 Krister Joas
//
#include <iostream>
#include <catch2/catch.hpp>
#include <lisp/libisp.hh>

namespace lisp
{

TEST_CASE("Create lisp objects")
{
  lisp lisp;
  current c(lisp);

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
    auto lambda = mkatom(lisp, "lambda");
    CHECK(lambda == C_LAMBDA);
  }

  SECTION("Set variable")
  {
    auto i = mkatom(lisp, "i");
    auto j = mkatom(lisp, "j");
    auto a = mkstring(lisp, "a");
    auto b = mkstring(lisp, "b");

    set(lisp, i, a);
    set(lisp, j, b);
    CHECK(i != j);
    set(lisp, j, a);
    CHECK(i != j);

    file_t out0(std::make_unique<io::string_sink>());
    prin0(lisp, i, out0);
    CHECK(to_string(out0.sink()) == std::string(i->getstr()));

    file_t out1(std::make_unique<io::string_sink>());
    prin0(lisp, j, out1);
    CHECK(to_string(out1.sink()) == std::string(j->getstr()));

    std::string s_hello{"(hello)"};
    file_t in(s_hello);
    auto hello = lispread(lisp, in);
    file_t out2(std::make_unique<io::string_sink>());
    prin0(lisp, hello, out2);
    CHECK(to_string(out2.sink()) == s_hello);
  }
}

TEST_CASE("C Variables")
{
  lisp l;
  current c(l);

  auto& cvar = l.a().initcvar("cvar", 123_l);
  auto a = eval(cvar);
  CHECK(eq(cvar, 123_l));
  cvar = 321_l;
  CHECK(eq(cvar, 321_l));

  auto r0 = eval(cvar);
  CHECK(r0->intval() == 321);

  auto r1 = eval(l, "(setq cvar 444)");
  CHECK(r1->intval() == 444);
  CHECK(cvar->intval() == 444);

  auto r2 = eval(l, "cvar");
  CHECK(r2->intval() == 444);

  cvar = mkstring("hello");
  CHECK(cvar->getstr() == "hello");

  auto& xvar = l.a().initcvar("xvar", "hello"_l);
  eval(l, "(setq xvar \"world\")");
  CHECK(xvar->getstr() == "world");

  auto& yvar = initcvar(l.a(), "yvar", 0_l);
  eval(l, "(setq yvar \"string\")");
  CHECK(yvar->getstr() == "string");

  auto& zvar = initcvar("zvar", 22_l);
  CHECK(zvar->intval() == 22);
  eval("(setq zvar \"foo\")");
  CHECK(zvar->getstr() == "foo");
}

TEST_CASE("obarray")
{
  lisp l;
  current c(l);

  auto a0 = mkatom("foo");
  auto obs = obarray();
  // The reason this is not 1 is that there are already two symbols in the
  // local symbol table: base and verbose.
  CHECK(length(obs)->intval() == 3);
  auto a1 = mkatom("bar");
  obs = obarray();
  CHECK(length(obs)->intval() == 4);

  // Test calling from lisp
  obs = eval(l, "(obarray)");
  CHECK(length(obs)->intval() == 4);
}

TEST_CASE("reclaim + freecount")
{
  auto f0 = freecount();
  CHECK(is_NIL(reclaim(mknumber(1))));
  auto f1 = freecount();
  CHECK(f1->intval() > f0->intval());
}

#ifdef ENABLE_OBJECT_SIZES
TEST_CASE("Object sizes")
{
  std::cout << "==========\n";
  std::cout << "sizeof conscells_t: " << sizeof(alloc::conscells_t) << std::endl;
  std::cout << "sizeof lisp_t: " << sizeof(lisp_t) << std::endl;
  std::cout << "==========\n";
  std::cout << "std::monostate: " << sizeof(std::monostate) << std::endl;
  std::cout << "symbol::print_name: " << sizeof(symbol::print_name) << std::endl;
  std::cout << "int: " << sizeof(int) << std::endl;
  std::cout << "double: " << sizeof(double) << std::endl;
  std::cout << "indirect_t: " << sizeof(indirect_t) << std::endl;
  std::cout << "cons_t: " << sizeof(cons_t) << std::endl;
  std::cout << "std::string: " << sizeof(std::string) << std::endl;
  std::cout << "subr_index: " << sizeof(subr_index) << std::endl;
  std::cout << "lambda_t: " << sizeof(lambda_t) << std::endl;
  std::cout << "closure_t: " << sizeof(closure_t) << std::endl;
  std::cout << "destblock_t*: " << sizeof(destblock_t*) << std::endl;
  std::cout << "std::shared_ptr<file_t>: " << sizeof(std::shared_ptr<file_t>) << std::endl;
  std::cout << "cvariable_t: " << sizeof(cvariable_t) << std::endl;
  std::cout << "==========\n";
  std::cout << "subr_t: " << sizeof(subr_t) << std::endl;
}
#endif

}

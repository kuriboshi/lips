//
// Lips, lisp shell.
// Copyright 2021 Krister Joas
//
#include <iostream>
#include <doctest/doctest.h>
#include "libisp.hh"

namespace lisp
{

TEST_CASE("Create lisp objects")
{
  lisp lisp;
  current c(lisp);

  SUBCASE("Multiple calls to intern should return the same object for the same string")
  {
    auto hello0 = intern("hello");
    auto hello1 = intern("hello");
    CHECK(hello0 == hello1);
  }

  SUBCASE("Check constants are the same as interned strings")
  {
    auto lambda = intern("lambda");
    CHECK(lambda == C_LAMBDA);
  }

  SUBCASE("Check constants are the same as a local atom")
  {
    auto lambda = mkatom(lisp, "lambda");
    CHECK(lambda == C_LAMBDA);
  }

  SUBCASE("Set variable")
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

    file_t out0(std::make_unique<string_sink>());
    prin0(lisp, i, out0);
    CHECK(to_string(out0.sink()) == std::string(i->getstr()));

    file_t out1(std::make_unique<string_sink>());
    prin0(lisp, j, out1);
    CHECK(to_string(out1.sink()) == std::string(j->getstr()));

    std::string s_hello{"(hello)"};
    file_t in(s_hello);
    auto hello = lispread(lisp, in, false);
    file_t out2(std::make_unique<string_sink>());
    prin0(lisp, hello, out2);
    CHECK(to_string(out2.sink()) == s_hello);
  }
}

#ifdef ENABLE_OBJECT_SIZES
TEST_CASE("Object sizes")
{
  std::cout << "sizeof conscells_t: " << sizeof(alloc::conscells_t) << std::endl;
  std::cout << "sizeof lisp_t: " << sizeof(lisp_t) << std::endl;
  std::cout << "sizeof freelist: " << sizeof(lisp::current().a().freelist) << std::endl;
  std::cout << "freelist.size(): " << lisp::current().a().freelist.size() << std::endl;
  std::cout << "sizeof lisp_t*: " << sizeof(lisp_t*) << std::endl;

  std::cout << "std::monostate: " << sizeof(std::monostate) << std::endl;
  std::cout << "symbol_t: " << sizeof(symbol::symbol_t) << std::endl;
  std::cout << "int: " << sizeof(int) << std::endl;
  std::cout << "double: " << sizeof(double) << std::endl;
  std::cout << "indirect_t: " << sizeof(indirect_t) << std::endl;
  std::cout << "cons_t: " << sizeof(cons_t) << std::endl;
  std::cout << "std::string: " << sizeof(std::string) << std::endl;
  std::cout << "subr_t: " << sizeof(subr_t) << std::endl;
  std::cout << "lambda_t: " << sizeof(lambda_t) << std::endl;
  std::cout << "closure_t: " << sizeof(closure_t) << std::endl;
  std::cout << "destblock_t*: " << sizeof(destblock_t*) << std::endl;
  std::cout << "std::unique_ptr<file_t>: " << sizeof(std::unique_ptr<file_t>) << std::endl;
  std::cout << "cvariable_t: " << sizeof(cvariable_t) << std::endl;
}
#endif

}

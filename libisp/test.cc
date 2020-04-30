#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN

#include <memory>
#include <doctest/doctest.h>

#include "lisp.hh"
#include "alloc.hh"
#include "low.hh"
#include "io.hh"

TEST_CASE("Create lisp object")
{
  lisp::lisp lisp;
  SUBCASE("intern should return the same object for the same string")
  {
    auto hello0 = lisp.a().intern("hello");
    auto hello1 = lisp.a().intern("hello");
    CHECK(hello0 == hello1);
  }
  SUBCASE("intern from two different lisp objects should be the same")
  {
    lisp::lisp lisp1;
    auto hello0 = lisp.a().intern("hello");
    auto hello1 = lisp1.a().intern("hello");
    CHECK(hello0 == hello1);
  }
  SUBCASE("Check constants are the same as interned strings")
  {
    auto lambda = lisp.a().intern("lambda");
    CHECK(lambda == lisp::C_LAMBDA);
  }
  SUBCASE("Check constants are the same as a local atom")
  {
    auto lambda = lisp.a().mkatom("lambda");
    CHECK(lambda == lisp::C_LAMBDA);
  }
  SUBCASE("Set variable")
  {
    auto i = lisp.a().mkatom("i");
    auto j = lisp.a().mkatom("j");
    auto a = lisp.a().mkstring("a");
    auto b = lisp.a().mkstring("b");
    set(lisp, i, a);
    set(lisp, j, b);
    CHECK(i != j);
    set(lisp, j, a);
    CHECK(i != j);
    auto out0 = std::make_unique<lisp::io::stringsink>();
    prin0(lisp, i, out0.get(), 0);
    CHECK(out0->string() == std::string(i->stringval()));
    auto out1 = std::make_unique<lisp::io::stringsink>();
    prin0(lisp, j, out1.get(), 0);
    CHECK(out1->string() == std::string(j->stringval()));
    std::string s_hello{"(hello)"};
    auto in = std::make_unique<lisp::io::stringsource>(s_hello.c_str());
    auto hello = lispread(lisp, in.get(), false);
    auto out2 = std::make_unique<lisp::io::stringsink>();
    prin0(lisp, hello, out2.get(), 0);
    CHECK(out2->string() == s_hello);
  }
}

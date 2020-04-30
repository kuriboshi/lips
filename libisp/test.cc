#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN

#include <doctest/doctest.h>

#include "lisp.hh"
#include "alloc.hh"

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
}

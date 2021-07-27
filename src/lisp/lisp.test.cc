//
// Lips, lisp shell.
// Copyright 2021 Krister Joas
//
#include <doctest/doctest.h>
#include "libisp.hh"

namespace lisp
{

TEST_CASE("lisp.cc: current")
{
  lisp lisp0;
  lisp lisp1;

  SUBCASE("test 1")
  {
    auto v0 = mkatom(lisp0, "v0");
    setqq(lisp0, v0, mkatom(lisp0, "world"));
    // Same atom in the same interpreter should be TRUE.
    CHECK(eq(lisp0, v0->symvalue(), mkatom(lisp0, "world")) != NIL);
    // Same printname atom from different interpreters should be FALSE.
    CHECK(eq(lisp0, v0->symvalue(), mkatom(lisp1, "world")) == NIL);
  }

  SUBCASE("test 2")
  {
    auto v1 = mkatom(lisp1, "v1");
    setqq(lisp1, v1, mkatom(lisp1, "world"));
    // Same atom in the same interpreter should be TRUE.
    CHECK(eq(lisp1, v1->symvalue(), mkatom(lisp1, "world")) != NIL);
    // Same printname atom from different interpreters should be FALSE.
    CHECK(eq(lisp1, v1->symvalue(), mkatom(lisp0, "world")) == NIL);
  }

  SUBCASE("current 1")
  {
    // Set default lisp interpreter to lisp0
    current c(lisp0);
    auto v2 = mkatom("v2");
    setqq(v2, mkatom("world"));
    // Same atom in the same interpreter should be TRUE.
    CHECK(eq(lisp1, v2->symvalue(), mkatom(lisp0, "world")) != NIL);
    // Same printname atom from different interpreters should be FALSE.
    CHECK(eq(lisp1, v2->symvalue(), mkatom(lisp1, "world")) == NIL);
  }

  SUBCASE("current 2")
  {
    // Set default lisp interpreter to lisp1
    current c(lisp1);
    auto v3 = mkatom("v3");
    setqq(v3, mkatom("world"));
    // Same atom in the same interpreter should be TRUE.
    CHECK(eq(lisp1, v3->symvalue(), mkatom(lisp1, "world")) != NIL);
    // Same printname atom from different interpreters should be FALSE.
    CHECK(eq(lisp1, v3->symvalue(), mkatom(lisp0, "world")) == NIL);
  }
}

}

//
// Lips, lisp shell.
// Copyright 2021 Krister Joas
//
#include <catch2/catch.hpp>
#include "libisp.hh"

namespace lisp
{

TEST_CASE("lisp.cc: current")
{
  lisp lisp0;
  lisp lisp1;

  SECTION("test 1")
  {
    auto v0 = mkatom(lisp0, "v0");
    setqq(lisp0, v0, mkatom(lisp0, "world"));
    // Same atom in the same interpreter should be TRUE.
    CHECK(eq(lisp0, v0->symvalue(), mkatom(lisp0, "world")) != NIL);
    // Same printname atom from different interpreters should be FALSE.
    CHECK(eq(lisp0, v0->symvalue(), mkatom(lisp1, "world")) == NIL);
  }

  SECTION("test 2")
  {
    auto v1 = mkatom(lisp1, "v1");
    setqq(lisp1, v1, mkatom(lisp1, "world"));
    // Same atom in the same interpreter should be TRUE.
    CHECK(eq(lisp1, v1->symvalue(), mkatom(lisp1, "world")) != NIL);
    // Same printname atom from different interpreters should be FALSE.
    CHECK(eq(lisp1, v1->symvalue(), mkatom(lisp0, "world")) == NIL);
  }

  SECTION("current 1")
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

  SECTION("current 2")
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

  SECTION("mkprim")
  {
    std::vector<int> result;
    mkprim(
      "printall",
      [&result](lisp& l, LISPT a) -> LISPT {
        for(auto p: a)
        {
          result.push_back(p->intval());
        }
        return NIL;
      },
      subr_t::subr::NOEVAL, subr_t::spread::NOSPREAD);
    eval("(printall 0 1 2)");
    CHECK(result[0] == 0);
    CHECK(result[1] == 1);
    CHECK(result[2] == 2);
  }
}

}

//
// Lips, lisp shell.
// Copyright 2021 Krister Joas
//
#include <doctest/doctest.h>
#include "libisp.hh"

namespace lisp
{

TEST_CASE("Map functions")
{
  SUBCASE("map")
  {
  }

  SUBCASE("mapc")
  {
  }

  SUBCASE("maplist")
  {
  }

  SUBCASE("mapcar")
  {
    lisp l;
    current c(l);

    auto ls = mklist(l, mknumber(1), mknumber(2), mknumber(3));
    auto f = eval(l, "(lambda (a) (+ a 1))");
    auto r0 = mapcar(l, ls, f, NIL);
    CHECK(car(r0)->intval() == 2);
    CHECK(cadr(r0)->intval() == 3);
    CHECK(caddr(r0)->intval() == 4);
  }
}

}

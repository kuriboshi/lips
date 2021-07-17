//
// Lips, lisp shell.
// Copyright 2021 Krister Joas
//
#include <doctest/doctest.h>
#include "libisp.hh"

namespace lisp
{

TEST_CASE("Primary functions")
{
  lisp l;
  current c(l);

  SUBCASE("CAR and CDR")
  {
    SUBCASE("CAR")
    {
      auto a = eval(l, "(car (cons 1 2))");
      CHECK(a->intval() == 1);
    }

    SUBCASE("CDR")
    {
      auto b = eval(l, "(cdr (cons 1 2))");
      CHECK(b->intval() == 2);
    }
  }
}

}

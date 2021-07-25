//
// Lips, lisp shell.
// Copyright 2021 Krister Joas
//
#include <doctest/doctest.h>
#include "libisp.hh"

namespace lisp
{

TEST_CASE("User defined functions")
{
  lisp l;
  current c(l);

  auto a = mkatom("a");
  auto b = mkatom("b");
  auto r0 = eval(R"(
(progn
  (de l (a) a)
  (df n (a) a)
  (setq a 'b)
  (cons (l a) (n a)))
)");
  print(r0);
  CHECK(car(r0) == b);
  CHECK(cdr(r0) == a);
}

}

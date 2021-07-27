//
// Lips, lisp shell.
// Copyright 2021 Krister Joas
//
#include <doctest/doctest.h>
#include "libisp.hh"

namespace lisp
{

TEST_CASE("Debug functions")
{
  lisp l;
  current c(l);

  auto t = l.e().trace();
  CHECK(t == 0);
  evaltrace(mknumber(1));
  t = l.e().trace();
  CHECK(t == 1);
  evaltrace(l, mknumber(0));
  t = l.e().trace();
  CHECK(t == 0);
}

}

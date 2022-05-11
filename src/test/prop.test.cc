//
// Lips, lisp shell.
// Copyright 2021-2022 Krister Joas
//
#include <catch2/catch.hpp>
#include <lisp/lisp.hh>

namespace lisp
{

TEST_CASE("Property functions")
{
  lisp l;
  current c(l);

  auto sym = mkatom("sym");
  auto prop0 = mkatom("prop0");
  auto value0 = mkstring("value0");
  CHECK(putprop(sym, prop0, value0) == value0);
  auto p0 = getprop(sym, prop0);
  CHECK(p0 == value0);
  auto prop1 = mkatom("prop1");
  auto value1 = mkstring("value1");
  CHECK(putprop(l, sym, prop1, value1) == value1);
  auto plist = getplist(sym);
  CHECK(length(plist)->intval() == 4);
  CHECK(remprop(sym, prop0) == value0);
  // plist is changed in place
  CHECK(length(plist)->intval() == 2);
  CHECK(length(getplist(sym))->intval() == 2);
  setplist(sym, mklist(l, prop0, value0, prop1, value1));
  CHECK(length(getplist(l, sym))->intval() == 4);
  CHECK(getprop(l, sym, prop1) == value1);
  CHECK(getprop(sym, mkatom("prop2")) == NIL);
}

}

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

  SUBCASE("lambda/nlambda")
  {
    auto a = mkatom("a");
    auto b = mkatom("b");
    auto lam = lambda(l, mklist(mkatom("a")), mklist(mkatom("a")));
    auto nlam = nlambda(l, mklist(mkatom("a")), mklist(mkatom("a")));
    set(l, mkatom("a"), mkatom("b"));
    apply(l, lam, cons(a, NIL));
    auto r0 = cons(l, eval(l, mklist(nlam, a)), eval(l, mklist(lam, a)));
    CHECK(car(r0) == a);
    CHECK(cdr(r0) == b);
  }
  SUBCASE("defineq")
  {
    auto f0 = mklist(mklist(mkatom("f0"), lambda(mklist(mkatom("a")), mklist(mkatom("a")))),
      mklist(mkatom("f1"), lambda(mklist(mkatom("b")), mklist(mkatom("b")))));
    print(f0);
    auto r0 = defineq(f0);
    REQUIRE(type_of(r0) == type::CONS);
    CHECK(eq(r0->car(), mkatom("f0")));
  }
}

}

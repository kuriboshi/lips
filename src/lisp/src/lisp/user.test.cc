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

  SUBCASE("de/df")
  {
    auto a = mkatom("a");
    auto b = mkatom("b");
    auto nlam = df(l, mkatom("f0"), mklist(mkatom("a")), mklist(mkatom("a")));
    auto lam = de(l, mkatom("f1"), mklist(mkatom("a")), mklist(mkatom("a")));
    set(l, a, b);
    auto r0 = eval(l, "(cons (f0 a) (f1 a))");
    CHECK(car(r0) == a);
    CHECK(cdr(r0) == b);
  }

  SUBCASE("defineq")
  {
    auto f0 = mklist(mklist(mkatom("f0"), lambda(mklist(mkatom("a")), mklist(mkatom("a")))),
      mklist(mkatom("f1"), lambda(mklist(mkatom("b")), mklist(mkatom("b")))));
    auto r0 = defineq(f0);
    REQUIRE(type_of(r0) == type::CONS);
    CHECK(equal(r0, mklist(mkatom("f0"), mkatom("f1"))));
  }

  SUBCASE("Verbose flag")
  {
    CHECK(is_NIL(l.verbose()));
    eval(l, "(setq verbose t)");
    CHECK(is_T(l.verbose()));
  }

  SUBCASE("Redefine function")
  {
    auto f0 = define(l, mkatom("f"), lambda(mklist(mkatom("a")), mklist(mkatom("a"))));
    auto redef0 = getprop(mkatom("f"), mkatom("olddef"));
    CHECK(is_NIL(redef0));
    std::ostringstream cout;
    auto out = std::make_unique<file_t>(cout);
    l.primout(std::move(out));
    eval(l, "(setq verbose t)");
    auto f1 = define(l, mkatom("f"), lambda(mklist(mkatom("b")), mklist(mkatom("b"))));
    auto redef1 = getprop(mkatom("f"), mkatom("olddef"));
    CHECK(!is_NIL(redef1));
    CHECK(cout.str() == "(f redefined)\n");
  }
}

}

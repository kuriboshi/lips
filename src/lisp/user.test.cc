//
// Lips, lisp shell.
// Copyright 2021-2022 Krister Joas
//

#include <catch2/catch.hpp>
#include "libisp.hh"

namespace lisp
{

TEST_CASE("User defined functions")
{
  lisp l;
  current c(l);

  SECTION("de/df")
  {
    auto a = mkatom("a");
    auto b = mkatom("b");
    {
      auto nlam = df(l, mkatom("f0"), mklist(mkatom("a")), mklist(mkatom("a")));
      auto lam = de(l, mkatom("f1"), mklist(mkatom("a")), mklist(mkatom("a")));
      set(l, a, b);
      auto r0 = eval(l, "(cons (f0 a) (f1 a))");
      CHECK(car(r0) == a);
      CHECK(cdr(r0) == b);
    }
    {
      auto nlam = df(mkatom("f0"), mklist(mkatom("a")), mklist(mkatom("a")));
      auto lam = de(mkatom("f1"), mklist(mkatom("a")), mklist(mkatom("a")));
      set(l, a, b);
      auto r0 = eval(l, "(cons (f0 a) (f1 a))");
      CHECK(car(r0) == a);
      CHECK(cdr(r0) == b);
    }
  }

  SECTION("defineq")
  {
    auto f0 = mklist(mklist(mkatom("f0"), lambda(mklist(mkatom("a")), mklist(mkatom("a")))),
      mklist(mkatom("f1"), lambda(mklist(mkatom("b")), mklist(mkatom("b")))));
    auto r0 = defineq(f0);
    REQUIRE(type_of(r0) == type::CONS);
    CHECK(equal(r0, mklist(mkatom("f0"), mkatom("f1"))));
    auto r1 = defineq(l, f0);
    REQUIRE(type_of(r1) == type::CONS);
    CHECK(equal(r1, mklist(mkatom("f0"), mkatom("f1"))));
  }

  SECTION("getrep")
  {
    auto f = lambda(mklist(mkatom("a")), mklist(mklist(mkatom("cons"), mkatom("a"), NIL)));
    auto rep0 = getrep(f);
    // There is a problem here that the symbol 'nil' is not considered equal to
    // the empty list '()'.
    std::string s("(lambda (a) (cons a ()))\n");
    auto expected = lispread(s);
    CHECK(!is_NIL(equal(rep0, expected)));
    auto rep1 = getrep(l, f);
    CHECK(!is_NIL(equal(rep0, rep1)));
    auto r0 = apply(f, mklist(0_l));
    CHECK(equal(r0, cons(0_l, NIL)));
  }

  SECTION("Verbose flag")
  {
    CHECK(is_NIL(l.verbose()));
    eval(l, "(setq verbose t)");
    CHECK(is_T(l.verbose()));
  }

  SECTION("Redefine function")
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

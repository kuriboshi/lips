//
// Lips, lisp shell.
// Copyright 2021 Krister Joas
//
#include <doctest/doctest.h>
#include "libisp.hh"

#include <iostream>

template<typename Enumeration>
typename std::underlying_type<Enumeration>::type as_integer(Enumeration const value)
{
  return static_cast<typename std::underlying_type<Enumeration>::type>(value);
}

namespace lisp
{

TEST_CASE("Logic functions")
{
  lisp l;
  current c(l);

  SUBCASE("and")
  {
    CHECK(p_and(cons(T, cons(T, NIL))) == T);
    CHECK(p_and(cons(NIL, cons(T, NIL))) == NIL);
    CHECK(p_and(cons(T, cons(NIL, NIL))) == NIL);
    CHECK(p_and(cons(NIL, cons(NIL, NIL))) == NIL);
    CHECK(p_and(l, cons(T, cons(T, NIL))) == T);
    CHECK(p_and(l, cons(NIL, cons(T, NIL))) == NIL);
    CHECK(p_and(l, cons(T, cons(NIL, NIL))) == NIL);
    CHECK(p_and(l, cons(NIL, cons(NIL, NIL))) == NIL);
  }

  SUBCASE("or")
  {
    CHECK(p_or(cons(T, cons(T, NIL))) == T);
    CHECK(p_or(cons(NIL, cons(T, NIL))) == T);
    CHECK(p_or(cons(T, cons(NIL, NIL))) == T);
    CHECK(p_or(cons(NIL, cons(NIL, NIL))) == NIL);
    CHECK(p_or(l, cons(T, cons(T, NIL))) == T);
    CHECK(p_or(l, cons(NIL, cons(T, NIL))) == T);
    CHECK(p_or(l, cons(T, cons(NIL, NIL))) == T);
    CHECK(p_or(l, cons(NIL, cons(NIL, NIL))) == NIL);
  }

  SUBCASE("not")
  {
    CHECK(p_not(l, T) == NIL);
    CHECK(p_not(l, NIL) == T);
    CHECK(p_not(T) == NIL);
    CHECK(p_not(NIL) == T);
  }

  SUBCASE("if")
  {
    auto s0 = mkstring("true");
    auto s1 = mkstring("false");
    auto r0 = xif(T, s0, s1);
    std::cout << as_integer(type_of(r0)) << std::endl;
    REQUIRE(type_of(r0) == type::STRING);
    CHECK(r0->stringval() == "true");

    // Last argument is treated as a progn
    auto r1 = xif(NIL, s0, cons(s1, NIL));
    REQUIRE(type_of(r1) == type::STRING);
    CHECK(r1->stringval() == "false");

    // Check that we only evaluate the branch taken
    auto e0 = eval(R"(
(progn (setq a "a")
       (if t nil (setq a "b"))
       a))");
    REQUIRE(type_of(e0) == type::STRING);
    CHECK(e0->stringval() == "a");
  }
}

}

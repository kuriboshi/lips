//
// Lips, lisp shell.
// Copyright 2021-2022 Krister Joas
//
#include <iostream>
#include <catch2/catch.hpp>
#include <lisp/liblisp.hh>

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

  SECTION("and")
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

  SECTION("or")
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

  SECTION("not")
  {
    CHECK(p_not(l, T) == NIL);
    CHECK(p_not(l, NIL) == T);
    CHECK(p_not(T) == NIL);
    CHECK(p_not(NIL) == T);
  }

  SECTION("logic: if")
  {
    auto r0 = "(if t 0 1)"_e;
    REQUIRE(type_of(r0) == type::INTEGER);
    CHECK(r0->intval() == 0);
    r0 = "(if nil 0 1)"_e;
    REQUIRE(type_of(r0) == type::INTEGER);
    CHECK(r0->intval() == 1);
  }
}

}

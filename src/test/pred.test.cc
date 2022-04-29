//
// Lips, lisp shell.
// Copyright 2021-2022 Krister Joas
//
#include <catch2/catch.hpp>
#include <lisp/liblisp.hh>

namespace lisp
{

TEST_CASE("Predicate functions")
{
  lisp l;
  current c(l);

  SECTION("numberp")
  {
    CHECK(numberp(l, mknumber(1)) != NIL);
    CHECK(numberp(mknumber(1)) != NIL);
    CHECK(numberp(l, mkfloat(1.0)) != NIL);
    CHECK(numberp(mkfloat(1.0)) != NIL);
    CHECK(numberp(l, mkstring("hello")) == NIL);
    CHECK(numberp(mkstring("hello")) == NIL);
  }

  SECTION("listp")
  {
    CHECK(listp(l, cons(l, mknumber(1), NIL)) != NIL);
    CHECK(listp(cons(l, mknumber(1), NIL)) != NIL);
    CHECK(listp(l, mkstring("hello")) == NIL);
    CHECK(listp(mkstring("hello")) == NIL);
    CHECK(listp(l, NIL) == NIL);
    CHECK(listp(NIL) == NIL);
  }

  SECTION("nlistp")
  {
    CHECK(nlistp(l, cons(l, mknumber(1), NIL)) == NIL);
    CHECK(nlistp(cons(l, mknumber(1), NIL)) == NIL);
    CHECK(nlistp(l, mkstring("hello")) != NIL);
    CHECK(nlistp(mkstring("hello")) != NIL);
    CHECK(nlistp(l, NIL) == T);
    CHECK(nlistp(NIL) == T);
  }

  SECTION("boundp")
  {
    CHECK(is_NIL(boundp("string"_l)));
    auto ub = mkatom("ub");
    CHECK(is_NIL(boundp(l, ub)));
    auto bd = mkatom("bd");
    set(bd, NIL);
    CHECK(is_T(boundp(l, bd)));
  }

  SECTION("memb")
  {
    CHECK(memb(l, mknumber(2), mklist(mknumber(1), mknumber(2), mknumber(3))) != NIL);
    CHECK(memb(mknumber(2), mklist(mknumber(1), mknumber(2), mknumber(3))) != NIL);
    CHECK(memb(l, mknumber(4), mklist(mknumber(1), mknumber(2), mknumber(3))) == NIL);
    CHECK(memb(mknumber(4), mklist(mknumber(1), mknumber(2), mknumber(3))) == NIL);
  }

  SECTION("litatom")
  {
    CHECK(litatom(l, mkatom("a")) != NIL);
    CHECK(litatom(mkatom("a")) != NIL);
    CHECK(litatom(l, mkatom("t")) != NIL);
    CHECK(litatom(mkatom("t")) != NIL);
    CHECK(litatom(l, mkstring("a")) == NIL);
    CHECK(litatom(mkstring("a")) == NIL);
  }

  SECTION("equal")
  {
    auto num0 = mknumber(0);
    auto num1 = mknumber(1);
    auto str0 = mkstring("0");
    auto str1 = mkstring("1");
    auto lam0 = lambda(NIL, str0);
    auto lam1 = lambda(NIL, str1);
    CHECK(equal(l, num0, mknumber(0)) != NIL);
    CHECK(equal(num0, mknumber(0)) != NIL);
    CHECK(equal(l, num0, num1) == NIL);
    CHECK(equal(num0, num1) == NIL);
    CHECK(equal(l, str0, mkstring("0")) != NIL);
    CHECK(equal(str0, mkstring("0")) != NIL);
    CHECK(equal(l, str0, str1) == NIL);
    CHECK(equal(str0, str1) == NIL);
    CHECK(equal(l, lam0, lam1) == NIL);
    CHECK(equal(lam0, lam1) == NIL);
    CHECK(equal(l, lam0, lambda(NIL, mkstring("0"))) != NIL);
    CHECK(equal(lam0, lambda(NIL, mkstring("0"))) != NIL);
    CHECK(equal(l, num0, str0) == NIL);
    CHECK(equal(num0, str0) == NIL);
    CHECK(equal(l, mklist(num0, num1, num0), mklist(num1, num0, num1)) == NIL);
    CHECK(equal(mklist(num0, num1, num0), mklist(num1, num0, num1)) == NIL);
  }

  SECTION("typeof")
  {
    CHECK(xtypeof(NIL) == NIL);
    CHECK(xtypeof(mkatom("symbol")) == C_SYMBOL);
    CHECK(xtypeof(mknumber(0)) == C_INTEGER);
    CHECK(xtypeof(mkfloat(0.0)) == C_FLOAT);
    CHECK(xtypeof(cons(NIL, NIL)) == C_CONS);
    CHECK(xtypeof(mkstring("foo")) == C_STRING);
    CHECK(xtypeof(eval(l, "plus")) == C_SUBR);
    CHECK(xtypeof(eval(l, "quote")) == C_FSUBR);
    CHECK(xtypeof(lambda(NIL, NIL)) == C_LAMBDA);
    CHECK(xtypeof(nlambda(NIL, NIL)) == C_NLAMBDA);
    CHECK(xtypeof(closure(NIL, NIL)) == C_CLOSURE);
    //CHECK(xtypeof(eval("unbound")) == C_UNBOUND);
    CHECK(xtypeof(T) == T);
  }
}

}

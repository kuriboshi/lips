//
// Lips, lisp shell.
// Copyright 2021 Krister Joas
//
#include <doctest/doctest.h>
#include "libisp.hh"

namespace lisp
{

TEST_CASE("Predicate functions")
{
  SUBCASE("typeof")
  {
    CHECK(xtypeof(NIL) == NIL);
    CHECK(xtypeof(mkatom("symbol")) == C_SYMBOL);
    CHECK(xtypeof(mknumber(0)) == C_INTEGER);
    CHECK(xtypeof(mkfloat(0.0)) == C_FLOAT);
    CHECK(xtypeof(cons(NIL, NIL)) == C_CONS);
    CHECK(xtypeof(mkstring("foo")) == C_STRING);
    CHECK(xtypeof(eval("+")) == C_SUBR);
    //CHECK(xtypeof(eval("quote")) == C_FSUBR);
    CHECK(xtypeof(lambda(NIL, NIL)) == C_LAMBDA);
    CHECK(xtypeof(nlambda(NIL, NIL)) == C_NLAMBDA);
    CHECK(xtypeof(closure(NIL, NIL)) == C_CLOSURE);
    //CHECK(xtypeof(eval("unbound")) == C_UNBOUND);
    CHECK(xtypeof(T) == T);
  }
}

}

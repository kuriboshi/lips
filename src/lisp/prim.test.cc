//
// Lips, lisp shell.
// Copyright 2021 Krister Joas
//
#include <doctest/doctest.h>
#include "libisp.hh"

namespace lisp
{

TEST_CASE("Primary functions")
{
  lisp l;
  current c(l);

  SUBCASE("CAR and CDR")
  {
    auto sample = eval("(cons 1 2)");
    CHECK(car(sample)->intval() == 1);
    CHECK(cdr(sample)->intval() == 2);
  }

  SUBCASE("C..R and C...R")
  {
    auto sample0 = eval("(quote ((1 . 2) 3 . 4))");
    CHECK(caar(sample0)->intval() == 1);
    CHECK(cdar(sample0)->intval() == 2);
    CHECK(cadr(sample0)->intval() == 3);
    CHECK(cddr(sample0)->intval() == 4);

    auto sample1 = eval("(quote (((1 . 2) 3 . 4) (5 . 6) 7 . 8))");
    CHECK(caaar(sample1)->intval() == 1);
    CHECK(cdaar(sample1)->intval() == 2);
    CHECK(cadar(sample1)->intval() == 3);
    CHECK(cddar(sample1)->intval() == 4);
    CHECK(caadr(sample1)->intval() == 5);
    CHECK(cdadr(sample1)->intval() == 6);
    CHECK(caddr(sample1)->intval() == 7);
    CHECK(cdddr(sample1)->intval() == 8);
  }

  SUBCASE("C.R, C..R, C...R should return NIL for non-CONS types")
  {
    CHECK(eval(l, "(car 'a)") == NIL);
    CHECK(eval(l, "(cdr 'a)") == NIL);

    CHECK(eval(l, "(caar 'a)") == NIL);
    CHECK(eval(l, "(cadr 'a)") == NIL);
    CHECK(eval(l, "(cdar 'a)") == NIL);
    CHECK(eval(l, "(cddr 'a)") == NIL);

    CHECK(eval(l, "(caaar 'a)") == NIL);
    CHECK(eval(l, "(caadr 'a)") == NIL);
    CHECK(eval(l, "(cadar 'a)") == NIL);
    CHECK(eval(l, "(caddr 'a)") == NIL);
    CHECK(eval(l, "(cdaar 'a)") == NIL);
    CHECK(eval(l, "(cdadr 'a)") == NIL);
    CHECK(eval(l, "(cddar 'a)") == NIL);
    CHECK(eval(l, "(cdddr 'a)") == NIL);
  }

  SUBCASE("append")
  {
    auto list0 = eval("(list 1 2)");
    auto list1 = eval("(list 3)");
    auto list2 = eval("(list 4)");
    auto list = append(cons(list0, cons(list1, NIL)));
    auto r0 = iplus(list);
    CHECK(r0->intval() == 6);
    list = append(cons(list, cons(list2, NIL)));
    auto r1 = iplus(list);
    CHECK(r1->intval() == 10);
  }
}

}

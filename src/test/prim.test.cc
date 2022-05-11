//
// Lips, lisp shell.
// Copyright 2021-2022 Krister Joas
//
#include <catch2/catch.hpp>
#include <lisp/lisp.hh>

namespace lisp
{

TEST_CASE("Primary functions")
{
  lisp l;
  current c(l);

  SECTION("QUOTE")
  {
    auto a0 = quote(l, "a"_a);
    CHECK(a0 == "a"_a);
    auto a1 = quote("a"_a);
    CHECK(a1 == "a"_a);
  }

  SECTION("LAMBDA")
  {
    set(l, "a"_a, 1_l);
    auto f0 = lambda(l, "(a)"_l, "((plus a 1))"_l);
    auto r0 = apply(l, f0, mklist(2_l));
    CHECK(type_of(r0) == type::INTEGER);
    CHECK(type_of(3_l) == type::INTEGER);
    // TODO: Can't do this because Catch2 goes into an infinite loop when
    // converting a LISPT to a string.
    // CHECK(r0 == 3_l);
    CHECK(r0->intval() == 3_l->intval());
  }

  SECTION("CAR and CDR")
  {
    auto sample = eval(l, "(cons 1 2)");
    CHECK(car(sample)->intval() == 1);
    CHECK(cdr(sample)->intval() == 2);

    CHECK(car(l, sample)->intval() == 1);
    CHECK(cdr(l, sample)->intval() == 2);
  }

  SECTION("C..R and C...R")
  {
    {
      auto sample = eval(l, "(quote ((1 . 2) 3 . 4))");
      CHECK(caar(sample)->intval() == 1);
      CHECK(cdar(sample)->intval() == 2);
      CHECK(cadr(sample)->intval() == 3);
      CHECK(cddr(sample)->intval() == 4);

      CHECK(caar(l, sample)->intval() == 1);
      CHECK(cdar(l, sample)->intval() == 2);
      CHECK(cadr(l, sample)->intval() == 3);
      CHECK(cddr(l, sample)->intval() == 4);
    }

    {
      auto sample = eval(l, "(quote (((1 . 2) 3 . 4) (5 . 6) 7 . 8))");
      CHECK(caaar(sample)->intval() == 1);
      CHECK(cdaar(sample)->intval() == 2);
      CHECK(cadar(sample)->intval() == 3);
      CHECK(cddar(sample)->intval() == 4);
      CHECK(caadr(sample)->intval() == 5);
      CHECK(cdadr(sample)->intval() == 6);
      CHECK(caddr(sample)->intval() == 7);
      CHECK(cdddr(sample)->intval() == 8);

      CHECK(caaar(l, sample)->intval() == 1);
      CHECK(cdaar(l, sample)->intval() == 2);
      CHECK(cadar(l, sample)->intval() == 3);
      CHECK(cddar(l, sample)->intval() == 4);
      CHECK(caadr(l, sample)->intval() == 5);
      CHECK(cdadr(l, sample)->intval() == 6);
      CHECK(caddr(l, sample)->intval() == 7);
      CHECK(cdddr(l, sample)->intval() == 8);
    }
  }

  SECTION("C.R, C..R, C...R should return NIL for non-CONS types")
  {
    auto sym = mkatom("a");
    CHECK(car(sym) == NIL);
    CHECK(cdr(sym) == NIL);

    CHECK(caar(sym) == NIL);
    CHECK(cadr(sym) == NIL);
    CHECK(cdar(sym) == NIL);
    CHECK(cddr(sym) == NIL);
    
    CHECK(caaar(sym) == NIL);
    CHECK(caadr(sym) == NIL);
    CHECK(cadar(sym) == NIL);
    CHECK(caddr(sym) == NIL);
    CHECK(cdaar(sym) == NIL);
    CHECK(cdadr(sym) == NIL);
    CHECK(cddar(sym) == NIL);
    CHECK(cdddr(sym) == NIL);
  }

  SECTION("atom")
  {
    auto sym = mkatom("sym");
    CHECK(!is_NIL(atom(sym)));
    CHECK(!is_NIL(atom(l, sym)));
  }

  SECTION("append")
  {
    auto list0 = mklist(1_l, 2_l);
    auto list1 = mklist(3_l);
    auto list = append(cons(list0, cons(list1, NIL)));
    // Original list is unchanged
    auto r0 = iplus(list0);
    CHECK(r0->intval() == 3);
    auto r1 = iplus(list);
    CHECK(r1->intval() == 6);
    auto list2 = mklist(4_l);
    list = append(cons(list, cons(list2, NIL)));
    auto r2 = iplus(list);
    CHECK(r2->intval() == 10);
  }

  SECTION("nconc")
  {
    auto list0 = mklist(1_l, 2_l);
    auto list1 = mklist(3_l);
    auto list = nconc(cons(list0, cons(list1, NIL)));
    // Original list changes
    auto r0 = iplus(list0);
    CHECK(r0->intval() == 6);
    auto r1 = iplus(list);
    CHECK(r1->intval() == 6);
    auto list2 = mklist(4_l);
    list = nconc(l, cons(list, cons(list2, NIL)));
    // Original list changes
    auto r2 = iplus(list0);
    CHECK(r2->intval() == 10);
    auto r3 = iplus(list);
    CHECK(r3->intval() == 10);
  }

  SECTION("tconc")
  {
    // This example is from the Interlisp manual page 6.2.
    auto foo = tconc(NIL, 1_l);
    tconc(foo, 4_l);
    tconc(foo, 3_l);
    tconc(foo, 2_l);
    tconc(foo, 1_l);
    CHECK(equal(car(foo), mklist(1_l, 4_l, 3_l, 2_l, 1_l)));
    // Another example from the Interlisp manual
    foo = cons(NIL, NIL);
    tconc(l, foo, 5_l);
    tconc(foo, 4_l);
    tconc(l, foo, 3_l);
    tconc(foo, 2_l);
    tconc(l, foo, 1_l);
    CHECK(equal(car(foo), mklist(5_l, 4_l, 3_l, 2_l, 1_l)));
  }

  SECTION("attach")
  {
    auto foo = mklist(2_l);
    attach(1_l, foo);
    CHECK(equal(foo, mklist(1_l, 2_l)));
    attach(l, 0_l, foo);
    CHECK(equal(foo, mklist(0_l, 1_l, 2_l)));
  }

  SECTION("length")
  {
    auto list = mklist(0_l, 1_l, 2_l, 3_l);
    CHECK(length(list)->intval() == 4);
    CHECK(length(l, list)->intval() == 4);
  }

  SECTION("nth")
  {
    {
      auto foo = mklist(1_l, 2_l, 3_l, 4_l, 5_l);
      CHECK(equal(nth(foo, 2_l), mklist(2_l, 3_l, 4_l, 5_l)));
      CHECK(equal(nth(foo, 3_l), mklist(3_l, 4_l, 5_l)));
      CHECK(equal(nth(foo, 4_l), mklist(4_l, 5_l)));
      CHECK(equal(nth(foo, 5_l), mklist(5_l)));
    }
  }

  SECTION("null")
  {
    CHECK(!is_NIL(null(NIL)));
    CHECK(!is_NIL(null(l, NIL)));
    CHECK(is_NIL(null(0_l)));
    CHECK(is_NIL(null(l, 0_l)));
  }
}

}

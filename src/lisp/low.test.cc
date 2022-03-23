//
// Lips, lisp shell.
// Copyright 2021 Krister Joas
//
#include <catch2/catch.hpp>
#include "libisp.hh"

namespace lisp
{

TEST_CASE("Low level functions")
{
  lisp l;
  current c(l);

  SECTION("low: set")
  {
    std::string set0 = R"(
(progn
 (set 'a 'b)
 (set a 123)
 b)
)";
    auto e0 = eval(l, set0);
    CHECK(e0->intval() == 123);
  }

  SECTION("low: prohibit set on constants")
  {
    std::ostringstream os;
    auto of = std::make_unique<file_t>(os);
    l.primerr(std::move(of));
    CHECK_THROWS(set(l, T, NIL));
    CHECK_THROWS(set(l, mkatom("nil"), NIL));
    CHECK_THROWS(set(T, NIL));
    CHECK_THROWS(set(mkatom("nil"), NIL));
  }

  SECTION("low: setq")
  {
    auto a = mkatom("a");
    setq(l, a, mknumber(100));
    CHECK(a->symvalue()->intval() == 100);
    setq(a, mknumber(200));
    CHECK(a->symvalue()->intval() == 200);
  }

  SECTION("low: while")
  {
    set("a"_a, 0_l);
    set("i"_a, 3_l);
    xwhile(mklist("greaterp"_a, "i"_a, 0_l),
      mklist(
        mklist("setq"_a, "a"_a, mklist("+"_a, "a"_a, "i"_a)),
        mklist("setq"_a, "i"_a, mklist("-"_a, "i"_a, 1_l))));
    CHECK("a"_a->symvalue()->intval() == 6);
    set("i"_a, 3_l);
    xwhile(l, mklist(l, "greaterp"_a, "i"_a, 0_l),
      mklist(l, 
        mklist(l, "setq"_a, "a"_a, mklist(l, "+"_a, "a"_a, "i"_a)),
        mklist(l, "setq"_a, "i"_a, mklist(l, "-"_a, "i"_a, 1_l))));
  }

  SECTION("low: cond")
  {
    std::string cond0 = R"(
(progn
 (setq a 1)
 (cond ((eq a 1)
        "a")))
)";
    auto a0 = eval(l, cond0);
    CHECK(a0->getstr() == "a");

    std::string cond1 = R"(
(cond (nil nil))
)";
    auto a1 = eval(l, cond1);
    CHECK(is_NIL(a1));

    auto a2 = cond(l, NIL);
    CHECK(is_NIL(a2));

    auto a3 = cond(cons(mklist(T, mkstring("A")), NIL));
    CHECK(a3->getstr() == "A");

    auto a4 = cond(l, cons(l, mklist(l, T, mkstring("A")), NIL));
    CHECK(a4->getstr() == "A");

    std::string cond5 = R"(
(cond (t))
)";
    auto a5 = eval(l, cond5);
    CHECK(is_T(a5));
  }

  SECTION("low: prog1")
  {
    auto r0 = prog1(l, 1_l, mklist(2_l, 3_l, 4_l));
    CHECK(r0->intval() == 1);
    auto r1 = prog1(1_l, mklist(2_l, 3_l, 4_l));
    CHECK(r1->intval() == 1);
  }

  SECTION("low: progn")
  {
    auto a0 = progn(l, mklist(l, 1_l, 2_l, 3_l));
    CHECK(a0->intval() == 3);
    auto a1 = progn(mklist(1_l, 2_l, 3_l));
    CHECK(a1->intval() == 3);
    auto a2 = progn(l, NIL);
    CHECK(a2 == NIL);
  }

  SECTION("low: while")
  {
    std::string p0 = R"(
(progn
  (setq r 0)
  ((lambda (i)
    (while (leq i 9)
           (setq i (+ i 1)))
    (setq r i))
   0)
  r)
)";
    auto r0 = eval(l, p0);
    CHECK(r0->intval() == 10);
  }
}

}

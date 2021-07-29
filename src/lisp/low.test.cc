//
// Lips, lisp shell.
// Copyright 2021 Krister Joas
//
#include <doctest/doctest.h>
#include "libisp.hh"

namespace lisp
{

TEST_CASE("Low level functions")
{
  lisp l;
  current c(l);

  SUBCASE("cond")
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
    std::string cond2 = R"(
(cond)
)";
    auto a2 = eval(l, cond2);
    CHECK(is_NIL(a2));

    auto a3 = cond(cons(mklist(T, mkstring("A")), NIL));
    CHECK(a3->getstr() == "A");
  }

  SUBCASE("prog1")
  {
    auto r0 = prog1(l, mknumber(1), mklist(mknumber(2), mknumber(3), mknumber(4)));
    CHECK(r0->intval() == 1);
    auto r1 = prog1(mknumber(1), mklist(mknumber(2), mknumber(3), mknumber(4)));
    CHECK(r1->intval() == 1);
  }

  SUBCASE("prog2")
  {
    auto r0 = prog2(l, mknumber(1), mknumber(2), mklist(mknumber(3), mknumber(4), mknumber(5)));
    CHECK(r0->intval() == 2);
    auto r1 = prog2(mknumber(1), mknumber(2), mklist(mknumber(3), mknumber(4), mknumber(5)));
    CHECK(r1->intval() == 2);
  }

  SUBCASE("while")
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

  SUBCASE("set")
  {
    std::ostringstream os;
    auto of = std::make_unique<file_t>(os);
    l.primerr(std::move(of));
    CHECK_THROWS(set(l, T, NIL));
    CHECK_THROWS(set(l, mkatom("nil"), NIL));
    CHECK_THROWS(set(T, NIL));
    CHECK_THROWS(set(mkatom("nil"), NIL));
  }
}

}

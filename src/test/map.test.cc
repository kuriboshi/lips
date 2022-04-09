//
// Lips, lisp shell.
// Copyright 2021-2022 Krister Joas
//
#include <catch2/catch.hpp>
#include <lisp/libisp.hh>

namespace lisp
{

TEST_CASE("Map functions")
{
  lisp l;
  current c(l);

  SECTION("map")
  {
    auto& cvar = l.a().initcvar("cvar", NIL);
    auto r0 = eval(l, R"(
(map '(1 2 3)
      (lambda (a)
       (setq cvar (cons (car a) cvar)))))");
    CHECK(type_of(cvar) == type::CONS);
    CHECK(car(cvar)->intval() == 3);
    CHECK(cadr(cvar)->intval() == 2);
    CHECK(caddr(cvar)->intval() == 1);

    cvar = 0_l;
    auto f = eval(l, "(lambda (a) (setq cvar (plus (apply plus a) cvar)))");
    map(mklist(1_l, 1_l, 1_l), f, NIL);
    CHECK(cvar->intval() == 6);

    cvar = NIL;
    map(l, "(1 2 3)"_l, lambda(l, "(a)"_l, "((setq cvar (cons (car a) cvar)))"_l),
      lambda(l, "(a)"_l, "((cdr a))"_l));
    CHECK(type_of(cvar) == type::CONS);
    CHECK(car(cvar)->intval() == 3);
    CHECK(cadr(cvar)->intval() == 2);
    CHECK(caddr(cvar)->intval() == 1);
  }

  SECTION("mapc")
  {
    auto& cvar = l.a().initcvar("cvar", NIL);
    auto r0 = eval(l, R"(
(mapc '(1 2 3)
       (lambda (a)
        (setq cvar (cons a cvar)))))");
    REQUIRE(type_of(cvar) == type::CONS);
    CHECK(car(cvar)->intval() == 3);
    CHECK(cadr(cvar)->intval() == 2);
    CHECK(caddr(cvar)->intval() == 1);

    cvar = 0_l;
    auto f = lambda(l, "(a)"_l, "((setq cvar (plus a cvar)))"_l);
    mapc(mklist(1_l, 1_l, 1_l), f, NIL);
    REQUIRE(type_of(cvar) == type::INTEGER);
    CHECK(cvar->intval() == 3);

    cvar = 0_l;
    mapc(l, mklist(1_l, 2_l, 3_l), f, lambda(l, "(a)"_l, "((cdr a))"_l));
    REQUIRE(type_of(cvar) == type::INTEGER);
    CHECK(cvar->intval() == 6);
  }

  SECTION("maplist")
  {
    auto ls = mklist(l, mknumber(1), mknumber(2), mknumber(3));
    auto f = lambda(l, "(a)"_l, "((car a))"_l);

    auto r0 = maplist(l, ls, f, NIL);
    REQUIRE(type_of(r0) == type::CONS);
    CHECK(car(r0)->intval() == 1);
    CHECK(cadr(r0)->intval() == 2);
    CHECK(caddr(r0)->intval() == 3);

    auto r1 = maplist(ls, f, NIL);
    REQUIRE(type_of(r1) == type::CONS);
    CHECK(car(r1)->intval() == 1);
    CHECK(cadr(r1)->intval() == 2);
    CHECK(caddr(r1)->intval() == 3);

    auto r2 = maplist(l, ls, f, lambda(l, "(a)"_l, "((cdr a))"_l));
    REQUIRE(type_of(r2) == type::CONS);
    CHECK(car(r1)->intval() == 1);
    CHECK(cadr(r1)->intval() == 2);
    CHECK(caddr(r1)->intval() == 3);
  }

  SECTION("mapcar")
  {
    auto ls = mklist(l, mknumber(1), mknumber(2), mknumber(3));
    auto f = lambda(l, "(a)"_l, "((plus a 1))"_l);

    auto r0 = mapcar(l, ls, f, NIL);
    REQUIRE(type_of(r0) == type::CONS);
    CHECK(car(r0)->intval() == 2);
    CHECK(cadr(r0)->intval() == 3);
    CHECK(caddr(r0)->intval() == 4);

    auto r1 = mapcar(ls, f, NIL);
    REQUIRE(type_of(r1) == type::CONS);
    CHECK(car(r1)->intval() == 2);
    CHECK(cadr(r1)->intval() == 3);
    CHECK(caddr(r1)->intval() == 4);

    auto r2 = mapcar(l, ls, f, lambda(l, "(a)"_l, "((cdr a))"_l));
    REQUIRE(type_of(r0) == type::CONS);
    CHECK(car(r2)->intval() == 2);
    CHECK(cadr(r2)->intval() == 3);
    CHECK(caddr(r2)->intval() == 4);
  }
}

}

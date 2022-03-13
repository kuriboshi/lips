//
// Lips, lisp shell.
// Copyright 2021 Krister Joas
//
#include <doctest/doctest.h>
#include "libisp.hh"

namespace lisp
{

TEST_CASE("Map functions")
{
  lisp l;
  current c(l);

  SUBCASE("map")
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
    auto f = eval(l, "(lambda (a) (setq cvar (+ (apply + a) cvar)))");
    map(mklist(1_l, 1_l, 1_l), f, NIL);
    CHECK(cvar->intval() == 6);
  }

  SUBCASE("mapc")
  {
    auto& cvar = l.a().initcvar("cvar", NIL);
    auto r0 = eval(l, R"(
(mapc '(1 2 3)
       (lambda (a)
        (setq cvar (cons a cvar)))))");
    CHECK(type_of(cvar) == type::CONS);
    CHECK(car(cvar)->intval() == 3);
    CHECK(cadr(cvar)->intval() == 2);
    CHECK(caddr(cvar)->intval() == 1);

    cvar = 0_l;
    auto f = eval(l, "(lambda (a) (setq cvar (+ a cvar)))");
    mapc(mklist(1_l, 1_l, 1_l), f, NIL);
    CHECK(cvar->intval() == 3);
  }

  SUBCASE("maplist")
  {
    auto ls = mklist(l, mknumber(1), mknumber(2), mknumber(3));
    auto f = eval(l, "(lambda (a) (car a))");
    auto r0 = maplist(l, ls, f, NIL);
    CHECK(car(r0)->intval() == 1);
    CHECK(cadr(r0)->intval() == 2);
    CHECK(caddr(r0)->intval() == 3);
    auto r1 = maplist(ls, f, NIL);
    CHECK(car(r1)->intval() == 1);
    CHECK(cadr(r1)->intval() == 2);
    CHECK(caddr(r1)->intval() == 3);
  }

  SUBCASE("mapcar")
  {
    auto ls = mklist(l, mknumber(1), mknumber(2), mknumber(3));
    auto f = eval(l, "(lambda (a) (+ a 1))");
    auto r0 = mapcar(l, ls, f, NIL);
    CHECK(car(r0)->intval() == 2);
    CHECK(cadr(r0)->intval() == 3);
    CHECK(caddr(r0)->intval() == 4);
    auto r1 = mapcar(ls, f, NIL);
    CHECK(car(r1)->intval() == 2);
    CHECK(cadr(r1)->intval() == 3);
    CHECK(caddr(r1)->intval() == 4);
  }
}

}

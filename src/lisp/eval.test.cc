//
// Lips, lisp shell.
// Copyright 2021-2022 Krister Joas
//
#include <doctest/doctest.h>
#include "libisp.hh"

namespace lisp
{

TEST_CASE("LAMBDA and NLAMBDA")
{
  lisp l;
  current c(l);

  SUBCASE("LAMBDA - basic case")
  {
    auto a = eval(l, "(setq f (lambda () \"hello\"))");
    auto b = eval(l, "(f)");
    CHECK(type_of(b) == type::STRING);
    CHECK(b->stringval() == "hello");
  }
  SUBCASE("LAMBDA - one argument")
  {
    auto a = eval(l, "(setq f (lambda (x) (cons x nil)))");
    auto b = eval(l, "(f 10)");
    CHECK(type_of(b) == type::CONS);
    CHECK(type_of(b->car()) == type::INTEGER);
    CHECK(b->car()->intval() == 10);
  }
  SUBCASE("LAMBDA - spread case")
  {
    auto a = eval(l, "(setq f (lambda x (cadr x)))");
    auto b = eval(l, "(f 1 2)");
    CHECK(type_of(b) == type::INTEGER);
    CHECK(b->intval() == 2);
  }
  SUBCASE("LAMBDA - half spread")
  {
    auto a = eval(l, "(setq f (lambda (a . x) (list a (cadr x))))");
    auto b = eval(l, "(f 0 1 2)");
    CHECK(type_of(b) == type::CONS);
    CHECK(type_of(b->car()) == type::INTEGER);
    CHECK(b->car()->intval() == 0);
    CHECK(type_of(b->cdr()->car()) == type::INTEGER);
    CHECK(b->cdr()->car()->intval() == 2);
  }
  SUBCASE("NLAMBDA - basic case")
  {
    auto a = eval(l, "(setq f (nlambda (a) a))");
    auto b = eval(l, "(f x)");
    CHECK(type_of(b) == type::SYMBOL);
    CHECK(b->symbol().pname.name == "x");
  }
}

TEST_CASE("Eval functions")
{
  lisp lisp;
  current c(lisp);

  SUBCASE("Evaluate variable")
  {
    auto var = mkatom(lisp, "i");
    auto val = mknumber(lisp, 123);
    set(lisp, var, val);
    auto r0 = eval(lisp, var);
    CHECK(r0->intval() == 123);
  }

  SUBCASE("Evaluate simple expression: (+ 123 1)")
  {
    auto e1 = cons(lisp, mkatom(lisp, "+"), cons(lisp, mknumber(lisp, 123), cons(lisp, mknumber(lisp, 1), nullptr)));
    auto out0 = std::make_unique<file_t>(std::make_unique<string_sink>());
    prin0(lisp, e1, *out0.get());
    CHECK(to_string(out0->sink()) == std::string("(+ 123 1)"));
    auto r1 = eval(lisp, e1);
    CHECK(r1->intval() == 124);
  }
}

TEST_CASE("Closure")
{
  lisp l;
  current c(l);

  auto a = setq(mkatom("a"), mknumber(1));
  auto clos = closure(lambda(NIL, cons(mkatom("a"), NIL)), cons(mkatom("a"), NIL));
  auto r0 = eval(cons(clos, NIL));
  a = setq(mkatom("a"), mknumber(2));
  auto r1 = eval(apply(l, clos, NIL));
  CHECK(equal(r0, r1) != NIL);
}

TEST_CASE("topofstack")
{
  lisp l;
  current c(l);

  auto a = set(l, mkatom("a"), 88_l);
  eval(l, R"(
(defineq (f0 (lambda (a) (destblock (topofstack))))
         (f1 (lambda (a) (f0 a))))
)");
  auto r0 = eval(l, "(f0 101)");
  CHECK(!is_NIL(equal(mklist(1_l, cons(mkatom("a"), 88_l)), r0)));
  auto r1 = eval(l, "(f1 99)");
  CHECK(!is_NIL(equal(mklist(1_l, cons(mkatom("a"), 99_l)), r1)));
}

}

#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN

#include <memory>
#include <iostream>
#include <doctest/doctest.h>

#include "lisp.hh"
#include "alloc.hh"
#include "eval.hh"
#include "low.hh"
#include "io.hh"

namespace
{
inline lisp::LISPT eval(lisp::lisp& l, std::string expr)
{
  lisp::file_t in(expr);
  auto e = lispread(l, in, false);
  return eval(l, e);
}

template<typename T>
std::string to_string(T& sink)
{
  return static_cast<lisp::string_sink&>(sink).string();
}
}

TEST_CASE("Create lisp object")
{
  lisp::lisp lisp;

  SUBCASE("Multiple calls to intern should return the same object for the same string")
  {
    auto hello0 = lisp::intern("hello");
    auto hello1 = lisp::intern("hello");
    CHECK(hello0 == hello1);
  }

  SUBCASE("Check constants are the same as interned strings")
  {
    auto lambda = lisp::intern("lambda");
    CHECK(lambda == lisp::C_LAMBDA);
  }

  SUBCASE("Check constants are the same as a local atom")
  {
    auto lambda = mkatom(lisp, "lambda");
    CHECK(lambda == lisp::C_LAMBDA);
  }

  SUBCASE("Set variable")
  {
    auto i = mkatom(lisp, "i");
    auto j = mkatom(lisp, "j");
    auto a = mkstring(lisp, "a");
    auto b = mkstring(lisp, "b");

    set(lisp, i, a);
    set(lisp, j, b);
    CHECK(i != j);
    set(lisp, j, a);
    CHECK(i != j);

    auto sink0 = std::make_unique<lisp::string_sink>();
    lisp::file_t out0(std::move(sink0));
    prin0(lisp, i, out0);
    CHECK(to_string(out0.sink()) == std::string(i->getstr()));

    auto sink1 = std::make_unique<lisp::string_sink>();
    lisp::file_t out1(std::move(sink1));
    prin0(lisp, j, out1);
    CHECK(to_string(out1.sink()) == std::string(j->getstr()));

    std::string s_hello{"(hello)"};
    lisp::file_t in(s_hello);
    auto hello = lispread(lisp, in, false);
    auto sink2 = std::make_unique<lisp::string_sink>();
    lisp::file_t out2(std::move(sink2));
    prin0(lisp, hello, out2);
    CHECK(to_string(out2.sink()) == s_hello);
  }
}

TEST_CASE("Evaluator")
{
  lisp::lisp lisp;

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
    auto sink0 = std::make_unique<lisp::string_sink>();
    auto out0 = std::make_unique<lisp::file_t>(std::move(sink0));
    prin0(lisp, e1, *out0.get());
    CHECK(to_string(out0->sink()) == std::string("(+ 123 1)"));
    auto r1 = eval(lisp, e1);
    CHECK(r1->intval() == 124);
  }
}

TEST_CASE("Basic I/O")
{
  lisp::lisp lisp;

  auto out0 = std::make_unique<lisp::string_sink>();
  lisp.primout(*new lisp::file_t(std::move(out0)));
  lisp.primout().printf("hello world %d", 123);
  CHECK(to_string(lisp.primout().sink()) == std::string("hello world 123"));
}

TEST_CASE("Check size of lisp_t object")
{
  CHECK(sizeof(lisp::lisp_t::u) == 40);
}

TEST_CASE("Arithmetic functions")
{
  lisp::lisp l;
  SUBCASE("+")
  {
    auto r = eval(l, "(+ 1 2 3 4 5)");
    CHECK(r->intval() == 15);
  }
  SUBCASE("-")
  {
    auto r = eval(l, "(- 1 2)");
    CHECK(r->intval() == -1);
  }
  SUBCASE("*")
  {
    auto r = eval(l, "(* 5 7)");
    CHECK(r->intval() == 35);
  }
  SUBCASE("/ 1")
  {
    auto r = eval(l, "(/ 4 2)");
    CHECK(r->intval() == 2);
  }
  SUBCASE("/ 2")
  {
    auto r = eval(l, "(/ 4 (itof 2))");
    CHECK(r->floatval() == 2.0);
  }
  SUBCASE("i+")
  {
    auto r = eval(l, "(i+ 1 2 7)");
    CHECK(r->intval() == 10);
  }
  SUBCASE("i-")
  {
    auto r = eval(l, "(i- 13 2)");
    CHECK(r->intval() == 11);
  }
  SUBCASE("i*")
  {
    auto r = eval(l, "(i* 6 8)");
    CHECK(r->intval() == 48);
  }
  SUBCASE("i/")
  {
    auto r = eval(l, "(i/ 5 2)");
    CHECK(r->intval() == 2);
  }
  SUBCASE("i%")
  {
    auto r = eval(l, "(i% 5 2)");
    CHECK(r->intval() == 1);
  }

  SUBCASE("iminus")
  {
    auto r = eval(l, "(iminus 1)");
    CHECK(r->intval() == -1);
  }
  SUBCASE("minus")
  {
    auto r = eval(l, "(minus (itof 1))");
    CHECK(r->floatval() == -1.0);
  }
  SUBCASE("add1")
  {
    auto r = eval(l, "(add1 2)");
    CHECK(r->intval() == 3);
  }
  SUBCASE("sub1")
  {
    auto r = eval(l, "(sub1 3)");
    CHECK(r->intval() == 2);
  }
  SUBCASE("abs")
  {
    auto r = eval(l, "(abs -1)");
    CHECK(r->intval() == 1);
  }
  SUBCASE("f+")
  {
    auto r = eval(l, "(f+ (itof 5) (itof 2))");
    CHECK(type_of(r) == lisp::FLOAT);
    CHECK(r->floatval() == 7.0);
  }
  SUBCASE("f-")
  {
    auto r = eval(l, "(f- (itof 5) (itof 2))");
    CHECK(type_of(r) == lisp::FLOAT);
    CHECK(r->floatval() == 3.0);
  }
  SUBCASE("f*")
  {
    auto r = eval(l, "(f* (itof 5) (itof 2))");
    CHECK(type_of(r) == lisp::FLOAT);
    CHECK(r->floatval() == 10.0);
  }
  SUBCASE("f/")
  {
    auto r = eval(l, "(f/ (itof 5) (itof 2))");
    CHECK(type_of(r) == lisp::FLOAT);
    CHECK(r->floatval() == 2.5);
  }
  SUBCASE("itof")
  {
    auto r = eval(l, "(itof 8)");
    CHECK(type_of(r) == lisp::FLOAT);
    CHECK(r->floatval() == 8.0);
  }

  SUBCASE("greaterp 1")
  {
    auto r = eval(l, "(greaterp 5 2)");
    CHECK(is_T(r));
  }
  SUBCASE("greaterp 2")
  {
    auto r = eval(l, "(greaterp 5 5)");
    CHECK(is_NIL(r));
  }
  SUBCASE("geq 1")
  {
    auto r = eval(l, "(geq 5 2)");
    CHECK(is_T(r));
  }
  SUBCASE("geq 2")
  {
    auto r = eval(l, "(geq 5 5)");
    CHECK(is_T(r));
  }
  SUBCASE("lessp 1")
  {
    auto r = eval(l, "(lessp 5 2)");
    CHECK(is_NIL(r));
  }
  SUBCASE("lessp 2")
  {
    auto r = eval(l, "(lessp 2 5)");
    CHECK(is_T(r));
  }
  SUBCASE("leq 1")
  {
    auto r = eval(l, "(leq 5 2)");
    CHECK(is_NIL(r));
  }
  SUBCASE("leq 2")
  {
    auto r = eval(l, "(leq 2 5)");
    CHECK(is_T(r));
  }
  SUBCASE("leq 3")
  {
    auto r = eval(l, "(leq 2 2)");
    CHECK(is_T(r));
  }
  SUBCASE("zerop 1")
  {
    auto r = eval(l, "(zerop 0)");
    CHECK(is_T(r));
  }
  SUBCASE("zerop 2")
  {
    auto r = eval(l, "(zerop 1)");
    CHECK(is_NIL(r));
  }
  SUBCASE("eqp")
  {
    auto r = eval(l, "(eqp 5 2)");
    CHECK(is_NIL(r));
  }
  SUBCASE("neqp")
  {
    auto r = eval(l, "(neqp 5 2)");
    CHECK(is_T(r));
  }
  SUBCASE("minusp")
  {
    auto r = eval(l, "(minusp -5)");
    CHECK(is_T(r));
  }
}

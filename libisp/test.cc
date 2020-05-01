#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN

#include <memory>
#include <iostream>
#include <doctest/doctest.h>

#include "lisp.hh"
#include "alloc.hh"
#include "eval.hh"
#include "low.hh"
#include "io.hh"

TEST_CASE("Create lisp object")
{
  lisp::lisp lisp;
  SUBCASE("intern should return the same object for the same string")
  {
    auto hello0 = intern(lisp, "hello");
    auto hello1 = intern(lisp, "hello");
    CHECK(hello0 == hello1);
  }
  SUBCASE("intern from two different lisp objects should be the same")
  {
    lisp::lisp lisp1;
    auto hello0 = intern(lisp, "hello");
    auto hello1 = intern(lisp1, "hello");
    CHECK(hello0 == hello1);
  }
  SUBCASE("Check constants are the same as interned strings")
  {
    auto lambda = intern(lisp, "lambda");
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
    auto out0 = std::make_unique<lisp::io::stringsink>();
    prin0(lisp, i, out0.get(), 0);
    CHECK(out0->string() == std::string(i->stringval()));
    auto out1 = std::make_unique<lisp::io::stringsink>();
    prin0(lisp, j, out1.get(), 0);
    CHECK(out1->string() == std::string(j->stringval()));
    std::string s_hello{"(hello)"};
    auto in = std::make_unique<lisp::io::stringsource>(s_hello.c_str());
    auto hello = lispread(lisp, in.get(), false);
    auto out2 = std::make_unique<lisp::io::stringsink>();
    prin0(lisp, hello, out2.get(), 0);
    CHECK(out2->string() == s_hello);
  }
}

TEST_CASE("Evaluator")
{
  SUBCASE("Evaluate variable")
  {
    lisp::lisp lisp;
    auto var = mkatom(lisp, "i");
    auto val = mknumber(lisp, 123);
    set(lisp, var, val);
    auto r0 = eval(lisp, var);
    CHECK(r0->intval() == 123);
    auto e1 = cons(lisp, mkatom(lisp, "+"), cons(lisp, r0, cons(lisp, mknumber(lisp, 1), nullptr)));
    auto out0 = std::make_unique<lisp::io::stringsink>();
    prin0(lisp, e1, out0.get(), 0);
    CHECK(out0->string() == std::string("(+ 123 1)"));
    auto r1 = eval(lisp, e1);
    CHECK(r1->intval() == 124);
  }
}

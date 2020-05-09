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
    auto out0 = std::make_unique<lisp::file_t>(std::move(sink0));
    prin0(lisp, i, *out0.get());
    CHECK(to_string(out0->sink()) == std::string(i->getstr()));
    auto sink1 = std::make_unique<lisp::string_sink>();
    auto out1 = std::make_unique<lisp::file_t>(std::move(sink1));
    prin0(lisp, j, *out1.get());
    CHECK(to_string(out1->sink()) == std::string(j->getstr()));
    std::string s_hello{"(hello)"};
    auto in = std::make_unique<lisp::file_t>(std::make_unique<lisp::string_source>(s_hello.c_str()));
    auto hello = lispread(lisp, *in.get(), false);
    auto sink2 = std::make_unique<lisp::string_sink>();
    auto out2 = std::make_unique<lisp::file_t>(std::move(sink2));
    prin0(lisp, hello, *out2.get());
    CHECK(to_string(out2->sink()) == s_hello);
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

//
// Lips, lisp shell.
// Copyright 2021-2022 Krister Joas
//
#include <catch2/catch.hpp>
#include "libisp.hh"

namespace lisp
{

TEST_CASE("lisp: current")
{
  lisp lisp0;
  lisp lisp1;

  SECTION("test 1")
  {
    auto v0 = mkatom(lisp0, "v0");
    setqq(lisp0, v0, mkatom(lisp0, "world"));
    // Same atom in the same interpreter should be TRUE.
    CHECK(eq(lisp0, v0->symvalue(), mkatom(lisp0, "world")) != NIL);
    // Same printname atom from different interpreters should be FALSE.
    CHECK(eq(lisp0, v0->symvalue(), mkatom(lisp1, "world")) == NIL);
  }

  SECTION("test 2")
  {
    auto v1 = mkatom(lisp1, "v1");
    setqq(lisp1, v1, mkatom(lisp1, "world"));
    // Same atom in the same interpreter should be TRUE.
    CHECK(eq(lisp1, v1->symvalue(), mkatom(lisp1, "world")) != NIL);
    // Same printname atom from different interpreters should be FALSE.
    CHECK(eq(lisp1, v1->symvalue(), mkatom(lisp0, "world")) == NIL);
  }

  SECTION("current 1")
  {
    // Set default lisp interpreter to lisp0
    current c(lisp0);
    auto v2 = mkatom("v2");
    setqq(v2, mkatom("world"));
    // Same atom in the same interpreter should be TRUE.
    CHECK(eq(lisp1, v2->symvalue(), mkatom(lisp0, "world")) != NIL);
    // Same printname atom from different interpreters should be FALSE.
    CHECK(eq(lisp1, v2->symvalue(), mkatom(lisp1, "world")) == NIL);
  }

  SECTION("current 2")
  {
    // Set default lisp interpreter to lisp1
    current c(lisp1);
    auto v3 = mkatom("v3");
    setqq(v3, mkatom("world"));
    // Same atom in the same interpreter should be TRUE.
    CHECK(eq(lisp1, v3->symvalue(), mkatom(lisp1, "world")) != NIL);
    // Same printname atom from different interpreters should be FALSE.
    CHECK(eq(lisp1, v3->symvalue(), mkatom(lisp0, "world")) == NIL);
  }

  SECTION("mkprim")
  {
    std::vector<int> result;
    mkprim(
      "printall",
      [&result](lisp& l, LISPT a) -> LISPT {
        for(auto p: a)
        {
          result.push_back(p->intval());
        }
        return NIL;
      },
      subr_t::subr::NOEVAL, subr_t::spread::NOSPREAD);
    eval("(printall 0 1 2)");
    CHECK(result[0] == 0);
    CHECK(result[1] == 1);
    CHECK(result[2] == 2);
  }
}

TEST_CASE("lisp: literals")
{
  SECTION("string")
  {
    auto s = "string"_s;
    CHECK(type_of(s) == type::STRING);
  }

  SECTION("integer")
  {
    auto i = 1001_l;
    CHECK(type_of(i) == type::INTEGER);
  }

  SECTION("float")
  {
    auto d = 1.2_l;
    CHECK(type_of(d) == type::FLOAT);
  }

  SECTION("sexpr")
  {
    auto sexpr = "((a b) c)"_l;
    REQUIRE(type_of(sexpr) == type::CONS);
    auto a = caar(sexpr);
    CHECK(type_of(a) == type::SYMBOL);
    CHECK(a->symbol().pname.name == "a");
    file_t out(std::make_unique<io::string_sink>());
    prin0(sexpr, out);
    CHECK(to_string(out.sink()) == "((a b) c)");
  }

  SECTION("atom")
  {
    auto a = "atom"_a;
    CHECK(type_of(a) == type::SYMBOL);
  }

  SECTION("eval")
  {
    auto e = "(+ 1 2)"_e;
    REQUIRE(type_of(e) == type::INTEGER);
    CHECK(e->intval() == 3);
  }
}

}

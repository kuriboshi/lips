//
// Lips, lisp shell.
// Copyright 2021 Krister Joas
//
#include <fstream>
#include <doctest/doctest.h>
#include "libisp.hh"

namespace lisp
{

TEST_CASE("File functions")
{
  lisp l;
  current c(l);

  SUBCASE("ratom")
  {
    {
      std::ofstream of("test.lisp");
      of << "atom\n";
    }
    auto in0 = open(mkstring("test.lisp"), C_READ);
    auto e0 = ratom(in0);
    REQUIRE(type_of(e0) == type::SYMBOL);
    CHECK(e0->getstr() == "atom");
  }

  SUBCASE("load")
  {
    {
      std::ofstream of("test.lisp");
      of << "(quote (a b c))\n";
    }
    auto e0 = load(mkstring("test.lisp"));
  }

  SUBCASE("print")
  {
    auto f0 = open(mkstring("test.lisp"), intern("write"));
    print(mkstring("hello"), f0);
  }

  SUBCASE("terpri")
  {
    auto f0 = open(mkstring("test.lisp"), intern("write"));
    print(mkstring("hello"), f0);
    terpri(f0);
  }

  SUBCASE("prin1")
  {
    auto f0 = open(mkstring("test.lisp"), intern("write"));
    prin1(mkstring("hello"), f0);
  }

  SUBCASE("prin2")
  {
    auto f0 = open(mkstring("test.lisp"), intern("write"));
    prin2(mkstring("hello"), f0);
  }
}

}

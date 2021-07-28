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
    auto f0 = open(mkstring("test_print.lisp"), intern("write"));
    print(mkstring("hello"), f0);
    close(f0);
    auto f1 = open(mkstring("test_print.lisp"), intern("read"));
    auto r1 = getline(f1);
    REQUIRE(r1 != NIL);
    CHECK(r1->getstr() == "\"hello\"");
  }

  SUBCASE("terpri")
  {
    auto f0 = open(mkstring("test_terpri.lisp"), intern("write"));
    print(mkstring("hello"), f0);
    terpri(f0);
    close(f0);
    auto f1 = open(mkstring("test_terpri.lisp"), intern("read"));
    auto r1 = getline(f1);
    REQUIRE(r1 != NIL);
    CHECK(r1->getstr() == "\"hello\"");
  }

  SUBCASE("prin1")
  {
    auto f0 = open(mkstring("test_prin1.lisp"), intern("write"));
    prin1(mkstring("hello \"world\""), f0);
    close(f0);
    auto f1 = open(mkstring("test_prin1.lisp"), intern("read"));
    auto r1 = getline(f1);
    REQUIRE(r1 != NIL);
    CHECK(r1->getstr() == "hello \"world\"");
  }

  SUBCASE("prin2")
  {
    auto f0 = open(mkstring("test_prin2.lisp"), intern("write"));
    prin2(mkstring("hello \"world\""), f0);
    close(f0);
    auto f1 = open(mkstring("test_prin2.lisp"), intern("read"));
    auto r1 = getline(f1);
    REQUIRE(r1 != NIL);
    // TODO: Is this correct?  Should replace print/prin1/prin2 with the CL
    // versions print/prin1/princ.
    CHECK(r1->getstr() == "\"hello \\\"world\\\"\"");
  }

  SUBCASE("readc")
  {
    std::string is = R"(test)";
    LISPT f = l.a().getobject();
    f->set(std::make_shared<file_t>(is));
    auto ch0 = readc(l, f);
    CHECK(ch0->intval() == 't');
    auto ch1 = readc(l, f);
    CHECK(ch1->intval() == 'e');
    auto ch2 = readc(l, f);
    CHECK(ch2->intval() == 's');
    auto ch3 = readc(l, f);
    CHECK(ch3->intval() == 't');
  }

  SUBCASE("read")
  {
    std::string is = R"((a b c))";
    LISPT f = l.a().getobject();
    f->set(std::make_shared<file_t>(is));
    auto sexpr = read(l, f);
    CHECK(!is_NIL(equal(sexpr, mklist(mkatom("a"), mkatom("b"), mkatom("c")))));
  }

  SUBCASE("spaces")
  {
    std::ostringstream cout;
    auto out = std::make_unique<file_t>(cout);
    l.primout(std::move(out));
    spaces(l, 8_l, NIL);
    CHECK(cout.str() == "        ");
  }

  SUBCASE("readline")
  {
    std::string is = R"(test)";
    LISPT f = l.a().getobject();
    f->set(std::make_shared<file_t>(is));
    auto r = readline(l, f);
    CHECK(type_of(r) == type::CONS);
    CHECK(equal(r, mklist(mkatom("test"))));
  }
}

}

//
// Lips, lisp shell.
// Copyright 2021-2022 Krister Joas
//
#include <fstream>
#include <filesystem>
#include <catch2/catch.hpp>
#include <lisp/libisp.hh>

namespace lisp
{

TEST_CASE("File functions")
{
  lisp l;
  current c(l);

  SECTION("ratom")
  {
    constexpr const char* test_file{"test.lisp"};
    {
      std::ofstream of(test_file);
      of << "atom\n";
    }
    auto in0 = open(mkstring("test.lisp"), C_READ);
    auto e0 = ratom(in0);
    REQUIRE(type_of(e0) == type::SYMBOL);
    CHECK(e0->getstr() == "atom");
    std::filesystem::remove(test_file);
  }

  SECTION("load")
  {
    constexpr const char* test_file{"test.lisp"};
    {
      std::ofstream of(test_file);
      of << "(quote (a b c))\n";
    }
    auto e0 = load(mkstring(test_file));
    std::filesystem::remove(test_file);
  }

  SECTION("print")
  {
    constexpr const char* test_file{"test_print.lisp"};
    auto f0 = open(mkstring(test_file), C_WRITE);
    print(mkstring("hello"), f0);
    close(f0);
    auto f1 = open(mkstring(test_file), C_READ);
    auto r1 = getline(f1);
    REQUIRE(r1 != NIL);
    CHECK(r1->getstr() == "\"hello\"");
    std::filesystem::remove(test_file);
  }

  SECTION("terpri")
  {
    constexpr const char* test_file{"test_terpri.lisp"};
    auto f0 = open(mkstring(test_file), C_WRITE);
    print(mkstring("hello"), f0);
    terpri(f0);
    close(f0);
    auto f1 = open(mkstring(test_file), C_READ);
    auto r1 = getline(f1);
    REQUIRE(r1 != NIL);
    CHECK(r1->getstr() == "\"hello\"");
    std::filesystem::remove(test_file);
  }

  SECTION("prin1")
  {
    constexpr const char* test_file{"test_prin1.lisp"};
    auto f0 = open(mkstring(test_file), C_WRITE);
    prin1(mkstring("hello \"world\""), f0);
    close(f0);
    auto f1 = open(mkstring(test_file), C_READ);
    auto r1 = getline(f1);
    REQUIRE(r1 != NIL);
    CHECK(r1->getstr() == "hello \"world\"");
    std::filesystem::remove(test_file);
  }

  SECTION("prin2")
  {
    constexpr const char* test_file = "test_prin2.lisp";
    auto f0 = open(mkstring(test_file), C_WRITE);
    prin2(mkstring("hello \"world\""), f0);
    close(f0);
    auto f1 = open(mkstring(test_file), C_READ);
    auto r1 = getline(f1);
    REQUIRE(r1 != NIL);
    // TODO: Is this correct?  Should replace print/prin1/prin2 with the CL
    // versions print/prin1/princ.
    CHECK(r1->getstr() == "\"hello \\\"world\\\"\"");
    std::filesystem::remove(test_file);
  }

  SECTION("printlevel")
  {
    constexpr const char* test_file = "test_printlevel.lisp";
    {
      auto f0 = open(mkstring(test_file), C_WRITE);
      printlevel(1_l);
      print("(a (b (c)))"_l, f0);
      close(f0);
      auto f1 = open(mkstring(test_file), C_READ);
      auto r1 = getline(f1);
      REQUIRE(r1 != NIL);
      CHECK(r1->getstr() == "(a &)");
    }
    {
      auto f0 = open(mkstring(test_file), C_WRITE);
      printlevel(l, 2_l);
      print(l, "(a (b (c)))"_l, f0);
      close(l, f0);
      auto f1 = open(l, mkstring(test_file), C_READ);
      auto r1 = getline(l, f1);
      REQUIRE(r1 != NIL);
      CHECK(r1->getstr() == "(a (b &))");
      std::filesystem::remove(test_file);
    }
  }

  SECTION("readc")
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

  SECTION("read")
  {
    std::string is = R"((a b c))";
    LISPT f = l.a().getobject();
    f->set(std::make_shared<file_t>(is));
    auto sexpr = read(l, f);
    CHECK(!is_NIL(equal(sexpr, mklist("a"_a, "b"_a, "c"_a))));
  }

  SECTION("spaces")
  {
    std::ostringstream cout;
    auto out = std::make_unique<file_t>(cout);
    l.primout(std::move(out));
    spaces(l, 8_l, NIL);
    CHECK(cout.str() == "        ");
  }

  SECTION("readline")
  {
    SECTION("One atom")
    {
      std::string is = R"(test)";
      LISPT f = l.a().getobject();
      f->set(std::make_shared<file_t>(is));
      auto r = readline(l, f);
      CHECK(type_of(r) == type::CONS);
      auto expected = mklist("test"_a);
      CHECK(equal(r, expected));
    }
    SECTION("Two atoms")
    {
      std::string is = R"(test test)";
      LISPT f = l.a().getobject();
      f->set(std::make_shared<file_t>(is));
      auto r = readline(l, f);
      CHECK(type_of(r) == type::CONS);
      auto expected = mklist("test"_a, "test"_a);
      CHECK(equal(r, expected));
    }
  }
}

}

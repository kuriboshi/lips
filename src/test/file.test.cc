//
// Lips, lisp shell.
// Copyright 2021-2022 Krister Joas
//
#include <fstream>
#include <filesystem>
#include <catch2/catch.hpp>
#include <lisp/lisp.hh>

namespace
{
struct create_test_file final
{
  static constexpr const char* file{"test.lisp"};
  create_test_file(const std::string& contents)
  {
    std::ofstream of(file);
    of << contents;
  }
  ~create_test_file()
  {
    std::filesystem::remove(file);
  }
};
}

namespace lisp
{

TEST_CASE("file: functions")
{
  lisp l;
  current c(l);

  SECTION("open and close")
  {
    create_test_file test("()");
    {
      auto f = open(l, mkstring(test.file), C_READ);
      auto r = read(l, f);
      CHECK(is_NIL(r));
      CHECK(is_T(close(l, f)));
    }

    {
      auto f = open(mkstring(test.file), C_READ);
      auto r = read(f);
      CHECK(is_NIL(r));
      CHECK(is_T(close(f)));
    }
  }

  SECTION("ratom")
  {
    create_test_file test("atom\n");
    {
      auto in0 = open(l, mkstring(test.file), C_READ);
      auto e0 = ratom(l, in0);
      REQUIRE(type_of(e0) == type::SYMBOL);
      CHECK(e0->getstr() == "atom");
    }
    {
      auto in0 = open(mkstring(test.file), C_READ);
      auto e0 = ratom(in0);
      REQUIRE(type_of(e0) == type::SYMBOL);
      CHECK(e0->getstr() == "atom");
    }
  }

  SECTION("load")
  {
    {
      create_test_file test("(setq a 1)\n");
      auto e0 = load(mkstring(test.file));
      CHECK("a"_a->value()->intval() == 1);
    }
    {
      create_test_file test("(setq a 2)\n");
      auto e0 = load(l, mkstring(test.file));
      CHECK("a"_a->value()->intval() == 2);
    }
  }

  SECTION("print")
  {
    constexpr const char* test_file{"test_print.lisp"};
    auto f0 = open(mkstring(test_file), C_WRITE);
    print(l, "hello"_s, f0);
    print("world"_s, f0);
    close(f0);
    auto f1 = open(mkstring(test_file), C_READ);
    auto r1 = getline(f1);
    REQUIRE(r1 != NIL);
    CHECK(r1->getstr() == "\"hello\"");
    auto r2 = getline(f1);
    REQUIRE(r2 != NIL);
    CHECK(r2->getstr() == "\"world\"");
    std::filesystem::remove(test_file);
  }

  SECTION("terpri")
  {
    constexpr const char* test_file{"test_terpri.lisp"};
    auto f0 = open(mkstring(test_file), C_WRITE);
    prin1("\"hello"_a, f0);
    terpri(l, f0);
    terpri(f0);
    prin1("world\""_a, f0);
    close(f0);
    auto f1 = open(mkstring(test_file), C_READ);
    auto r1 = read(f1);
    REQUIRE(r1 != NIL);
    CHECK(type_of(r1) == type::STRING);
    CHECK(r1->getstr() == "hello\n\nworld");
    std::filesystem::remove(test_file);
  }

  SECTION("prin1")
  {
    constexpr const char* test_file{"test_prin1.lisp"};
    auto f0 = open(mkstring(test_file), C_WRITE);
    prin1(l, mkstring("hello "), f0);
    prin1(mkstring("\"world\""), f0);
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
    LISPT f = l.a().getobject();
    f->set(ref_file_t::create(R"(test)"));
    auto ch0 = readc(l, f);
    CHECK(ch0->intval() == 't');
    auto ch1 = readc(f);
    CHECK(ch1->intval() == 'e');
    auto ch2 = readc(l, f);
    CHECK(ch2->intval() == 's');
    auto ch3 = readc(f);
    CHECK(ch3->intval() == 't');
  }

  SECTION("read")
  {
    LISPT f = l.a().getobject();
    f->set(ref_file_t::create(R"((a b c))"));
    auto sexpr = read(l, f);
    CHECK(!is_NIL(equal(sexpr, mklist("a"_a, "b"_a, "c"_a))));
  }

  SECTION("spaces")
  {
    {
      std::ostringstream cout;
      auto out = ref_file_t::create(cout);
      l.primout(std::move(out));
      spaces(l, 8_l, NIL);
      CHECK(cout.str() == "        ");
    }
    {
      std::ostringstream cout;
      auto out = ref_file_t::create(cout);
      l.primout(std::move(out));
      spaces(8_l, NIL);
      CHECK(cout.str() == "        ");
    }
  }

  SECTION("readline")
  {
    SECTION("One atom")
    {
      LISPT f = l.a().getobject();
      f->set(ref_file_t::create(R"(test)"));
      auto r = readline(l, f);
      CHECK(type_of(r) == type::CONS);
      auto expected = mklist("test"_a);
      CHECK(equal(r, expected));
    }
    SECTION("Two atoms")
    {
      LISPT f = l.a().getobject();
      f->set(ref_file_t::create(R"(test test)"));
      auto r = readline(f);
      CHECK(type_of(r) == type::CONS);
      auto expected = mklist("test"_a, "test"_a);
      CHECK(equal(r, expected));
    }
  }

  SECTION("loadfile")
  {
    create_test_file test("(setq a \"loadfile\")");
    {
      CHECK(loadfile(l, test.file));
      CHECK("a"_a->value()->string() == "loadfile");
    }
    {
      CHECK(loadfile(test.file));
      CHECK("a"_a->value()->string() == "loadfile");
    }
  }
}

}

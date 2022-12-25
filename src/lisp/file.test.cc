//
// Lips, lisp shell.
// Copyright 2021-2022 Krister Joas
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//

#include <fstream>
#include <filesystem>
#include <string>

#include <catch2/catch.hpp>

#include "alloc.hh"
#include "file.hh"
#include "pred.hh"

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
  ~create_test_file() { std::filesystem::remove(file); }
};
} // namespace

namespace lisp
{

TEST_CASE("file: functions")
{
  auto& ctx = context::current();
  SECTION("open and close")
  {
    create_test_file test("()");
    {
      auto f = open(mkstring(test.file), C_READ);
      auto r = read(f);
      CHECK(is_NIL(r));
      CHECK(is_T(close(f)));
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
      auto in0 = open(mkstring(test.file), C_READ);
      auto e0 = ratom(in0);
      REQUIRE(type_of(e0) == type::Symbol);
      CHECK(e0->getstr() == "atom");
    }
    {
      auto in0 = open(mkstring(test.file), C_READ);
      auto e0 = ratom(in0);
      REQUIRE(type_of(e0) == type::Symbol);
      CHECK(e0->getstr() == "atom");
    }
  }

  SECTION("load")
  {
    context::current().loadpath(mklist(C_DOT));
    {
      create_test_file test("(setq a 1)\n");
      auto e0 = load(mkstring(test.file));
      CHECK("a"_a->value()->intval() == 1);
    }
    {
      create_test_file test("(setq a 2)\n");
      auto e0 = load(mkstring(test.file));
      CHECK("a"_a->value()->intval() == 2);
    }
  }

  SECTION("print")
  {
    constexpr const char* test_file{"test_print.lisp"};
    auto f0 = open(mkstring(test_file), C_WRITE);
    print("hello"_s, f0);
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
    terpri(f0);
    terpri(f0);
    prin1("world\""_a, f0);
    close(f0);
    auto f1 = open(mkstring(test_file), C_READ);
    auto r1 = read(f1);
    REQUIRE(r1 != NIL);
    CHECK(type_of(r1) == type::String);
    CHECK(r1->getstr() == "hello\n\nworld");
    std::filesystem::remove(test_file);
  }

  SECTION("prin1")
  {
    constexpr const char* test_file{"test_prin1.lisp"};
    auto f0 = open(mkstring(test_file), C_WRITE);
    prin1(mkstring("hello "), f0);
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
      printlevel(2_l);
      print("(a (b (c)))"_l, f0);
      close(f0);
      auto f1 = open(mkstring(test_file), C_READ);
      auto r1 = getline(f1);
      REQUIRE(r1 != NIL);
      CHECK(r1->getstr() == "(a (b &))");
      std::filesystem::remove(test_file);
    }
    printlevel(0_l);
  }

  SECTION("readc")
  {
    lisp_t f = details::alloc::getobject(ref_file_t::create(R"(test)"));
    auto ch0 = readc(f);
    CHECK(ch0->intval() == 't');
    auto ch1 = readc(f);
    CHECK(ch1->intval() == 'e');
    auto ch2 = readc(f);
    CHECK(ch2->intval() == 's');
    auto ch3 = readc(f);
    CHECK(ch3->intval() == 't');
  }

  SECTION("read")
  {
    lisp_t f = details::alloc::getobject(ref_file_t::create(R"((a b c))"));
    auto sexpr = read(f);
    CHECK(!is_NIL(equal(sexpr, mklist("a"_a, "b"_a, "c"_a))));
  }

  SECTION("spaces")
  {
    {
      std::ostringstream cout;
      auto old = ctx.primout(ref_file_t::create(cout));
      spaces(8_l, NIL);
      CHECK(cout.str() == "        ");
      ctx.primout(old);
    }
    {
      std::ostringstream cout;
      auto old = ctx.primout(ref_file_t::create(cout));
      spaces(8_l, NIL);
      CHECK(cout.str() == "        ");
      ctx.primout(old);
    }
  }

  SECTION("readline")
  {
    SECTION("One atom")
    {
      lisp_t f = details::alloc::getobject(ref_file_t::create(R"(test)"));
      auto r = readline(f);
      CHECK(type_of(r) == type::Cons);
      auto expected = mklist("test"_a);
      CHECK(equal(r, expected));
    }
    SECTION("Two atoms")
    {
      lisp_t f = details::alloc::getobject(ref_file_t::create(R"(test test)"));
      auto r = readline(f);
      CHECK(type_of(r) == type::Cons);
      auto expected = mklist("test"_a, "test"_a);
      CHECK(equal(r, expected));
    }
  }

  SECTION("loadfile")
  {
    context::current().loadpath(mklist(C_DOT));
    create_test_file test("(setq a \"loadfile\")");
    {
      REQUIRE(loadfile(test.file));
      CHECK("a"_a->value()->string() == "loadfile");
    }
    {
      REQUIRE(loadfile(test.file));
      CHECK("a"_a->value()->string() == "loadfile");
    }
  }

  SECTION("append")
  {
    create_test_file test("(setq a");
    auto f = open(mkstring(test.file), C_APPEND);
    prin1(" 999)"_s, f);
    terpri(f);
    close(f);
    context::current().loadpath(mklist(C_DOT));
    auto e = load(mkstring(test.file));
    REQUIRE(type_of("a"_a->value()) == type::Integer);
    CHECK("a"_a->value()->intval() == 999);
  }
}

TEST_CASE("file: open error conditions")
{
  SECTION("open with illegal mode")
  {
    create_test_file test("");
    CHECK_THROWS(open(mkstring(test.file), C_CONS));
  }

  SECTION("open non-existing file")
  {
    CHECK_THROWS(open(mkstring("/etc/xyzzy"), NIL));
  }
}

} // namespace lisp

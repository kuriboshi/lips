//
// Lips, lisp shell.
// Copyright 2021-2023 Krister Joas
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

#include <iostream>
#include <fstream>
#include <filesystem>
#include <string>

#include <catch2/catch_test_macros.hpp>

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
  SECTION("open and close")
  {
    create_test_file test("()");
    {
      auto f = open(mkstring(test.file), C_READ);
      auto r = read(f);
      CHECK(is_nil(r));
      CHECK(is_T(close(f)));
    }

    {
      auto f = open(mkstring(test.file), C_READ);
      auto r = read(f);
      CHECK(is_nil(r));
      CHECK(is_T(close(f)));
    }
  }

  SECTION("ratom")
  {
    create_test_file test("atom\n");
    SECTION("from file")
    {
      auto in0 = open(mkstring(test.file), C_READ);
      auto e0 = ratom(in0);
      REQUIRE(type_of(e0) == object::type::Symbol);
      CHECK(e0->getstr() == "atom");
    }

    SECTION("from primin")
    {
      auto in = ref_file_t::create("atom");
      auto old = vm::primin(in);
      auto r = ratom(lisp_t());
      REQUIRE(type_of(r) == object::type::Symbol);
      CHECK(r->getstr() == "atom");
      vm::primin(old);
    }

    SECTION("from stdin")
    {
      std::istringstream stream("nil");
      auto* buf = std::cin.rdbuf(stream.rdbuf());
      auto r = ratom(T);
      CHECK(r == nil);
      std::cin.rdbuf(buf);
    }
  }

  SECTION("load")
  {
    vm::loadpath(mklist(C_DOT));
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
    REQUIRE(r1 != nil);
    CHECK(r1->getstr() == "\"hello\"");
    auto r2 = getline(f1);
    REQUIRE(r2 != nil);
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
    REQUIRE(r1 != nil);
    CHECK(type_of(r1) == object::type::String);
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
    REQUIRE(r1 != nil);
    CHECK(r1->getstr() == "hello \"world\"");
    std::filesystem::remove(test_file);
  }

  SECTION("prin2")
  {
    constexpr const char* test_file = "test_prin2.lisp";
    SECTION("basic")
    {
      auto f0 = open(mkstring(test_file), C_WRITE);
      prin2(mkstring("hello \"world\""), f0);
      close(f0);
      auto f1 = open(mkstring(test_file), C_READ);
      auto r1 = getline(f1);
      REQUIRE(r1 != nil);
      // TODO: Is this correct?  Should replace print/prin1/prin2 with the CL
      // versions print/prin1/princ.
      CHECK(r1->getstr() == "\"hello \\\"world\\\"\"");
      std::filesystem::remove(test_file);
    }

    SECTION("to primout")
    {
      std::ostringstream cout;
      auto old = vm::primout(ref_file_t::create(cout));
      prin2(mkstring("hello \"world\""), nil);
      CHECK(cout.str() == "\"hello \\\"world\\\"\"");
      vm::primout(old);
    }
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
      REQUIRE(r1 != nil);
      CHECK(r1->getstr() == "(a &)");
    }
    {
      auto f0 = open(mkstring(test_file), C_WRITE);
      printlevel(2_l);
      print("(a (b (c)))"_l, f0);
      close(f0);
      auto f1 = open(mkstring(test_file), C_READ);
      auto r1 = getline(f1);
      REQUIRE(r1 != nil);
      CHECK(r1->getstr() == "(a (b &))");
      std::filesystem::remove(test_file);
    }
    printlevel(0_l);
  }

  SECTION("readc")
  {
    SECTION("basic")
    {
      lisp_t f = getobject(ref_file_t::create(R"(test)"));
      auto ch0 = readc(f);
      CHECK(ch0->intval() == 't');
      auto ch1 = readc(f);
      CHECK(ch1->intval() == 'e');
      auto ch2 = readc(f);
      CHECK(ch2->intval() == 's');
      auto ch3 = readc(f);
      CHECK(ch3->intval() == 't');
    }

    SECTION("from primin")
    {
      auto in = ref_file_t::create("a");
      auto old = vm::primin(in);
      auto r = readc(nil);
      REQUIRE(type_of(r) == object::type::Integer);
      CHECK(r->intval() == 'a');
      vm::primin(old);
    }

    SECTION("from stdin")
    {
      std::istringstream stream("a");
      auto* buf = std::cin.rdbuf(stream.rdbuf());
      auto r = readc(T);
      REQUIRE(type_of(r) == object::type::Integer);
      CHECK(r->intval() == 'a');
      std::cin.rdbuf(buf);
    }
  }

  SECTION("read")
  {
    SECTION("basic")
    {
      lisp_t f = getobject(ref_file_t::create(R"((a b c))"));
      auto sexpr = read(f);
      CHECK(!is_nil(equal(sexpr, mklist("a"_a, "b"_a, "c"_a))));
    }

    SECTION("from primin")
    {
      auto in = ref_file_t::create(R"((a b c))");
      auto old = vm::primin(in);
      auto r = read(nil);
      REQUIRE(type_of(r) == object::type::Cons);
      CHECK(!is_nil(equal(r, mklist("a"_a, "b"_a, "c"_a))));
      vm::primin(old);
    }

    SECTION("from stdin")
    {
      std::istringstream stream(R"((a b c))");
      auto* buf = std::cin.rdbuf(stream.rdbuf());
      auto r = read(T);
      REQUIRE(type_of(r) == object::type::Cons);
      CHECK(!is_nil(equal(r, mklist("a"_a, "b"_a, "c"_a))));
      std::cin.rdbuf(buf);
    }
  }

  SECTION("spaces")
  {
    SECTION("basic")
    {
      constexpr const char* test_file{"test_spaces.txt"};
      auto f0 = open(mkstring(test_file), C_WRITE);
      spaces(8_l, f0);
      close(f0);
      std::ifstream in{test_file};
      std::string line;
      std::getline(in, line);
      CHECK(line == "        ");
      std::filesystem::remove(test_file);
    }

    SECTION("to primout")
    {
      std::ostringstream cout;
      auto old = vm::primout(ref_file_t::create(cout));
      spaces(8_l, nil);
      CHECK(cout.str() == "        ");
      vm::primout(old);
    }

    SECTION("to primerr")
    {
      std::ostringstream cout;
      auto old = vm::primerr(ref_file_t::create(cout));
      spaces(8_l, T);
      CHECK(cout.str() == "        ");
      vm::primerr(old);
    }
  }

  SECTION("readline")
  {
    SECTION("One atom")
    {
      lisp_t f = getobject(ref_file_t::create(R"(test)"));
      auto r = readline(f);
      CHECK(type_of(r) == object::type::Cons);
      auto expected = mklist("test"_a);
      CHECK(equal(r, expected));
    }

    SECTION("Two atoms")
    {
      lisp_t f = getobject(ref_file_t::create(R"(test test)"));
      auto r = readline(f);
      CHECK(type_of(r) == object::type::Cons);
      auto expected = mklist("test"_a, "test"_a);
      CHECK(equal(r, expected));
    }

    SECTION("eof")
    {
      create_test_file test{""};
      auto in = open(mkstring(test.file), C_READ);
      auto r = readline(in);
      CHECK(r == C_EOF);
    }

    SECTION("from primin")
    {
      auto in = ref_file_t::create(R"((a b c))");
      auto old = vm::primin(in);
      auto r = readline(lisp_t(nil));
      REQUIRE(type_of(r) == object::type::Cons);
      CHECK(!is_nil(equal(r, mklist("a"_a, "b"_a, "c"_a))));
      vm::primin(old);
    }

    SECTION("from stdin")
    {
      std::istringstream stream("a");
      auto* buf = std::cin.rdbuf(stream.rdbuf());
      auto r = readline(T);
      REQUIRE(type_of(r) == object::type::Cons);
      REQUIRE(type_of(r->car()) == object::type::Symbol);
      CHECK(r->car()->symbol()->pname == "a");
      std::cin.rdbuf(buf);
    }
  }

  SECTION("loadfile")
  {
    vm::loadpath(mklist(C_DOT));
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
    vm::loadpath(mklist(C_DOT));
    auto e = load(mkstring(test.file));
    REQUIRE(type_of("a"_a->value()) == object::type::Integer);
    CHECK("a"_a->value()->intval() == 999);
  }
}

TEST_CASE("file: error reading file")
{
  create_test_file test("(error)");
  auto file = mkstring(test.file);
  CHECK_THROWS(load(file));
}

TEST_CASE("file: try to read non-existing file")
{
  auto file = mkstring("/does-not-exist.lisp");
  CHECK_THROWS(load(file));
}

TEST_CASE("file: open error conditions")
{
  SECTION("open with illegal mode")
  {
    create_test_file test("");
    CHECK_THROWS(open(mkstring(test.file), C_CONS));
  }

  SECTION("open non-existing file") { CHECK_THROWS(open(mkstring("/etc/xyzzy"), nil)); }
}

} // namespace lisp

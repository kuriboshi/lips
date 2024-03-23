//
// Lips, lisp shell.
// Copyright 2021-2024 Krister Joas
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
#include <catch2/catch_approx.hpp>

#include "alloc.hh"
#include "atoms.hh"
#include "file.hh"
#include "list.hh"
#include "predicate.hh"

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
      auto f = open(mkstring(test.file), atoms::READ);
      auto r = read(f);
      CHECK(is_nil(r));
      CHECK(is_T(close(f)));
    }

    {
      auto f = open(mkstring(test.file), atoms::READ);
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
      auto in0 = open(mkstring(test.file), atoms::READ);
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
    vm::loadpath(mklist(atoms::DOT));
    {
      create_test_file test("(setq a 1)\n");
      auto e0 = load(mkstring(test.file));
      CHECK("a"_a->value()->as_integer() == 1);
    }
    {
      create_test_file test("(setq a 2)\n");
      auto e0 = load(mkstring(test.file));
      CHECK("a"_a->value()->as_integer() == 2);
    }
  }

  SECTION("print")
  {
    constexpr const char* test_file{"test_print.lisp"};
    auto f0 = open(mkstring(test_file), atoms::WRITE);
    print("hello"_s, f0);
    print("world"_s, f0);
    close(f0);
    auto f1 = open(mkstring(test_file), atoms::READ);
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
    auto f0 = open(mkstring(test_file), atoms::WRITE);
    prin1("\"hello"_a, f0);
    terpri(f0);
    terpri(f0);
    prin1("world\""_a, f0);
    close(f0);
    auto f1 = open(mkstring(test_file), atoms::READ);
    auto r1 = read(f1);
    REQUIRE(r1 != nil);
    CHECK(type_of(r1) == object::type::String);
    CHECK(r1->getstr() == "hello\n\nworld");
    std::filesystem::remove(test_file);
  }

  SECTION("prin1")
  {
    constexpr const char* test_file{"test_prin1.lisp"};
    auto f0 = open(mkstring(test_file), atoms::WRITE);
    prin1(mkstring("hello "), f0);
    prin1(mkstring("\"world\""), f0);
    close(f0);
    auto f1 = open(mkstring(test_file), atoms::READ);
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
      auto f0 = open(mkstring(test_file), atoms::WRITE);
      prin2(mkstring("hello \"world\""), f0);
      close(f0);
      auto f1 = open(mkstring(test_file), atoms::READ);
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
    SECTION("printlevel == 1")
    {
      auto f0 = open(mkstring(test_file), atoms::WRITE);
      printlevel(1_l);
      print("(a (b (c)))"_l, f0);
      close(f0);
      auto f1 = open(mkstring(test_file), atoms::READ);
      auto r1 = getline(f1);
      REQUIRE(r1 != nil);
      CHECK(r1->getstr() == "(a &)");
      std::filesystem::remove(test_file);
    }

    SECTION("printlevel == 2")
    {
      auto f0 = open(mkstring(test_file), atoms::WRITE);
      printlevel(2_l);
      print("(a (b (c)))"_l, f0);
      close(f0);
      auto f1 = open(mkstring(test_file), atoms::READ);
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
      CHECK(ch0->as_integer() == 't');
      auto ch1 = readc(f);
      CHECK(ch1->as_integer() == 'e');
      auto ch2 = readc(f);
      CHECK(ch2->as_integer() == 's');
      auto ch3 = readc(f);
      CHECK(ch3->as_integer() == 't');
    }

    SECTION("from primin")
    {
      auto in = ref_file_t::create("a");
      auto old = vm::primin(in);
      auto r = readc(nil);
      REQUIRE(type_of(r) == object::type::Integer);
      CHECK(r->as_integer() == 'a');
      vm::primin(old);
    }

    SECTION("from stdin")
    {
      std::istringstream stream("a");
      auto* buf = std::cin.rdbuf(stream.rdbuf());
      auto r = readc(T);
      REQUIRE(type_of(r) == object::type::Integer);
      CHECK(r->as_integer() == 'a');
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
      auto f0 = open(mkstring(test_file), atoms::WRITE);
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

    SECTION("endoffile")
    {
      create_test_file test{""};
      auto in = open(mkstring(test.file), atoms::READ);
      auto r = readline(in);
      CHECK(r == atoms::ENDOFFILE);
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
      CHECK(r->car()->as_symbol()->pname == "a");
      std::cin.rdbuf(buf);
    }
  }

  SECTION("loadfile")
  {
    vm::loadpath(mklist(atoms::DOT));
    create_test_file test("(setq a \"loadfile\")");
    {
      REQUIRE(loadfile(test.file));
      CHECK("a"_a->value()->as_string() == "loadfile");
    }
    {
      REQUIRE(loadfile(test.file));
      CHECK("a"_a->value()->as_string() == "loadfile");
    }
  }

  SECTION("append")
  {
    create_test_file test("(setq a");
    auto f = open(mkstring(test.file), atoms::APPEND);
    prin1(" 999)"_s, f);
    terpri(f);
    close(f);
    vm::loadpath(mklist(atoms::DOT));
    auto e = load(mkstring(test.file));
    REQUIRE(type_of("a"_a->value()) == object::type::Integer);
    CHECK("a"_a->value()->as_integer() == 999);
  }
}

TEST_CASE("file: ratom")
{
  SECTION("integer")
  {
    auto in = getobject(ref_file_t::create("124"));
    auto r = ratom(in);
    CHECK(type_of(r) == object::type::Integer);
  }

  SECTION("symbol")
  {
    auto in = getobject(ref_file_t::create("124abc"));
    auto r = ratom(in);
    CHECK(type_of(r) == object::type::Symbol);
  }

  SECTION("string")
  {
    // This does not match the behaviour of Interlisp which would return the
    // symbol \".
    auto in = getobject(ref_file_t::create(R"("hello")"));
    auto r = ratom(in);
    CHECK(type_of(r) == object::type::String);
  }
}

TEST_CASE("file: lispread/readline")
{
  SECTION("Read from utf-8")
  {
    std::string s_nihongo{"\"日本語\"\n"};
    auto nihongo = lispread(s_nihongo);
    file_t out(std::make_unique<io::string_sink>());
    print(nihongo, out);
    CHECK(to_string(out.sink()) == s_nihongo);
  }

  SECTION("Read from utf-8 2")
  {
    std::string s_nihongo{
      R"((((field "payee") (re "ライゼボツクス") (category "Housing/Storage")) ((field "payee") (re "ビューカード") (category "Transfer/viewcard")) ((field "payee") (re "楽天コミュニケー") (category "Utilities/Phone")))
)"};
    auto nihongo = lispread(s_nihongo);
    file_t out(std::make_unique<io::string_sink>());
    print(nihongo, out);
    CHECK(to_string(out.sink()) == s_nihongo);
  }

  SECTION("Read utf-8 from file")
  {
    constexpr const char* test_file{"test.lisp"};
    std::string s_nihongo{
      R"((((field "payee") (re "ライゼボツクス") (category "Housing/Storage")) ((field "payee") (re "ビューカード") (category "Transfer/viewcard")) ((field "payee") (re "楽天コミュニケー") (category "Utilities/Phone")))
)"};
    {
      std::ofstream o{test_file};
      o << s_nihongo;
    }
    auto f = ref_file_t::create(std::make_unique<io::file_source>(test_file));
    auto nihongo = lispread(f);
    file_t out(std::make_unique<io::string_sink>());
    print(nihongo, out);
    CHECK(to_string(out.sink()) == s_nihongo);
    std::filesystem::remove(test_file);
  }

  SECTION("lispread vs. readline")
  {
    auto result0 = lispread(R"((hello world))");
    auto f1 = ref_file_t::create(R"(hello world)");
    auto result1 = readline(f1);
    CHECK(equal(result0, result1) != nil);
  }

  SECTION("floatp")
  {
    auto f0 = lispread("-1.2345E-2");
    CHECK(f0->as_double() == Catch::Approx(-1.2345E-2).epsilon(0.01));
  }

  SECTION("lispread & readline")
  {
    std::string s0{"hello"};
    auto r0 = lispread(s0);
    CHECK(r0 == "hello"_a);
    auto f1 = ref_file_t::create(s0);
    auto r1 = readline(f1);
    CHECK(r0 == "hello"_a);
  }

  SECTION("readline eof")
  {
    auto r0 = readline(ref_file_t::create(""));
    CHECK(r0 == atoms::ENDOFFILE);
  }

  SECTION("readline expression")
  {
    auto r = readline(ref_file_t::create("(a b c)"));
    CHECK(listp(r) != nil);
  }

  SECTION("readline three expressions")
  {
    auto r = readline(ref_file_t::create("a b c"));
    CHECK(eq(length(r), 3_l));
  }
}

TEST_CASE("file: file_t functions")
{
  file_t f_out(std::make_unique<io::string_sink>());
  file_t f_in(std::make_unique<io::string_source>("hello"));

  SECTION("patom")
  {
    patom("hello"_a, f_out);
    terpri(f_out);
    patom("world"_a, f_out);
    terpri(f_out);
    CHECK(to_string(f_out.sink()) == "hello\nworld\n");
  }

  SECTION("puts")
  {
    f_out.puts("hello");
    CHECK(to_string(f_out.sink()) == "hello");
  }

  SECTION("move file_t")
  {
    file_t f = std::move(f_out);
    CHECK(!f_out.has_sink());
    CHECK(f.has_sink());
  }

  SECTION("ungetch")
  {
    auto c = f_in.getch();
    CHECK(c == 'h');
    c = f_in.getch();
    CHECK(c == 'e');
    f_in.ungetch(c);
    c = 0;
    c = f_in.getch();
    CHECK(c == 'e');
  }

  SECTION("getline")
  {
    std::ifstream is{"/dev/null"};
    auto f = getobject(ref_file_t::create(is));
    auto g = getline(f);
    CHECK(!g);
  }
}

TEST_CASE("file: patom primout/primerr")
{
  {
    std::ostringstream out;
    auto old = vm::primout(ref_file_t::create(out));
    patom("foo"_a, io::output::PRIMARY);
    terpri(io::output::PRIMARY);
    CHECK(out.str() == "foo\n");
    vm::primout(old);
  }
  {
    std::ostringstream err;
    auto old = vm::primerr(ref_file_t::create(err));
    patom("bar"_a, io::output::ERROR);
    terpri(io::output::ERROR);
    CHECK(err.str() == "bar\n");
    vm::primerr(old);
  }
  {
    std::ostringstream out;
    auto old = vm::primout(ref_file_t::create(out));
    patom("foo"_a, io::output::PRIMARY);
    terpri(io::output::PRIMARY);
    CHECK(out.str() == "foo\n");
    vm::primout(old);
  }
  {
    std::ostringstream err;
    auto old = vm::primerr(ref_file_t::create(err));
    patom("bar"_a, io::output::ERROR);
    terpri(io::output::ERROR);
    CHECK(err.str() == "bar\n");
    vm::primerr(old);
  }
}

TEST_CASE("file: prinbody")
{
  auto list = "(a b c . d)"_l;
  {
    std::ostringstream os;
    auto f = std::make_unique<file_t>(os);
    prinbody(list, *f);
    CHECK(os.str() == "a b c . d");
  }
  {
    std::ostringstream os;
    auto f = std::make_unique<file_t>(os);
    prinbody(list, *f);
    CHECK(os.str() == "a b c . d");
  }
  {
    std::ostringstream os;
    auto old = vm::primout(ref_file_t::create(os));
    prinbody(list, io::output::PRIMARY);
    CHECK(os.str() == "a b c . d");
    vm::primout(old);
  }
  {
    std::ostringstream os;
    auto old = vm::primerr(ref_file_t::create(os));
    prinbody(list, io::output::ERROR);
    CHECK(os.str() == "a b c . d");
    vm::primerr(old);
  }
  {
    std::ostringstream os;
    auto old = vm::primout(ref_file_t::create(os));
    prinbody(list, io::output::PRIMARY);
    CHECK(os.str() == "a b c . d");
    vm::primout(old);
  }
  {
    std::ostringstream os;
    auto old = vm::primerr(ref_file_t::create(os));
    prinbody(list, io::output::ERROR);
    CHECK(os.str() == "a b c . d");
    vm::primerr(old);
  }
}

TEST_CASE("file: prin0")
{
  {
    std::ostringstream os;
    auto f = std::make_unique<file_t>(os);
    prin0(nil, *f);
    CHECK(os.str() == "nil");
  }
  {
    std::ostringstream os;
    auto f = std::make_unique<file_t>(os);
    prin0(T, *f);
    CHECK(os.str() == "t");
  }
  {
    std::ostringstream os;
    auto f = std::make_unique<file_t>(os);
    prin0(1.0_l, *f);
    CHECK(os.str() == "1.00000");
  }
  {
    std::ostringstream os;
    auto f = std::make_unique<file_t>(os);
    prin0(lambda("a"_a, nil), *f);
    CHECK(os.str().starts_with("#<lambda"));
  }
  {
    std::ostringstream os;
    auto f = std::make_unique<file_t>(os);
    prin0(nlambda("a"_a, nil), *f);
    CHECK(os.str().starts_with("#<nlambda"));
  }
  {
    std::ostringstream os;
    auto f = std::make_unique<file_t>(os);
    prin0(closure(lambda(nil, "(a)"_l), "(a)"_l), *f);
    CHECK(os.str().starts_with("#<closure"));
  }
  {
    std::ostringstream os;
    auto f = std::make_unique<file_t>(os);
    prin0("plus"_e, *f);
    CHECK(os.str().starts_with("#<subr"));
  }
  {
    std::ostringstream os;
    auto f = std::make_unique<file_t>(os);
    prin0("quote"_e, *f);
    CHECK(os.str().starts_with("#<fsubr"));
  }
  {
    std::ostringstream os;
    auto f = std::make_unique<file_t>(os);
    prin0(atoms::UNBOUND, *f);
    CHECK(os.str() == "unbound");
  }
  {
    std::ostringstream os;
    auto f = std::make_unique<file_t>(os);
    prin0(atoms::ENDOFFILE, *f);
    CHECK(os.str() == "endoffile");
  }
  {
    std::ostringstream os;
    auto f = std::make_unique<file_t>(os);
    prin0(atoms::ERROR, *f);
    CHECK(os.str() == "error");
  }
  {
    std::ostringstream os;
    auto out = ref_file_t::create(os);
    auto old = vm::primerr(out);
    prin0(nil, io::output::ERROR);
    CHECK(os.str() == "nil");
    vm::primerr(old);
  }
}

TEST_CASE("file: print")
{
  std::ostringstream os;
  auto out = ref_file_t::create(os);
  auto old = vm::primerr(out);
  print(nil, io::output::ERROR);
  CHECK(os.str() == "nil\n");
  vm::primerr(old);
}

TEST_CASE("file: splice")
{
  {
    auto x = "(a b c)"_l;
    auto y = "(x y z)"_l;
    auto r = splice(x, y, nil);
    CHECK(!is_nil(equal(x, "(x y z b c)"_l)));
    CHECK(!is_nil(equal(r, "(z b c)"_l)));
  }
  {
    auto x = "(a b c)"_l;
    auto y = "(x y z)"_l;
    auto r = splice(cdr(x), y, nil);
    CHECK(!is_nil(equal(x, "(a x y z c)"_l)));
    CHECK(!is_nil(equal(r, "(z c)"_l)));
  }
  {
    auto x = "(a b c)"_l;
    auto y = "(x y z)"_l;
    auto r = splice(cdr(x), y, T);
    CHECK(!is_nil(equal(x, "(a b x y z c)"_l)));
    CHECK(!is_nil(equal(r, "(z c)"_l)));
  }
  {
    auto x = "(a b c)"_l;
    auto y = "x"_l;
    auto r = splice(x, y, nil);
    CHECK(!is_nil(equal(x, "(x b c)"_l)));
    CHECK(!is_nil(equal(r, "(x b c)"_l)));
  }
  {
    auto x = "(a b c)"_l;
    auto y = "x"_l;
    auto r = splice(x, y, T);
    CHECK(!is_nil(equal(x, "(a x b c)"_l)));
    CHECK(!is_nil(equal(r, "(a x b c)"_l)));
  }
  {
    auto x = "(a b c)"_l;
    auto r = splice(x, nil, T);
    CHECK(!is_nil(equal(r, x)));
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
    CHECK_THROWS(open(mkstring(test.file), atoms::CONS));
  }

  SECTION("open non-existing file") { CHECK_THROWS(open(mkstring("/etc/xyzzy"), nil)); }
}

TEST_CASE("file: file move assignment")
{
  std::ostringstream os;
  auto out = ref_file_t::create(os);
  CHECK(out->has_sink());
  CHECK(!out->has_source());
  auto file = ref_file_t::create("input");
  CHECK(file->has_source());
  CHECK(!file->has_sink());
  *file = std::move(*out);
  CHECK(!file->has_source());
  CHECK(file->has_sink());
  CHECK(!out->has_source());
  CHECK(!out->has_sink());
}

} // namespace lisp

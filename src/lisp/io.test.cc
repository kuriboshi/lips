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

#include <filesystem>
#include <functional>

#include <catch2/catch.hpp>

#include "alloc.hh"
#include "io.hh"
#include "pred.hh"
#include "prim.hh"

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

TEST_CASE("io: basic i/o")
{
  auto& ctx = context::current();
  auto old = ctx.primout(ref_file_t::create(std::make_unique<io::string_sink>()));
  ctx.primout()->format("hello world {}", 123);
  CHECK(to_string(ctx.primout()->sink()) == std::string("hello world 123"));
  ctx.primout(old);
}

TEST_CASE("io: ratom")
{
  SECTION("integer")
  {
    auto in = ref_file_t::create("124");
    auto r = ratom(in);
    CHECK(type_of(r) == type::Integer);
  }
  SECTION("symbol")
  {
    {
      auto in = ref_file_t::create("124abc");
      auto r = ratom(in);
      CHECK(type_of(r) == type::Symbol);
    }
    {
      auto in = ref_file_t::create("124abc");
      auto r = ratom(in);
      CHECK(type_of(r) == type::Symbol);
    }
  }
  SECTION("string")
  {
    // This does not match the behaviour of Interlisp which would return the
    // symbol \".
    auto in = ref_file_t::create(R"("hello")");
    auto r = ratom(in);
    CHECK(type_of(r) == type::String);
  }
}

TEST_CASE("io: lispread/readline")
{
  SECTION("Read from utf-8")
  {
    std::string s_nihongo{"\"日本語\"\n"};
    auto in = ref_file_t::create(s_nihongo);
    auto nihongo = lispread(in);
    file_t out(std::make_unique<io::string_sink>());
    print(nihongo, out);
    CHECK(to_string(out.sink()) == s_nihongo);
  }

  SECTION("Read from utf-8 2")
  {
    std::string s_nihongo{
      R"((((field "payee") (re "ライゼボツクス") (category "Housing/Storage")) ((field "payee") (re "ビューカード") (category "Transfer/viewcard")) ((field "payee") (re "楽天コミュニケー") (category "Utilities/Phone")))
)"};
    auto in = ref_file_t::create(s_nihongo);
    auto nihongo = lispread(in);
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
    auto f0 = ref_file_t::create(R"((hello world))");
    auto result0 = lispread(f0);
    auto f1 = ref_file_t::create(R"(hello world)");
    auto result1 = readline(f1);
    CHECK(equal(result0, result1) != NIL);
  }

  SECTION("floatp")
  {
    auto f0 = lispread("-1.2345E-2");
    CHECK(f0->floatval() == Approx(-1.2345E-2).epsilon(0.01));
  }

  SECTION("lispread & readline")
  {
    std::string s0{"hello"};
    auto f0 = ref_file_t::create(s0);
    auto r0 = lispread(f0);
    CHECK(r0 == "hello"_a);
    auto f1 = ref_file_t::create(s0);
    auto r1 = readline(f1);
    CHECK(r0 == "hello"_a);
  }
}

TEST_CASE("io: source/sink")
{
  file_t f0(std::make_unique<io::string_source>("(a)"));
  CHECK(f0.has_source());
  CHECK(!f0.has_sink());
  CHECK_THROWS_WITH(f0.terpri(), "file_t: No sink");
  file_t f1(std::make_unique<io::string_sink>());
  CHECK(!f1.has_source());
  CHECK(f1.has_sink());
  CHECK_THROWS_WITH(f1.getline(), "file_t: No source");
}

TEST_CASE("io: file_t functions")
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
}

TEST_CASE("io: source")
{
  SECTION("io::file_source")
  {
    create_test_file test("#!\n");
    io::file_source f{test.file};
    auto c = f.getch();
    CHECK(c == '#');
    f.ungetch(c);
    c = f.getch();
    CHECK(c == '#');
    c = f.getch();
    CHECK(c == '!');
    f.close();
  }

  SECTION("io::stream_source")
  {
    create_test_file test("#!\n");
    {
      std::ifstream is{test.file};
      io::stream_source f{is};
      auto c = f.getch();
      CHECK(c == '#');
      f.ungetch(c);
      c = f.getch();
      CHECK(c == '#');
      c = f.getch();
      CHECK(c == '!');
      f.close();
    }
    {
      std::ifstream is{test.file};
      io::stream_source f{is};
      auto l = f.getline();
      REQUIRE(l);
      CHECK(*l == "#!");
    }
    {
      std::ifstream is{test.file};
      io::stream_source f{is};
      auto b = f.begin();
      CHECK(*b == '#');
      ++b;
      CHECK(*b == '!');
      ++b;
      CHECK(*b == '\n');
      ++b;
      CHECK(b == f.end());
    }
    {
      std::ifstream is{"/dev/null"};
      io::stream_source f{is};
      auto g = f.getline();
      CHECK(!g);
    }
  }

  SECTION("io::string_source")
  {
    io::string_source ss{"#!\n"};
    auto c = ss.getch();
    CHECK(c == '#');
    ss.ungetch(c);
    c = ss.getch();
    CHECK(c == '#');
    ss.close();
  }
}

TEST_CASE("io: sink")
{
  SECTION("io::file_sink")
  {
    create_test_file test("");
    io::file_sink f(test.file);
    f.puts("hello");
    f.terpri();
    f.close();
    std::ifstream fs(test.file);
    std::ostringstream ss;
    ss << fs.rdbuf();
    CHECK(ss.str() == "hello\n");
  }

  SECTION("io::stream_sink")
  {
    create_test_file test("world");
    std::ofstream of(test.file);
    io::stream_sink f(of);
    f.puts("hello");
    f.terpri();
    f.close();
    std::ifstream fs(test.file);
    std::ostringstream ss;
    ss << fs.rdbuf();
    CHECK(ss.str() == "hello\n");
  }

  SECTION("io::string_sink")
  {
    io::string_sink ss;
    ss.putch('\03', false);
    ss.terpri();
    ss.close();
    CHECK(ss.string() == "^C\n");
  }
}

TEST_CASE("io: patom primout/primerr")
{
  auto& ctx = context::current();
  {
    std::ostringstream out;
    auto old = ctx.primout(ref_file_t::create(out));
    patom("foo"_a, false);
    terpri(false);
    CHECK(out.str() == "foo\n");
    ctx.primout(old);
  }
  {
    std::ostringstream err;
    auto old = ctx.primerr(ref_file_t::create(err));
    patom("bar"_a, true);
    terpri(true);
    CHECK(err.str() == "bar\n");
    ctx.primerr(old);
  }
  {
    std::ostringstream out;
    auto old = ctx.primout(ref_file_t::create(out));
    patom("foo"_a, false);
    terpri(false);
    CHECK(out.str() == "foo\n");
    ctx.primout(old);
  }
  {
    std::ostringstream err;
    auto old = ctx.primerr(ref_file_t::create(err));
    patom("bar"_a, true);
    terpri(true);
    CHECK(err.str() == "bar\n");
    ctx.primerr(old);
  }
}

TEST_CASE("io: prinbody")
{
  auto& ctx = context::current();
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
    auto old = ctx.primout(ref_file_t::create(os));
    prinbody(list, false);
    CHECK(os.str() == "a b c . d");
    ctx.primout(old);
  }
  {
    std::ostringstream os;
    auto old = ctx.primerr(ref_file_t::create(os));
    prinbody(list, true);
    CHECK(os.str() == "a b c . d");
    ctx.primerr(old);
  }
  {
    std::ostringstream os;
    auto old = ctx.primout(ref_file_t::create(os));
    prinbody(list, false);
    CHECK(os.str() == "a b c . d");
    ctx.primout(old);
  }
  {
    std::ostringstream os;
    auto old = ctx.primerr(ref_file_t::create(os));
    prinbody(list, true);
    CHECK(os.str() == "a b c . d");
    ctx.primerr(old);
  }
}

TEST_CASE("io: prin0")
{
  {
    std::ostringstream os;
    auto f = std::make_unique<file_t>(os);
    prin0(NIL, *f);
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
    prin0(lambda("a"_a, NIL), *f);
    CHECK(os.str().starts_with("#<lambda"));
  }
  {
    std::ostringstream os;
    auto f = std::make_unique<file_t>(os);
    prin0(nlambda("a"_a, NIL), *f);
    CHECK(os.str().starts_with("#<nlambda"));
  }
  {
    std::ostringstream os;
    auto f = std::make_unique<file_t>(os);
    prin0(closure(lambda(NIL, "(a)"_l), "(a)"_l), *f);
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
    prin0(C_UNBOUND, *f);
    CHECK(os.str() == "#<unbound>");
  }
  {
    std::ostringstream os;
    auto f = std::make_unique<file_t>(os);
    prin0(C_EOF, *f);
    CHECK(os.str().starts_with("#<endoffile"));
  }
  {
    std::ostringstream os;
    auto f = std::make_unique<file_t>(os);
    prin0(C_ERROR, *f);
    CHECK(os.str().starts_with("#<error"));
  }
  {
    std::ostringstream os;
    auto out = ref_file_t::create(os);
    auto old = context::current().primerr(out);
    prin0(NIL, true);
    CHECK(os.str() == "nil");
    context::current().primerr(old);
  }
}

TEST_CASE("io: print")
{
  std::ostringstream os;
  auto out = ref_file_t::create(os);
  auto old = context::current().primerr(out);
  print(NIL, true);
  CHECK(os.str() == "nil\n");
  context::current().primerr(old);
}

TEST_CASE("io: splice")
{
  auto& ctx = context::current();
  {
    auto x = "(a b c)"_l;
    auto y = "(x y z)"_l;
    auto r = io::splice(ctx, x, y, false);
    CHECK(!is_NIL(equal(x, "(x y z b c)"_l)));
    CHECK(!is_NIL(equal(r, "(z b c)"_l)));
  }
  {
    auto x = "(a b c)"_l;
    auto y = "(x y z)"_l;
    auto r = io::splice(ctx, cdr(x), y, false);
    CHECK(!is_NIL(equal(x, "(a x y z c)"_l)));
    CHECK(!is_NIL(equal(r, "(z c)"_l)));
  }
  {
    auto x = "(a b c)"_l;
    auto y = "(x y z)"_l;
    auto r = io::splice(ctx, cdr(x), y, true);
    CHECK(!is_NIL(equal(x, "(a b x y z c)"_l)));
    CHECK(!is_NIL(equal(r, "(z c)"_l)));
  }
  {
    auto x = "(a b c)"_l;
    auto y = "x"_l;
    auto r = io::splice(ctx, x, y, false);
    CHECK(!is_NIL(equal(x, "(x b c)"_l)));
    CHECK(!is_NIL(equal(r, "(x b c)"_l)));
  }
  {
    auto x = "(a b c)"_l;
    auto y = "x"_l;
    auto r = io::splice(ctx, x, y, true);
    CHECK(!is_NIL(equal(x, "(a x b c)"_l)));
    CHECK(!is_NIL(equal(r, "(a x b c)"_l)));
  }
}

} // namespace lisp

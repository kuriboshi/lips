//
// Lips, lisp shell.
// Copyright 2021-2022 Krister Joas
//
#include <catch2/catch.hpp>
#include <filesystem>
#include <lisp/liblisp.hh>

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

TEST_CASE("io: Basic I/O")
{
  lisp lisp;
  current c(lisp);

  lisp.primout(std::make_unique<file_t>(std::make_unique<io::string_sink>()));
  lisp.primout().format("hello world {}", 123);
  CHECK(to_string(lisp.primout().sink()) == std::string("hello world 123"));
}

TEST_CASE("io: ratom")
{
  lisp l;
  current c(l);

  SECTION("integer")
  {
    std::string s{"124"};
    file_t in{s};
    auto r = ratom(in);
    CHECK(type_of(r) == type::INTEGER);
  }
  SECTION("symbol")
  {
    {
      std::string s{"124abc"};
      file_t in{s};
      auto r = ratom(in);
      CHECK(type_of(r) == type::SYMBOL);
    }
    {
      std::string s{"124abc"};
      file_t in{s};
      auto r = ratom(l, in);
      CHECK(type_of(r) == type::SYMBOL);
    }
  }
  SECTION("string")
  {
    // This does not match the behaviour of Interlisp which would return the
    // symbol \".
    std::string s{"\"hello\""};
    file_t in{s};
    auto r = ratom(in);
    CHECK(type_of(r) == type::STRING);
  }
}

TEST_CASE("io: lispread/readline")
{
  lisp l;
  current c(l);

  SECTION("Read from utf-8")
  {
    std::string s_nihongo{"\"日本語\"\n"};
    file_t in{s_nihongo};
    auto nihongo = lispread(l, in);
    file_t out(std::make_unique<io::string_sink>());
    print(l, nihongo, out);
    CHECK(to_string(out.sink()) == s_nihongo);
  }

  SECTION("Read from utf-8 2")
  {
    std::string s_nihongo{R"((((field "payee") (re "ライゼボツクス") (category "Housing/Storage")) ((field "payee") (re "ビューカード") (category "Transfer/viewcard")) ((field "payee") (re "楽天コミュニケー") (category "Utilities/Phone")))
)"};
    file_t in{s_nihongo};
    auto nihongo = lispread(l, in);
    file_t out(std::make_unique<io::string_sink>());
    print(l, nihongo, out);
    CHECK(to_string(out.sink()) == s_nihongo);
  }

  SECTION("Read utf-8 from file")
  {
    constexpr const char* test_file{"test.lisp"};
    std::string s_nihongo{R"((((field "payee") (re "ライゼボツクス") (category "Housing/Storage")) ((field "payee") (re "ビューカード") (category "Transfer/viewcard")) ((field "payee") (re "楽天コミュニケー") (category "Utilities/Phone")))
)"};
    {
      std::ofstream o{test_file};
      o << s_nihongo;
    }
    auto f = file_t(std::make_unique<io::file_source>(test_file));
    auto nihongo = lispread(l, f);
    file_t out(std::make_unique<io::string_sink>());
    print(l, nihongo, out);
    CHECK(to_string(out.sink()) == s_nihongo);
    std::filesystem::remove(test_file);
  }

  SECTION("lispread vs. readline")
  {
    std::string s0{R"((hello world))"};
    auto f0 = file_t(s0);
    auto result0 = lispread(l, f0);
    std::string s1{R"(hello world)"};
    auto f1 = file_t(s1);
    auto result1 = readline(l, f1);
    CHECK(equal(l, result0, result1) != NIL);
  }

  SECTION("floatp")
  {
    auto f0 = lispread("-1.2345E-2");
    CHECK(f0->floatval() == Approx(-1.2345E-2).epsilon(0.01));
  }

  SECTION("lispread & readline")
  {
    std::string s0{"hello"};
    auto f0 = file_t(s0);
    auto r0 = lispread(f0);
    CHECK(r0 == "hello"_a);
    auto f1 = file_t(s0);
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
  lisp l;
  current c(l);

  file_t f0(std::make_unique<io::string_sink>());
  patom("hello"_a, f0);
  terpri(f0);
  patom(l, "world"_a, f0);
  terpri(l, f0);
  CHECK(to_string(f0.sink()) == "hello\nworld\n");
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
    CHECK(f.close());
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
      CHECK(f.close());
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
  }

  SECTION("io::string_source")
  {
    io::string_source ss{"#!\n"};
    auto c = ss.getch();
    CHECK(c == '#');
    ss.ungetch(c);
    c = ss.getch();
    CHECK(c == '#');
    CHECK(ss.close());
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
    CHECK(f.close());
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
    CHECK(f.close());
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
  lisp l;
  current c(l);

  {
    std::ostringstream out;
    l.primout(std::make_unique<file_t>(out));
    patom(l, "foo"_a, false);
    terpri(l, false);
    CHECK(out.str() == "foo\n");
  }
  {
    std::ostringstream err;
    l.primerr(std::make_unique<file_t>(err));
    patom(l, "bar"_a, true);
    terpri(l, true);
    CHECK(err.str() == "bar\n");
  }
  {
    std::ostringstream out;
    l.primout(std::make_unique<file_t>(out));
    patom("foo"_a, false);
    terpri(false);
    CHECK(out.str() == "foo\n");
  }
  {
    std::ostringstream err;
    l.primerr(std::make_unique<file_t>(err));
    patom("bar"_a, true);
    terpri(true);
    CHECK(err.str() == "bar\n");
  }
}

TEST_CASE("io: prinbody")
{
  lisp l;
  current c(l);

  auto list = "(a b c . d)"_l;
  {
    std::ostringstream os;
    auto f = std::make_unique<file_t>(os);
    prinbody(l, list, *f);
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
    l.primout(std::make_unique<file_t>(os));
    prinbody(l, list, false);
    CHECK(os.str() == "a b c . d");
  }
  {
    std::ostringstream os;
    l.primerr(std::make_unique<file_t>(os));
    prinbody(l, list, true);
    CHECK(os.str() == "a b c . d");
  }
  {
    std::ostringstream os;
    l.primout(std::make_unique<file_t>(os));
    prinbody(list, false);
    CHECK(os.str() == "a b c . d");
  }
  {
    std::ostringstream os;
    l.primerr(std::make_unique<file_t>(os));
    prinbody(list, true);
    CHECK(os.str() == "a b c . d");
  }
}

TEST_CASE("io: prin0")
{
  {
    std::ostringstream os;
    auto f = std::make_unique<file_t>(os);
    prin0(C_EMPTY, *f);
    CHECK(os.str().empty());
  }
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
}

TEST_CASE("io: Read tables")
{
  lisp l;
  current c(l);

  CHECK(is_ascii(0));
  CHECK(is_ascii(127));
  CHECK(!is_ascii(128));

  CHECK(is_sepr(l, ' '));
  CHECK(is_sepr(' '));
  CHECK(is_brk(l, '('));
  CHECK(is_brk('('));
  CHECK(is_ctrl(l, '\b'));
  CHECK(is_ctrl('\a'));
  CHECK(is_insert(l, '"'));
  CHECK(is_insert('"'));
  CHECK(is_splice(l, '!'));
  CHECK(is_splice('!'));
  CHECK(!is_infix(l, '$'));
  CHECK(!is_infix('$'));
}

TEST_CASE("io: splice")
{
  lisp l;
  current c(l);
  {
    auto x = "(a b c)"_l;
    auto y = "(x y z)"_l;
    auto r = io::splice(l, x, y, false);
    CHECK(!is_NIL(equal(x, "(x y z b c)"_l)));
    CHECK(!is_NIL(equal(r, "(z b c)"_l)));
  }
  {
    auto x = "(a b c)"_l;
    auto y = "(x y z)"_l;
    auto r = io::splice(l, cdr(l, x), y, false);
    CHECK(!is_NIL(equal(x, "(a x y z c)"_l)));
    CHECK(!is_NIL(equal(r, "(z c)"_l)));
  }
  {
    auto x = "(a b c)"_l;
    auto y = "(x y z)"_l;
    auto r = io::splice(l, cdr(l, x), y, true);
    CHECK(!is_NIL(equal(x, "(a b x y z c)"_l)));
    CHECK(!is_NIL(equal(r, "(z c)"_l)));
  }
  {
    auto x = "(a b c)"_l;
    auto y = "x"_l;
    auto r = io::splice(l, x, y, false);
    CHECK(!is_NIL(equal(x, "(x b c)"_l)));
    CHECK(!is_NIL(equal(r, "(x b c)"_l)));
  }
  {
    auto x = "(a b c)"_l;
    auto y = "x"_l;
    auto r = io::splice(l, x, y, true);
    CHECK(!is_NIL(equal(x, "(a x b c)"_l)));
    CHECK(!is_NIL(equal(r, "(a x b c)"_l)));
  }
}

}

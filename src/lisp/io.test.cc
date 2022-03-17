//
// Lips, lisp shell.
// Copyright 2021-2022 Krister Joas
//
#include <catch2/catch.hpp>
#include <filesystem>
#include "libisp.hh"

namespace lisp
{

TEST_CASE("Basic I/O")
{
  lisp lisp;
  current c(lisp);

  lisp.primout(std::make_unique<file_t>(std::make_unique<string_sink>()));
  lisp.primout().format("hello world {}", 123);
  CHECK(to_string(lisp.primout().sink()) == std::string("hello world 123"));
}

TEST_CASE("Read lisp objects")
{
  lisp lisp;
  current c(lisp);

  SECTION("Read from utf-8")
  {
    std::string s_nihongo{"\"日本語\"\n"};
    file_t in{s_nihongo};
    auto nihongo = lispread(lisp, in);
    file_t out(std::make_unique<string_sink>());
    print(lisp, nihongo, out);
    CHECK(to_string(out.sink()) == s_nihongo);
  }

  SECTION("Read from utf-8 2")
  {
    std::string s_nihongo{R"((((field "payee") (re "ライゼボツクス") (category "Housing/Storage")) ((field "payee") (re "ビューカード") (category "Transfer/viewcard")) ((field "payee") (re "楽天コミュニケー") (category "Utilities/Phone")))
)"};
    file_t in{s_nihongo};
    auto nihongo = lispread(lisp, in);
    file_t out(std::make_unique<string_sink>());
    print(lisp, nihongo, out);
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
    auto f = file_t(std::make_unique<file_source>(test_file));
    auto nihongo = lispread(lisp, f);
    file_t out(std::make_unique<string_sink>());
    print(lisp, nihongo, out);
    CHECK(to_string(out.sink()) == s_nihongo);
    std::filesystem::remove(test_file);
  }

  SECTION("lispread vs. readline")
  {
    std::string s0{R"((hello world))"};
    auto f0 = file_t(s0);
    auto result0 = lispread(lisp, f0);
    std::string s1{R"(hello world)"};
    auto f1 = file_t(s1);
    auto result1 = readline(lisp, f1);
    CHECK(equal(lisp, result0, result1) != NIL);
  }

  SECTION("floatp")
  {
    auto f0 = lispread("-1.2345E-2");
    CHECK(f0->floatval() == Approx(-1.2345E-2).epsilon(0.01));
  }
}

}

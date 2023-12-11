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

#include <filesystem>
#include <functional>

#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers.hpp>

#include "alloc.hh"
#include "io.hh"
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

TEST_CASE("io: basic i/o")
{
  auto old = vm::primout(ref_file_t::create(std::make_unique<io::string_sink>()));
  vm::primout()->format("hello world {}", 123);
  CHECK(to_string(vm::primout()->sink()) == std::string("hello world 123"));
  vm::primout(old);
}

TEST_CASE("io: source/sink")
{
  file_t f0(std::make_unique<io::string_source>("(a)"));
  CHECK(f0.has_source());
  CHECK(!f0.has_sink());
  CHECK_THROWS_WITH(f0.terpri(), "No sink");
  file_t f1(std::make_unique<io::string_sink>());
  CHECK(!f1.has_source());
  CHECK(f1.has_sink());
  CHECK_THROWS_WITH(f1.getline(), "No source");
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
      auto b = begin(f);
      CHECK(*b == '#');
      ++b;
      CHECK(*b == '!');
      ++b;
      CHECK(*b == '\n');
      ++b;
      CHECK(b == end(f));
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
    ss.putch('\03', io::escape::NO);
    ss.terpri();
    ss.close();
    CHECK(ss.string() == "^C\n");
  }
}

} // namespace lisp

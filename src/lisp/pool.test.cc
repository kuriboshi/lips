//
// Lips, lisp shell
// Copyright 2022 Krister Joas
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

#include <catch2/catch_test_macros.hpp>

#include "pool.hh"

namespace lisp
{
class Test
{
public:
  explicit Test(bool t = false)
  {
    // This is to cover operator delete(void*)
    if(t)
      throw std::runtime_error("Test");
  }
  ~Test()
  {
    t0 = 0;
    t1 = 0;
    t2 = 0;
  }
  static void* operator new(std::size_t) { return _pool.allocate(); }
  static void operator delete(void* x) { _pool.deallocate(x); }
  static void operator delete(Test* x, std::destroying_delete_t) { _pool.deallocate(x); }
  int t0 = 1;
  int t1 = 12;
  int t2 = 123;
  using pool_t = ::lisp::pool<Test, 2>;
  static const pool_t& pool() { return _pool; };

private:
  static pool_t _pool;
};

Test::pool_t Test::_pool;

TEST_CASE("pool: simple")
{
  auto& p = Test::pool();
  CHECK(p.size() == 0);

  auto* t = new Test;
  REQUIRE(t != nullptr);
  CHECK(p.size() == 1);
  CHECK(t->t0 == 1);
  CHECK(t->t1 == 12);
  CHECK(t->t2 == 123);
  t->t0 = 99;
  t->t1 = 199;
  t->t2 = 321;
  delete t;
  CHECK(p.size() == 2);

  auto* t0 = new Test;
  CHECK(t0->t0 == 1);
  CHECK(t0->t1 == 12);
  CHECK(t0->t2 == 123);
  CHECK(p.size() == 1);
  auto* t1 = new Test;
  CHECK(p.size() == 0);
  auto* t2 = new Test;
  CHECK(p.size() == 1);
  delete t2;
  CHECK(p.size() == 2);
  delete t1;
  CHECK(p.size() == 3);
  delete t0;
  CHECK(p.size() == 4);

  auto* n = new Test;
  REQUIRE(n != nullptr);
  CHECK(p.size() == 3);
  delete n;
  CHECK(p.size() == 4);

  CHECK(p.size() == 4);
}

TEST_CASE("pool: exception thrown in constructor")
{
  SECTION("Test")
  {
    auto c0 = Test::pool().size();
    CHECK_THROWS(new Test(true));
    CHECK(c0 == Test::pool().size());
  }
}

} // namespace lisp

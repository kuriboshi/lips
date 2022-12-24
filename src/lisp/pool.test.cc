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

#include <array>
#include <chrono>
#include <cstdint>
#include <iostream>
#include <string>

#include <catch2/catch.hpp>

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
  static pool_t& pool() { return _pool; };

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

  CHECK_THROWS(new Test(true));
}

template<class T>
auto timing(T t0, T t1) -> decltype(std::chrono::microseconds(0).count())
{
  auto time = std::chrono::duration_cast<std::chrono::microseconds>(t1 - t0).count();
  return time;
}

class Foo
{
public:
  explicit Foo(bool t = false)
  {
    // This is to cover operator delete(void*)
    if(t)
      throw std::runtime_error("Test");
  }
  static void* operator new(std::size_t) { return _pool.allocate(); }
  static void operator delete(void* x) { _pool.deallocate(x); }
  static void operator delete(Foo* x, std::destroying_delete_t) { _pool.deallocate(x); }

private:
  std::string s;
  using pool_t = lisp::pool<Foo, 400>;
  static pool_t _pool;
};

Foo::pool_t Foo::_pool;

class Bar
{
public:
  Bar() = default;

private:
  std::string s;
};

template<typename T, std::size_t N>
auto pool_test() -> decltype(std::chrono::microseconds(0).count())
{
  std::array<const T*, N * 2> a;
  auto t0 = std::chrono::high_resolution_clock::now();
  for(auto i = 0; i != N; ++i)
  {
    a[i] = new T;
  }
  for(auto i = 0; i != N; ++i)
  {
    delete a[i];
  }
  for(auto i = 0; i != N * 2; ++i)
  {
    a[i] = new T;
  }
  for(auto i = 0; i != N * 2; ++i)
  {
    delete a[i];
  }
  auto t1 = std::chrono::high_resolution_clock::now();
  auto t = timing(t0, t1);
  return t;
}

template<std::size_t N, std::size_t M = 10>
std::pair<std::uint64_t, std::uint64_t> do_test()
{
  std::uint64_t f{0};
  std::uint64_t b{0};
  for(auto i = 0; i != M; ++i)
  {
    f += pool_test<Foo, N>();
    b += pool_test<Bar, N>();
  }
  return {f, b};
}

TEST_CASE("pool: speed")
{
  CHECK_THROWS(new Foo(true));
  auto p0 = do_test<100>();
  CHECK(p0.first < (p0.second * 1.50));
  auto p1 = do_test<500>();
  CHECK(p1.first < (p1.second * 1.50));
  auto p2 = do_test<1000>();
  CHECK(p2.first < (p2.second * 1.50));
}

} // namespace lisp

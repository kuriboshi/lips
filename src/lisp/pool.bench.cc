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
#include <string>

#include <catch2/catch_test_macros.hpp>
#include <catch2/benchmark/catch_benchmark.hpp>

#include "pool.hh"

namespace lisp
{

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

  using pool_t = lisp::pool<Foo, 400>;
  static const pool_t& pool() { return _pool; }

private:
  std::string s;
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

TEST_CASE("pool: speed")
{
  BENCHMARK("pool: speed(Foo, 100)") { return pool_test<Foo, 100>(); };

  BENCHMARK("pool: speed(Bar, 100)") { return pool_test<Bar, 100>(); };

  BENCHMARK("pool: speed(Foo, 500)") { return pool_test<Foo, 500>(); };

  BENCHMARK("pool: speed(Bar, 500)") { return pool_test<Bar, 500>(); };

  BENCHMARK("pool: speed(Foo, 1000)") { return pool_test<Foo, 1000>(); };

  BENCHMARK("pool: speed(Bar, 1000)") { return pool_test<Bar, 1000>(); };
}

} // namespace lisp

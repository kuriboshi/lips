//
// Lips, lisp shell
// Copyright 2022 Krister Joas
//
#include <array>
#include <chrono>
#include <iostream>
#include <string>
#include <utility>
#include <catch2/catch.hpp>
#include <lisp/pool.hh>

namespace lisp
{
class Test
{
public:
  Test() = default;
  ~Test()
  {
    t0 = 0;
    t1 = 0;
    t2 = 0;
  }
  void* operator new(std::size_t)
  {
    return _pool.allocate();
  }
  void operator delete(void* x)
  {
    _pool.deallocate(x);
  }
  int t0 = 1;
  int t1 = 12;
  int t2 = 123;
  using pool_t = pool<Test, 2>;
  static pool_t& pool() { return _pool; };
private:
  static pool_t _pool;
};

Test::pool_t Test::_pool;

TEST_CASE("pool: simple")
{
  auto& p = Test::pool();
  CHECK(p.size() == 0);
  auto* t = p.allocate();
  REQUIRE(t != nullptr);
  CHECK(t->t0 == 1);
  CHECK(t->t1 == 12);
  CHECK(t->t2 == 123);
  CHECK(p.size() == 1);
  p.deallocate(t);
  CHECK(p.size() == 2);
  auto t0 = p.allocate();
  CHECK(p.size() == 1);
  auto t1 = p.allocate();
  CHECK(p.size() == 0);
  auto t2 = p.allocate();
  CHECK(p.size() == 1);
  p.deallocate(t2);
  CHECK(p.size() == 2);
  p.deallocate(t1);
  CHECK(p.size() == 3);
  p.deallocate(t0);
  CHECK(p.size() == 4);
  p.deallocate(t);
  CHECK(p.size() == 5);
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
  Foo() = default;
  void* operator new(std::size_t)
  {
    return _pool.allocate();
  }
  void operator delete(Foo* x, std::destroying_delete_t)
  {
    _pool.deallocate(x);
  }
  static std::size_t size() { return _pool.size(); }
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
  for(auto i = 0; i != M; ++i) {
    f += pool_test<Foo, N>();
    b += pool_test<Bar, N>();
  }
  return {f, b};
}

TEST_CASE("pool: speed")
{
  auto p0 = do_test<100>();
  CHECK(p0.first < p0.second);
  auto p1 = do_test<500>();
  CHECK(p1.first < p1.second);
  auto p2 = do_test<1000>();
  CHECK(p2.first < p2.second);
}

}

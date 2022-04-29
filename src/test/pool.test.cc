//
// Lips, lisp shell
// Copyright 2022 Krister Joas
//
#include <array>
#include <chrono>
#include <iostream>
#include <string>
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
auto pool_test(const std::string& title) -> decltype(std::chrono::microseconds(0).count())
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
  std::cout << title << ' ' << t << '\n';
  return t;
}

TEST_CASE("pool: speed")
{
  auto f100 = pool_test<Foo, 100>("with    pool");
  auto f500 = pool_test<Foo, 500>("with    pool");
  auto f1000 = pool_test<Foo, 1000>("with    pool");
  auto b100 = pool_test<Bar, 100>("without pool");
  auto b500 = pool_test<Bar, 500>("without pool");
  auto b1000 = pool_test<Bar, 1000>("without pool");
  CHECK(f100 < b100);
  CHECK(f500 < b500);
  CHECK(f1000 < b1000);
}

}

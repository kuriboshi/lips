#include <iostream>
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

}

#include <catch2/catch_test_macros.hpp>
#include <catch2/catch_session.hpp>
#include <lisp/lisp.hh>

TEST_CASE("package_test")
{
  lisp::vm vm;
  lisp::context context;
  auto result = vm.eval("(plus 1 2 3)");
  REQUIRE(lisp::type_of(result) == lisp::object::type::Integer);
  CHECK(result->intval() == 6);
}

int main()
{
  Catch::Session session;
  return session.run();
}

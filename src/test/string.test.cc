//
// Lips, lisp shell.
// Copyright 2021-2022 Krister Joas
//
#include <catch2/catch.hpp>
#include <lisp/libisp.hh>

namespace lisp
{

TEST_CASE("String functions")
{
  lisp l;
  current c(l);

  SECTION("stringp")
  {
    auto s = mkstring("hello");
    auto r0 = stringp(s);
    CHECK(r0 != NIL);
    CHECK(r0->string() == s->string());
    auto i = mknumber(100);
    auto r1 = stringp(l, i);
    CHECK(r1 == NIL);
  }
  SECTION("streq")
  {
    auto s0 = mkstring("lorem");
    auto s1 = mkstring("lorem");
    auto s2 = mkstring("ipsem");
    auto r0 = streq(s0, s1);
    CHECK(r0 == T);
    auto r1 = streq(l, s0, s2);
    CHECK(r1 == NIL);
  }
  SECTION("concat")
  {
    auto s0 = mkstring("hello ");
    auto s1 = mkstring("world");
    auto s2 = concat(cons(s0, cons(s1, NIL)));
    CHECK(s2->string() == mkstring("hello world")->string());
    auto s3 = concat(l, cons(s0, cons(s1, NIL)));
    CHECK(s3->string() == mkstring("hello world")->string());
  }
  SECTION("strlen")
  {
    auto s0 = mkstring("lorem");
    auto l0 = strlen(s0);
    CHECK(l0->intval() == 5);
    auto l1 = strlen(l, s0);
    CHECK(l1->intval() == 5);
  }
  SECTION("substr")
  {
    auto s0 = mkstring("hello world");
    auto s1 = substr(s0, mknumber(0), mknumber(5));
    REQUIRE(s1 != NIL);
    CHECK(s1->string() == "hello");
    auto s2 = substr(s0, mknumber(6), mknumber(10));
    REQUIRE(s2 != NIL);
    CHECK(s2->string() == "world");
    auto s3 = substr(s0, mknumber(-1), mknumber(5));
    CHECK(s3 == NIL);
    auto s4 = substr(s0, mknumber(0), mknumber(15));
    CHECK(s4 == NIL);
    auto s5 = substr(l, s0, mknumber(0), mknumber(-1));
    CHECK(s5 == NIL);
  }
  SECTION("symstr")
  {
    auto p0 = intern("symbol");
    auto r0 = symstr(p0);
    CHECK(type_of(r0) == type::STRING);
    CHECK(r0->string() == p0->getstr());
    auto r1 = symstr(l, p0);
    CHECK(type_of(r1) == type::STRING);
    CHECK(r1->string() == p0->getstr());
  }
  SECTION("strcmp")
  {
    auto s0 = mkstring("alpha");
    auto s1 = mkstring("zeta");
    auto r0 = strcmp(s0, s1);
    REQUIRE(type_of(r0) == type::INTEGER);
    CHECK(r0->intval() < 0);
    auto r1 = strcmp(s1, s0);
    REQUIRE(type_of(r1) == type::INTEGER);
    CHECK(r1->intval() > 0);
    auto r2 = strcmp(l, s0, s0);
    REQUIRE(type_of(r2) == type::INTEGER);
    CHECK(r2->intval() == 0);
  }
}

}
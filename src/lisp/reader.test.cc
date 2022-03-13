//
// Lips, lisp shell.
// Copyright 2022 Krister Joas
//
#include <doctest/doctest.h>
#include "reader.hh"

using namespace std::literals;

namespace
{
void reader_check(lisp::Reader& reader, enum lisp::token_t::type type, const std::string& token)
{
  auto b = reader.read();
  REQUIRE(b);
  CHECK(b->type == type);
  CHECK(b->token == token);
}
}

namespace lisp
{

TEST_CASE("Reader a.")
{
  Reader reader{"a."};
  reader_check(reader, token_t::type::SYMBOL, "a.");
}

TEST_CASE("Reader .a")
{
  Reader reader{".a"};
  reader_check(reader, token_t::type::SYMBOL, ".a");
}

TEST_CASE("Reader a01")
{
  Reader reader{"a01"};
  reader_check(reader, token_t::type::SYMBOL, "a01");
}

TEST_CASE("Reader::read (a b c)")
{
  Reader reader{"(a b c)"};
  reader_check(reader, token_t::type::MACRO, "("s);
  reader_check(reader, token_t::type::SYMBOL, "a"s);
  reader_check(reader, token_t::type::SYMBOL, "b"s);
  reader_check(reader, token_t::type::SYMBOL, "c"s);
  reader_check(reader, token_t::type::MACRO, ")"s);
  CHECK(!reader.read());
}

TEST_CASE("Reader::read ( a b c )")
{
  Reader reader{"( a b c )"};
  reader_check(reader, token_t::type::MACRO, "("s);
  reader_check(reader, token_t::type::SYMBOL, "a"s);
  reader_check(reader, token_t::type::SYMBOL, "b"s);
  reader_check(reader, token_t::type::SYMBOL, "c"s);
  reader_check(reader, token_t::type::MACRO, ")"s);
  CHECK(!reader.read());
}

TEST_CASE("Reader::read \"string\"")
{
  Reader reader{"\"string\""};
  reader_check(reader, token_t::type::STRING, "string"s);
}

TEST_CASE("Reader::read \"string\" hello")
{
  Reader reader{"\"string\" hello"};
  reader_check(reader, token_t::type::STRING, "string"s);
  reader_check(reader, token_t::type::SYMBOL, "hello"s);
}

TEST_CASE("Reader::read \"st\\\"ring\"")
{
  Reader reader{"\"st\\\"ring\""};
  reader_check(reader, token_t::type::STRING, "st\"ring"s);
}

TEST_CASE("Reader::read \"st\\ring\"")
{
  Reader reader{"\"st\\\\ring\""};
  reader_check(reader, token_t::type::STRING, "st\\ring"s);
}

TEST_CASE("Reader::read \"st\\\\\\\"ring\"")
{
  Reader reader{"\"st\\\\\\\"ring\""};
  reader_check(reader, token_t::type::STRING, "st\\\"ring"s);
}

TEST_CASE("Reader::read \"(hello)\"")
{
  Reader reader{"\"(hello)\""};
  reader_check(reader, token_t::type::STRING, "(hello)"s);
}

TEST_CASE("Reader::read 123")
{
  Reader reader{"123"};
  reader_check(reader, token_t::type::INT, "123");
}

TEST_CASE("Reader::read 0123456789")
{
  Reader reader{"0123456789"};
  reader_check(reader, token_t::type::INT, "0123456789");
}

TEST_CASE("Reader::unread")
{
  Reader reader{"()"};
  auto token = reader.read();
  CHECK(token->token == "(");
  reader.unread(*token);
  token = reader.read();
  CHECK(token->token == "(");
  token = reader.read();
  CHECK(token->token == ")");
}

}

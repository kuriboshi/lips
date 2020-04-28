//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#pragma once

#include <stdexcept>
#include <string>

namespace lisp
{
class lisp_error: public std::runtime_error
{
public:
  lisp_error(const std::string& error): std::runtime_error(error) {}
};

class type_error: public lisp_error
{
public:
  // Some standard messages, all of them not necessarily used
  static constexpr const char* errmess[] = {"NIL", "symbol", "integer", "bignum", "float",
    "indirect", "long", "list", "string", "SUBR", "FSUBR", "LAMBDA", "NLAMBDA",
    "closure", "unbound", "environment", "file pointer", "T", "free", "EOF",
    "ERROR", "hash table"};

  type_error(LISPT arg, lisp_type type): lisp_error(std::string("Expected ") + errmess[type] + ) {}
  type_error(LISPT arg, lisp_type type0, lisp_type type1)
    : lisp_error(std::string("Expected ") + errmess[type0] + " or " + errmess[type1])
  {}
};

} // namespace lisp

//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#pragma once

#include <stdexcept>
#include <string>

namespace lisp
{
class type_error: public std::runtime_error
{
public:
  // Some standard messages, all of them not necessarily used
  static constexpr const char* errmess[] = {"Not NIL", "Not a symbol", "Not an integer", "Not a bignum", "Not a float",
    "Not indirect", "Not a long", "Not a list", "Not a string", "Not SUBR", "Not FSUBR", "Not LAMBDA", "Not NLAMBDA",
    "Not a closure", "Not unbound", "Not an environment", "Not a file pointer", "Not T", "Not free", "Not EOF",
    "Not an ERROR", "Not a hash table"};

  type_error(lisp_type type): std::runtime_error(errmess[type]) {}
};

class lisp_error: public std::runtime_error
{
public:
  lisp_error(const std::string& error): std::runtime_error(error) {}
};

} // namespace lisp

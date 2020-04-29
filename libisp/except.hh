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

class lisp_reset: public lisp_error
{
public:
  lisp_reset(): lisp_error("reset") {}
};

} // namespace lisp

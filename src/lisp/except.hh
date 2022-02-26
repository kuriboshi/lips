//
// Lips, lisp shell.
// Copyright 2020-2021 Krister Joas
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

class lisp_finish: public std::runtime_error
{
public:
  lisp_finish(const std::string& message, int code): std::runtime_error(message), exit_code(code) {}
  int exit_code;
};

} // namespace lisp

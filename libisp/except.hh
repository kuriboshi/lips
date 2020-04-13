//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#pragma once

#include <stdexcept>
#include <string>

class lips_error: public std::runtime_error
{
public:
  lips_error(const std::string& error): std::runtime_error(error) {}
};

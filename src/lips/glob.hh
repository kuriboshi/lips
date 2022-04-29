//
// Lips, lisp shell.
// Copyright 2020-2022 Krister Joas
//

#ifndef LIPS_GLOB_HH
#define LIPS_GLOB_HH

#include <optional>
#include <string>
#include <filesystem>
#include <catch2/catch.hpp>
#include <lisp/liblisp.hh>

namespace glob
{
void init();
using LISPT = lisp::LISPT;

std::optional<std::string> extilde(const std::string& w);
LISPT expandfiles(const std::string& wild, bool sort);
LISPT expand(LISPT wild);
}

inline lisp::LISPT expand(lisp::lisp&, lisp::LISPT wild)
{
  return glob::expand(wild);
}
inline lisp::LISPT expand(lisp::LISPT wild)
{
  return glob::expand(wild);
}

#endif

/*
 * Lips, lisp shell.
 * Copyright 2020-2021 Krister Joas
 *
 */

#pragma once

#include <doctest/doctest.h>
#include <lisp/libisp.hh>
#include <optional>
#include <string>
#include <filesystem>

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

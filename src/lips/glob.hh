/*
 * Lips, lisp shell.
 * Copyright 2020 Krister Joas
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

std::optional<std::string> extilde(const std::string& w, bool report);
LISPT expandfiles(const std::string& wild, bool all, bool report, bool sort);
LISPT expand(LISPT wild, LISPT rep, LISPT all);
}

inline lisp::LISPT expand(lisp::lisp&, lisp::LISPT a, lisp::LISPT b, lisp::LISPT c)
{
  return glob::expand(a, b, c);
}
inline lisp::LISPT expand(lisp::LISPT a, lisp::LISPT b, lisp::LISPT c)
{
  return glob::expand(a, b, c);
}

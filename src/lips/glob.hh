/*
 * Lips, lisp shell.
 * Copyright 2020 Krister Joas
 *
 */

#pragma once

#include <lisp/libisp.hh>
#include <optional>
#include <string>

extern const char* extilde(const char*, int);
extern const std::optional<std::string> extilde2(const std::string&, bool);
extern lisp::LISPT expandfiles(const char*, int, int, int);
extern lisp::LISPT glob(lisp::LISPT);

extern lisp::LISPT expand(lisp::LISPT, lisp::LISPT, lisp::LISPT);

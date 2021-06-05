/*
 * Lips, lisp shell.
 * Copyright 2020 Krister Joas
 *
 */

#pragma once

#include <lisp/libisp.hh>
#include <optional>
#include <string>

extern const std::optional<std::string> extilde(const std::string&, bool);
extern lisp::LISPT expandfiles(const std::string&, bool, bool, bool);
extern lisp::LISPT glob(lisp::LISPT);

extern lisp::LISPT expand(lisp::LISPT, lisp::LISPT, lisp::LISPT);

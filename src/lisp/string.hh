//
// Lips, lisp shell.
// Copyright 2020-2021 Krister Joas
//

#pragma once

#include "lisp.hh"

namespace lisp::string
{
void init();

LISPT symstr(lisp&, LISPT);
LISPT stringp(lisp&, LISPT);
LISPT strequal(lisp&, LISPT, LISPT);
LISPT strcmp(lisp&, LISPT, LISPT);
LISPT concat(lisp&, LISPT);
LISPT strlen(lisp&, LISPT);
LISPT substring(lisp&, LISPT, LISPT, LISPT);
} // namespace lisp::string

namespace lisp
{
inline LISPT symstr(lisp& l, LISPT x) { return string::symstr(l, x); }
inline LISPT symstr(LISPT x) { return string::symstr(lisp::current(), x); }
inline LISPT stringp(lisp& l, LISPT x) { return string::stringp(l, x); }
inline LISPT stringp(LISPT x) { return string::stringp(lisp::current(), x); }
inline LISPT strequal(lisp& l, LISPT x, LISPT y) { return string::strequal(l, x, y); }
inline LISPT strequal(LISPT x, LISPT y) { return string::strequal(lisp::current(), x, y); }
inline LISPT strcmp(lisp& l, LISPT x, LISPT y) { return string::strcmp(l, x, y); }
inline LISPT strcmp(LISPT x, LISPT y) { return string::strcmp(lisp::current(), x, y); }
inline LISPT concat(lisp& l, LISPT x) { return string::concat(l, x); }
inline LISPT concat(LISPT x) { return string::concat(lisp::current(), x); }
inline LISPT strlen(lisp& l, LISPT x) { return string::strlen(l, x); }
inline LISPT strlen(LISPT x) { return string::strlen(lisp::current(), x); }
inline LISPT substring(lisp& l, LISPT x, LISPT y, LISPT z) { return string::substring(l, x, y, z); }
inline LISPT substring(LISPT x, LISPT y, LISPT z) { return string::substring(lisp::current(), x, y, z); }
} // namespace lisp

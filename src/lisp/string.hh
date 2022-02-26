//
// Lips, lisp shell.
// Copyright 2020-2021 Krister Joas
//

#pragma once

#include "lisp.hh"

namespace lisp
{
namespace string
{
void init();

LISPT symstr(lisp&, LISPT);
LISPT stringp(lisp&, LISPT);
LISPT streq(lisp&, LISPT, LISPT);
LISPT strcmp(lisp&, LISPT, LISPT);
LISPT concat(lisp&, LISPT);
LISPT strlen(lisp&, LISPT);
LISPT substr(lisp&, LISPT, LISPT, LISPT);
} // namespace string

inline LISPT symstr(lisp& l, LISPT x) { return string::symstr(l, x); }
inline LISPT symstr(LISPT x) { return string::symstr(lisp::current(), x); }
inline LISPT stringp(lisp& l, LISPT x) { return string::stringp(l, x); }
inline LISPT stringp(LISPT x) { return string::stringp(lisp::current(), x); }
inline LISPT streq(lisp& l, LISPT x, LISPT y) { return string::streq(l, x, y); }
inline LISPT streq(LISPT x, LISPT y) { return string::streq(lisp::current(), x, y); }
inline LISPT strcmp(lisp& l, LISPT x, LISPT y) { return string::strcmp(l, x, y); }
inline LISPT strcmp(LISPT x, LISPT y) { return string::strcmp(lisp::current(), x, y); }
inline LISPT concat(lisp& l, LISPT x) { return string::concat(l, x); }
inline LISPT concat(LISPT x) { return string::concat(lisp::current(), x); }
inline LISPT strlen(lisp& l, LISPT x) { return string::strlen(l, x); }
inline LISPT strlen(LISPT x) { return string::strlen(lisp::current(), x); }
inline LISPT substr(lisp& l, LISPT x, LISPT y, LISPT z) { return string::substr(l, x, y, z); }
inline LISPT substr(LISPT x, LISPT y, LISPT z) { return string::substr(lisp::current(), x, y, z); }

} // namespace lisp

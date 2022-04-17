//
// Lips, lisp shell.
// Copyright 2020-2021 Krister Joas
//

#pragma once

#include "lisp.hh"

namespace lisp
{
namespace user
{
void init();

LISPT getrep(lisp&, LISPT);
LISPT define(lisp&, LISPT, LISPT);
LISPT defineq(lisp&, LISPT);
LISPT funeq(lisp&, LISPT, LISPT);
} // namespace user

inline LISPT getrep(lisp& l, LISPT a) { return user::getrep(l, a); }
inline LISPT getrep(LISPT a) { return user::getrep(lisp::current(), a); }
inline LISPT define(lisp& l, LISPT a, LISPT b) { return user::define(l, a, b); }
inline LISPT define(LISPT a, LISPT b) { return user::define(lisp::current(), a, b); }
inline LISPT defineq(lisp& l, LISPT a) { return user::defineq(l, a); }
inline LISPT defineq(LISPT a) { return user::defineq(lisp::current(), a); }

} // namespace lisp

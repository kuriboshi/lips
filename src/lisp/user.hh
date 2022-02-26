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
LISPT de(lisp&, LISPT, LISPT, LISPT);
LISPT df(lisp&, LISPT, LISPT, LISPT);
LISPT funeq(lisp&, LISPT, LISPT);
} // namespace user

inline LISPT funeq(lisp& l, LISPT a, LISPT b) { return user::funeq(l, a, b); }
inline LISPT funeq(LISPT a, LISPT b) { return user::funeq(lisp::current(), a, b); }
inline LISPT getrep(lisp& l, LISPT a) { return user::getrep(l, a); }
inline LISPT getrep(LISPT a) { return user::getrep(lisp::current(), a); }
inline LISPT define(lisp& l, LISPT a, LISPT b) { return user::define(l, a, b); }
inline LISPT define(LISPT a, LISPT b) { return user::define(lisp::current(), a, b); }
inline LISPT defineq(lisp& l, LISPT a) { return user::defineq(l, a); }
inline LISPT defineq(LISPT a) { return user::defineq(lisp::current(), a); }
inline LISPT de(lisp& l, LISPT a, LISPT b, LISPT c) { return user::de(l, a, b, c); }
inline LISPT de(LISPT a, LISPT b, LISPT c) { return user::de(lisp::current(), a, b, c); }
inline LISPT df(lisp& l, LISPT a, LISPT b, LISPT c) { return user::df(l, a, b, c); }
inline LISPT df(LISPT a, LISPT b, LISPT c) { return user::df(lisp::current(), a, b, c); }

} // namespace lisp

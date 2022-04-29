//
// Lips, lisp shell.
// Copyright 2020-2022 Krister Joas
//

#ifndef LISP_USER_HH
#define LISP_USER_HH

#include "lisp.hh"

namespace lisp::user
{
void init();

LISPT getrep(lisp&, LISPT);
LISPT define(lisp&, LISPT, LISPT);
LISPT defineq(lisp&, LISPT);
LISPT funeq(lisp&, LISPT, LISPT);
} // namespace lisp::user

namespace lisp
{
inline LISPT getrep(lisp& l, LISPT a) { return user::getrep(l, a); }
inline LISPT getrep(LISPT a) { return user::getrep(lisp::current(), a); }
inline LISPT define(lisp& l, LISPT a, LISPT b) { return user::define(l, a, b); }
inline LISPT define(LISPT a, LISPT b) { return user::define(lisp::current(), a, b); }
inline LISPT defineq(lisp& l, LISPT a) { return user::defineq(l, a); }
inline LISPT defineq(LISPT a) { return user::defineq(lisp::current(), a); }
} // namespace lisp

#endif

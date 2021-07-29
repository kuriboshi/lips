//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#pragma once

#include "lisp.hh"
#include "base.hh"

namespace lisp
{
class user: public base
{
public:
  user();
  user(lisp&);
  ~user() = default;
  static void init();

  static LISPT getrep(lisp&, LISPT);
  static LISPT define(lisp&, LISPT, LISPT);
  static LISPT defineq(lisp&, LISPT);
  static LISPT de(lisp&, LISPT, LISPT, LISPT);
  static LISPT df(lisp&, LISPT, LISPT, LISPT);
  static LISPT funeq(lisp&, LISPT, LISPT);

private:
  static LISPT getargs(lisp&, LISPT);
  static LISPT def(lisp&, LISPT, LISPT, LISPT, type);
  static LISPT checkfn(lisp&, LISPT, LISPT);
};

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

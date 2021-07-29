//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#pragma once

#include "lisp.hh"
#include "base.hh"

namespace lisp
{
class string: public base
{
public:
  string();
  string(lisp&);
  ~string() = default;
  static void init();

  static LISPT symstr(lisp&, LISPT);
  static LISPT stringp(lisp&, LISPT);
  static LISPT streq(lisp&, LISPT, LISPT);
  static LISPT strcmp(lisp&, LISPT, LISPT);
  static LISPT concat(lisp&, LISPT);
  static LISPT strlen(lisp&, LISPT);
  static LISPT substr(lisp&, LISPT, LISPT, LISPT);
};

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

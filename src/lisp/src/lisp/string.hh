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

  LISPT symstr(LISPT);
  LISPT stringp(LISPT);
  LISPT streq(LISPT, LISPT);
  LISPT strcomp(LISPT, LISPT);
  LISPT concat(LISPT);
  LISPT strlen(LISPT);
  LISPT substr(LISPT, LISPT, LISPT);
};

inline LISPT symstr(lisp& l, LISPT x) { return string(l).symstr(x); }
inline LISPT symstr(LISPT x) { return string().symstr(x); }
inline LISPT stringp(lisp& l, LISPT x) { return string(l).stringp(x); }
inline LISPT stringp(LISPT x) { return string().stringp(x); }
inline LISPT streq(lisp& l, LISPT x, LISPT y) { return string(l).streq(x, y); }
inline LISPT streq(LISPT x, LISPT y) { return string().streq(x, y); }
inline LISPT strcomp(lisp& l, LISPT x, LISPT y) { return string(l).strcomp(x, y); }
inline LISPT strcomp(LISPT x, LISPT y) { return string().strcomp(x, y); }
inline LISPT concat(lisp& l, LISPT x) { return string(l).concat(x); }
inline LISPT concat(LISPT x) { return string().concat(x); }
inline LISPT strlen(lisp& l, LISPT x) { return string(l).strlen(x); }
inline LISPT strlen(LISPT x) { return string().strlen(x); }
inline LISPT substr(lisp& l, LISPT x, LISPT y, LISPT z) { return string(l).substr(x, y, z); }
inline LISPT substr(LISPT x, LISPT y, LISPT z) { return string().substr(x, y, z); }

} // namespace lisp

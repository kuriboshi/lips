//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#pragma once

#include <cstdio>
#include "base.hh"

namespace lisp
{
inline constexpr auto PN_LOAD = "load";         // load file
inline constexpr auto PN_PRIN1 = "prin1";       // print without escapes
inline constexpr auto PN_PRIN2 = "prin2";       // print without new-line
inline constexpr auto PN_PRINT = "print";       // print
inline constexpr auto PN_PLEVEL = "printlevel"; // how deep to print
inline constexpr auto PN_RATOM = "ratom";       // read atom
inline constexpr auto PN_READ = "read";         // read expression
inline constexpr auto PN_READC = "readc";       // read characte
inline constexpr auto PN_READLINE = "readline"; // read a line
inline constexpr auto PN_SPACES = "spaces";     // print some spaces
inline constexpr auto PN_TERPRI = "terpri";     // print new-line
inline constexpr auto PN_CPPRINT = "cpprint";   // find and prettyprint c function

class file: public base
{
public:
  file(lisp&);
  ~file() = default;
  static void init();

  LISPT xratom(LISPT);
  LISPT readc(LISPT);
  LISPT xread(LISPT);
  LISPT xprint(LISPT, LISPT);
  LISPT load(LISPT);
  LISPT xterpri(LISPT);
  LISPT prin1(LISPT, LISPT);
  LISPT prin2(LISPT, LISPT);
  LISPT plevel(LISPT);
  LISPT spaces(LISPT, LISPT);
  LISPT xreadline(LISPT);
  LISPT cpprint(LISPT, LISPT);

  bool loadfile(const char*);
};

inline LISPT xratom(lisp& l, LISPT a) { return file(l).xratom(a); }
inline LISPT readc(lisp& l, LISPT a) { return file(l).readc(a); }
inline LISPT xread(lisp& l, LISPT a) { return file(l).xread(a); }
inline LISPT xprint(lisp& l, LISPT a, LISPT b) { return file(l).xprint(a, b); }
inline LISPT load(lisp& l, LISPT a) { return file(l).load(a); }
inline LISPT xterpri(lisp& l, LISPT a) { return file(l).xterpri(a); }
inline LISPT prin1(lisp& l, LISPT a, LISPT b) { return file(l).prin1(a, b); }
inline LISPT prin2(lisp& l, LISPT a, LISPT b) { return file(l).prin2(a, b); }
inline LISPT plevel(lisp& l, LISPT a) { return file(l).plevel(a); }
inline LISPT spaces(lisp& l, LISPT a, LISPT b) { return file(l).spaces(a, b); }
inline LISPT xreadline(lisp& l, LISPT a) { return file(l).xreadline(a); }
inline LISPT cpprint(lisp& l, LISPT a, LISPT b) { return file(l).cpprint(a, b); }

inline bool loadfile(lisp& l, const char* filename) { return file(l).loadfile(filename); }

} // namespace lisp

//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#pragma once

#include <cstdio>
#include "base.hh"

extern FILE* primin;
extern FILE* primout;
extern FILE* primerr;

namespace lisp
{
class file: public base
{
public:
  file(lisp&);
  ~file() = default;

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

//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#pragma once

#include "lisp.hh"
#include "base.hh"

namespace lisp
{
class file: public base
{
public:
  file();
  file(lisp&);
  ~file() = default;
  static void init();

  LISPT open(LISPT, LISPT);
  LISPT close(LISPT);
  LISPT ratom(LISPT);
  LISPT readc(LISPT);
  LISPT read(LISPT);
  LISPT print(LISPT, LISPT);
  LISPT load(LISPT);
  LISPT terpri(LISPT);
  LISPT prin1(LISPT, LISPT);
  LISPT prin2(LISPT, LISPT);
  LISPT plevel(LISPT);
  LISPT spaces(LISPT, LISPT);
  LISPT readline(LISPT);

  bool loadfile(const std::string&);
};

extern LISPT C_READ;

inline LISPT open(lisp& l, LISPT a, LISPT b) { return file(l).open(a, b); }
inline LISPT open(LISPT a, LISPT b) { return file().open(a, b); }
inline LISPT close(lisp& l, LISPT a) { return file(l).close(a); }
inline LISPT close(LISPT a) { return file().close(a); }
inline LISPT ratom(lisp& l, LISPT a) { return file(l).ratom(a); }
inline LISPT ratom(LISPT a) { return file().ratom(a); }
inline LISPT readc(lisp& l, LISPT a) { return file(l).readc(a); }
inline LISPT readc(LISPT a) { return file().readc(a); }
inline LISPT read(lisp& l, LISPT a) { return file(l).read(a); }
inline LISPT read(LISPT a) { return file().read(a); }
inline LISPT print(lisp& l, LISPT a, LISPT b) { return file(l).print(a, b); }
inline LISPT print(LISPT a, LISPT b) { return file().print(a, b); }
inline LISPT load(lisp& l, LISPT a) { return file(l).load(a); }
inline LISPT load(LISPT a) { return file().load(a); }
inline LISPT terpri(lisp& l, LISPT a) { return file(l).terpri(a); }
inline LISPT terpri(LISPT a) { return file().terpri(a); }
inline LISPT prin1(lisp& l, LISPT a, LISPT b) { return file(l).prin1(a, b); }
inline LISPT prin1(LISPT a, LISPT b) { return file().prin1(a, b); }
inline LISPT prin2(lisp& l, LISPT a, LISPT b) { return file(l).prin2(a, b); }
inline LISPT prin2(LISPT a, LISPT b) { return file().prin2(a, b); }
inline LISPT plevel(lisp& l, LISPT a) { return file(l).plevel(a); }
inline LISPT plevel(LISPT a) { return file().plevel(a); }
inline LISPT spaces(lisp& l, LISPT a, LISPT b) { return file(l).spaces(a, b); }
inline LISPT spaces(LISPT a, LISPT b) { return file().spaces(a, b); }
inline LISPT readline(lisp& l, LISPT a) { return file(l).readline(a); }
inline LISPT readline(LISPT a) { return file().readline(a); }

inline bool loadfile(lisp& l, const std::string& filename) { return file(l).loadfile(filename); }
inline bool loadfile(const std::string& filename) { return file().loadfile(filename); }

} // namespace lisp

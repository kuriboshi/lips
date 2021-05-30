//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#pragma once

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
  LISPT cpprint(LISPT, LISPT);

  bool loadfile(const std::string&);
};

inline LISPT ratom(lisp& l, LISPT a) { return file(l).ratom(a); }
inline LISPT ratom(LISPT a) { return file(lisp::current()).ratom(a); }
inline LISPT readc(lisp& l, LISPT a) { return file(l).readc(a); }
inline LISPT readc(LISPT a) { return file(lisp::current()).readc(a); }
inline LISPT read(lisp& l, LISPT a) { return file(l).read(a); }
inline LISPT read(LISPT a) { return file(lisp::current()).read(a); }
inline LISPT print(lisp& l, LISPT a, LISPT b) { return file(l).print(a, b); }
inline LISPT print(LISPT a, LISPT b) { return file(lisp::current()).print(a, b); }
inline LISPT load(lisp& l, LISPT a) { return file(l).load(a); }
inline LISPT load(LISPT a) { return file(lisp::current()).load(a); }
inline LISPT terpri(lisp& l, LISPT a) { return file(l).terpri(a); }
inline LISPT terpri(LISPT a) { return file(lisp::current()).terpri(a); }
inline LISPT prin1(lisp& l, LISPT a, LISPT b) { return file(l).prin1(a, b); }
inline LISPT prin1(LISPT a, LISPT b) { return file(lisp::current()).prin1(a, b); }
inline LISPT prin2(lisp& l, LISPT a, LISPT b) { return file(l).prin2(a, b); }
inline LISPT prin2(LISPT a, LISPT b) { return file(lisp::current()).prin2(a, b); }
inline LISPT plevel(lisp& l, LISPT a) { return file(l).plevel(a); }
inline LISPT plevel(LISPT a) { return file(lisp::current()).plevel(a); }
inline LISPT spaces(lisp& l, LISPT a, LISPT b) { return file(l).spaces(a, b); }
inline LISPT spaces(LISPT a, LISPT b) { return file(lisp::current()).spaces(a, b); }
inline LISPT readline(lisp& l, LISPT a) { return file(l).readline(a); }
inline LISPT readline(LISPT a) { return file(lisp::current()).readline(a); }
inline LISPT cpprint(lisp& l, LISPT a, LISPT b) { return file(l).cpprint(a, b); }
inline LISPT cpprint(LISPT a, LISPT b) { return file(lisp::current()).cpprint(a, b); }

inline bool loadfile(lisp& l, const char* filename) { return file(l).loadfile(filename); }
inline bool loadfile(const char* filename) { return file(lisp::current()).loadfile(filename); }

} // namespace lisp

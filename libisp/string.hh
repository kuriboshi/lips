//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#pragma once

#include "lisp.hh"
#include "base.hh"

namespace lisp
{
inline constexpr auto PN_STRINGP = "stringp"; // t if string
inline constexpr auto PN_STREQ = "streq";     // string equal
inline constexpr auto PN_CONCAT = "concat";   // concatenate strings
inline constexpr auto PN_STRLEN = "strlen";   // length of string
inline constexpr auto PN_SUBSTR = "substr";   // get sub string
inline constexpr auto PN_SYMSTR = "symstr";   // make symbol a string
inline constexpr auto PN_STRCMP = "strcmp";   // compare strings

class string: public base
{
public:
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
inline LISPT symstr(LISPT x) { return string(lisp::current()).symstr(x); }
inline LISPT stringp(lisp& l, LISPT x) { return string(l).stringp(x); }
inline LISPT stringp(LISPT x) { return string(lisp::current()).stringp(x); }
inline LISPT streq(lisp& l, LISPT x, LISPT y) { return string(l).streq(x, y); }
inline LISPT streq(LISPT x, LISPT y) { return string(lisp::current()).streq(x, y); }
inline LISPT strcomp(lisp& l, LISPT x, LISPT y) { return string(l).strcomp(x, y); }
inline LISPT strcomp(LISPT x, LISPT y) { return string(lisp::current()).strcomp(x, y); }
inline LISPT concat(lisp& l, LISPT x) { return string(l).concat(x); }
inline LISPT concat(LISPT x) { return string(lisp::current()).concat(x); }
inline LISPT strlen(lisp& l, LISPT x) { return string(l).strlen(x); }
inline LISPT strlen(LISPT x) { return string(lisp::current()).strlen(x); }
inline LISPT substr(lisp& l, LISPT x, LISPT y, LISPT z) { return string(l).substr(x, y, z); }
inline LISPT substr(LISPT x, LISPT y, LISPT z) { return string(lisp::current()).substr(x, y, z); }

} // namespace lisp

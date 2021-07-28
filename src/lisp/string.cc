/*
 * Lips, lisp shell.
 * Copyright 1988, 2020 Krister Joas
 *
 */

#include "string.hh"
#include "alloc.hh"
#include "io.hh"

namespace lisp
{
string::string(): base() {}
string::string(lisp& lisp): base(lisp) {}

/* Return symbols print name as a string. */
LISPT string::symstr(LISPT sym)
{
  l.check(sym, type::SYMBOL, type::T, type::NIL);
  if(type_of(sym) == type::NIL)
    return mkstring("nil");
  return mkstring(l, sym->symbol().pname.name);
}

/* T if s is a string, NIL otherwise. */
LISPT string::stringp(LISPT s)
{
  if(type_of(s) == type::STRING)
    return s;
  return NIL;
}

/* T if both strings are equal */
LISPT string::streq(LISPT s1, LISPT s2)
{
  l.check(s1, type::STRING);
  l.check(s2, type::STRING);
  if(s1->stringval() == s2->stringval())
    return T;
  return NIL;
}

LISPT string::strcmp(LISPT s1, LISPT s2)
{
  l.check(s1, type::STRING);
  l.check(s2, type::STRING);
  return mknumber(l, s1->stringval().compare(s2->stringval()));
}

/* Concatenate arbitrary many strings to
   to one string */
LISPT string::concat(LISPT strlist)
{
  std::string result;
  for(auto sl = strlist; !is_NIL(sl); sl = sl->cdr())
  {
    l.check(sl->car(), type::STRING);
    result += sl->car()->stringval();
  }
  return mkstring(l, result);
}

/* Return string length of s */
LISPT string::strlen(LISPT s)
{
  l.check(s, type::STRING);
  return mknumber(l, s->stringval().length());
}

/* Extract a substring from start to end.
   If start or end is out of bounds, return
   NIL. If end is one less than start the
   zero length string is returned. end equal
   to zero if start is equal to one is accepted. */
LISPT string::substr(LISPT str, LISPT start, LISPT end)
{
  l.check(str, type::STRING);
  l.check(start, type::INTEGER);
  l.check(end, type::INTEGER);
  auto s = start->intval();
  auto e = end->intval();
  auto size = e - s + 1;
  if(size < 0 || s > str->stringval().length()
    || e > str->stringval().length() || s < 0 || e < 0)
    return NIL;
  return mkstring(l, str->stringval().substr(s, e));
}

namespace pn
{
inline constexpr auto STRINGP = "stringp"; // t if string
inline constexpr auto STREQ = "streq";     // string equal
inline constexpr auto CONCAT = "concat";   // concatenate strings
inline constexpr auto STRLEN = "strlen";   // length of string
inline constexpr auto SUBSTR = "substr";   // get sub string
inline constexpr auto SYMSTR = "symstr";   // make symbol a string
inline constexpr auto STRCMP = "strcmp";   // compare strings
} // namespace pn

void string::init()
{
  // clang-format off
  mkprim(pn::STRINGP, ::lisp::stringp, subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::STREQ,   ::lisp::streq,   subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::CONCAT,  ::lisp::concat,  subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::STRLEN,  ::lisp::strlen,  subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::SUBSTR,  ::lisp::substr,  subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::SYMSTR,  ::lisp::symstr,  subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::STRCMP,  ::lisp::strcmp, subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
  // clang-format on
}

} // namespace lisp

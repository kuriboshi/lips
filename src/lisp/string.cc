//
// Lips, lisp shell.
// Copyright 1988, 2020-2022 Krister Joas
//

#include "string.hh"
#include "alloc.hh"
#include "io.hh"

namespace lisp::string
{
/// @brief Return symbols print name as a string.
LISPT symstr(lisp& l, LISPT sym)
{
  check(sym, type::SYMBOL, type::T, type::NIL);
  if(type_of(sym) == type::NIL)
    return mkstring("nil");
  return mkstring(l, sym->symbol().pname);
}

/// @brief T if s is a string, NIL otherwise.
LISPT stringp(lisp& l, LISPT s)
{
  if(type_of(s) == type::STRING)
    return s;
  return NIL;
}

/// @brief T if both strings are equal.
LISPT strequal(lisp& l, LISPT s1, LISPT s2)
{
  check(s1, type::STRING);
  check(s2, type::STRING);
  if(s1->string() == s2->string())
    return T;
  return NIL;
}

/// @brief Compare two strings.
LISPT strcmp(lisp& l, LISPT s1, LISPT s2)
{
  check(s1, type::STRING);
  check(s2, type::STRING);
  return mknumber(l, s1->string().compare(s2->string()));
}

/// @brief Concatenate arbitrary many strings to one string.
LISPT concat(lisp& l, LISPT strlist)
{
  std::string result;
  for(auto sl = strlist; !is_NIL(sl); sl = sl->cdr())
  {
    check(sl->car(), type::STRING);
    result += sl->car()->string();
  }
  return mkstring(l, result);
}

/// @brief Return string length of s.
LISPT strlen(lisp& l, LISPT s)
{
  check(s, type::STRING);
  return mknumber(l, s->string().length());
}

/// @brief Extract a substring from start to end.
///
/// @details If start or end is out of bounds, return NIL.  If end is one less
/// than start the zero length string is returned.  End equal to zero if
/// start is equal to one is accepted.
LISPT substring(lisp& l, LISPT str, LISPT begin, LISPT end)
{
  check(str, type::STRING);
  check(begin, type::INTEGER);
  check(end, type::INTEGER, type::NIL);
  const auto& s = str->string();
  auto i = begin->intval();
  if(i == 0)
    return NIL;
  auto b = [&]() -> std::string::size_type {
    if(i < 0)
      return s.size() + i;
    return i - 1;
  }();
  std::string::size_type e{0};
  if(type_of(end) == type::INTEGER)
  {
    auto j = end->intval();
    if(j == 0)
      return NIL;
    if(i > j)
      return NIL;
    e = j - b;
    if(e == 0)
      return NIL;
  }
  else
    e = std::string::npos;
  if(b > str->string().length())
    return NIL;
  return mkstring(l, str->string().substr(b, e));
}

namespace pn
{
inline constexpr auto STRINGP = "stringp";     // t if string
inline constexpr auto STREQUAL = "strequal";   // string equal
inline constexpr auto CONCAT = "concat";       // concatenate strings
inline constexpr auto STRLEN = "strlen";       // length of string
inline constexpr auto SUBSTRING = "substring"; // get sub string
inline constexpr auto SYMSTR = "symstr";       // make symbol a string
inline constexpr auto STRCMP = "strcmp";       // compare strings
} // namespace pn

void init()
{
  // clang-format off
  mkprim(pn::STRINGP,   stringp,   subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::STREQUAL,  strequal,  subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::CONCAT,    concat,    subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::STRLEN,    strlen,    subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::SUBSTRING, substring, subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::SYMSTR,    symstr,    subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::STRCMP,    strcmp,    subr_t::subr::EVAL, subr_t::spread::SPREAD);
  // clang-format on
}

} // namespace lisp::string

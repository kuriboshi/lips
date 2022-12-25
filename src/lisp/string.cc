//
// Lips, lisp shell.
// Copyright 1988, 2020-2022 Krister Joas
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//

#include <string>

#include "alloc.hh"
#include "check.hh"
#include "string.hh"

namespace lisp::details::string
{
/// @brief Return symbols print name as a string.
lisp_t symstr(context&, lisp_t sym)
{
  check(sym, type::Symbol, type::T, type::Nil);
  if(type_of(sym) == type::Nil)
    return mkstring("nil");
  return mkstring(sym->symbol()->pname);
}

/// @brief T if s is a string, nil otherwise.
lisp_t stringp(context&, lisp_t s)
{
  if(type_of(s) == type::String)
    return s;
  return nil;
}

/// @brief T if both strings are equal.
lisp_t strequal(context&, lisp_t s1, lisp_t s2)
{
  check(s1, type::String);
  check(s2, type::String);
  if(s1->string() == s2->string())
    return T;
  return nil;
}

/// @brief Compare two strings.
lisp_t strcmp(context&, lisp_t s1, lisp_t s2)
{
  check(s1, type::String);
  check(s2, type::String);
  return mknumber(s1->string().compare(s2->string()));
}

/// @brief Concatenate arbitrary many strings to one string.
lisp_t concat(context&, lisp_t strlist)
{
  std::string result;
  for(auto sl = strlist; !is_nil(sl); sl = sl->cdr())
  {
    check(sl->car(), type::String);
    result += sl->car()->string();
  }
  return mkstring(result);
}

/// @brief Return string length of s.
lisp_t strlen(context&, lisp_t s)
{
  check(s, type::String);
  return mknumber(static_cast<int>(s->string().length()));
}

/// @brief Extract a substring from start to end.
///
/// @details If start or end is out of bounds, return nil.  If end is one less
/// than start the zero length string is returned.  End equal to zero if
/// start is equal to one is accepted.
lisp_t substring(context&, lisp_t str, lisp_t begin, lisp_t end)
{
  check(str, type::String);
  check(begin, type::Integer);
  check(end, type::Integer, type::Nil);
  const auto& s = str->string();
  auto i = begin->intval();
  if(i == 0)
    return nil;
  auto b = [&]() -> std::string::size_type {
    if(i < 0)
      return s.size() + i;
    return i - 1;
  }();
  std::string::size_type e{0};
  if(type_of(end) == type::Integer)
  {
    auto j = end->intval();
    if(j == 0)
      return nil;
    if(i > j)
      return nil;
    e = j - b;
    if(e == 0)
      return nil;
  }
  else
    e = std::string::npos;
  if(b > str->string().length())
    return nil;
  return mkstring(str->string().substr(b, e));
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

} // namespace lisp::details::string

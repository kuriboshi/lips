//
// Lips, lisp shell.
// Copyright 1988, 2020-2023 Krister Joas
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
#include <string_view>

#include "alloc.hh"
#include "check.hh"
#include "string.hh"

namespace lisp::details::string
{
/// @brief Concatenate arbitrary many strings to one string.
lisp_t concat(lisp_t strlist)
{
  std::string result;
  for(auto sl = strlist; !is_nil(sl); sl = sl->cdr())
  {
    check(sl->car(), object::type::String);
    result += sl->car()->string();
  }
  return mkstring(result);
}

/// @brief Compare two strings.
lisp_t strcmp(lisp_t s1, lisp_t s2)
{
  check(s1, object::type::String);
  check(s2, object::type::String);
  return mknumber(s1->string().compare(s2->string()));
}

/// @brief T if both strings are equal.
lisp_t strequal(lisp_t s1, lisp_t s2)
{
  check(s1, object::type::String);
  check(s2, object::type::String);
  if(s1->string() == s2->string())
    return T;
  return nil;
}

/// @brief T if s is a string, nil otherwise.
lisp_t stringp(lisp_t s)
{
  if(type_of(s) == object::type::String)
    return s;
  return nil;
}

/// @brief Return string length of s.
lisp_t strlen(lisp_t s)
{
  check(s, object::type::String);
  return mknumber(static_cast<int>(s->string().length()));
}

lisp_t substring(lisp_t str, lisp_t begin, lisp_t end)
{
  check(str, object::type::String);
  check(begin, object::type::Integer);
  check(end, object::type::Integer, object::type::Nil);
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
  if(type_of(end) == object::type::Integer)
  {
    auto j = end->intval();
    if(j == 0)
      return nil;
    if(i > j)
      return nil;
    e = j - b;
  }
  else
    e = std::string::npos;
  if(b > str->string().length())
    return nil;
  return mkstring(str->string().substr(b, e));
}

lisp_t symstr(lisp_t sym)
{
  check(sym, object::type::Symbol, object::type::Nil);
  if(type_of(sym) == object::type::Nil)
    return mkstring("nil");
  return mkstring(sym->symbol()->pname);
}

namespace pn
{
inline constexpr std::string_view CONCAT = "concat";       // concatenate strings
inline constexpr std::string_view STRCMP = "strcmp";       // compare strings
inline constexpr std::string_view STREQUAL = "strequal";   // string equal
inline constexpr std::string_view STRINGP = "stringp";     // t if string
inline constexpr std::string_view STRLEN = "strlen";       // length of string
inline constexpr std::string_view SUBSTRING = "substring"; // get sub string
inline constexpr std::string_view SYMSTR = "symstr";       // make symbol a string
} // namespace pn

void init()
{
  // clang-format off
  mkprim(pn::CONCAT,    concat,    subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::STRCMP,    strcmp,    subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::STREQUAL,  strequal,  subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::STRINGP,   stringp,   subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::STRLEN,    strlen,    subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::SUBSTRING, substring, subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::SYMSTR,    symstr,    subr_t::subr::EVAL, subr_t::spread::SPREAD);
  // clang-format on
}

} // namespace lisp::details::string

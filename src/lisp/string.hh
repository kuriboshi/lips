//
// Lips, lisp shell.
// Copyright 2020-2024 Krister Joas
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

#ifndef LISP_STRING_HH
#define LISP_STRING_HH

/// @file string.hh
///
/// # String Functions

#include "types.hh"
#include "details/string.hh"

namespace lisp
{

/// @brief Concatenate strings.
/// @lisp{(concat args...),NoSpread Function}
inline lisp_t concat(const lisp_t& x) { return details::string::concat(x); }

/// @brief Compare two strings.
/// @lisp{(strcmp s1 s2),Function}
///
/// Compares the two strings _s1_ and _s2_ lexicographically and returns a
/// negative number, zero, or a positive number if _s1_ is less than, equal, or
/// greater than _s2_.
///
/// @param s1 A string.
/// @param s2 A string.
///
/// @returns A positive number of x is lexiographically greater than y, a
/// negative number if x is lexiographically less than y and zero if they are
/// equal.
inline lisp_t strcmp(const lisp_t& s1, const lisp_t& s2) { return details::string::strcmp(s1, s2); }

/// @brief Compare if to strings are equal.
/// @lisp{(strequal s1 s2),Function}
///
/// Compares the strings _s1_ and _s2_ and returns `t` if the strings are equal
/// or `nil` if they are not equal.
///
/// @param s1 A string.
/// @param s2 A string.
///
/// @returns `t` if _s1_ is equal to _s2_, `nil` otherwise.
inline lisp_t strequal(const lisp_t& s1, const lisp_t& s2) { return details::string::strequal(s1, s2); }

/// @brief Check if parameter is a string.
/// @lisp{(stringp s),Function}
///
/// @param s A string.
///
/// @returns Returns the string if it's a string, nil otherwise.
inline lisp_t stringp(const lisp_t& s) { return details::string::stringp(s); }

/// @brief Returns the length of a string.
/// @lisp{(strlen s),Function}
///
/// @param s A string.
/// @returns The length of the string.
inline lisp_t strlen(const lisp_t& s) { return details::string::strlen(s); }

/// @brief Extract a substring from start to end.
/// @lisp{(substring str n m),Function}
///
/// Creates a new string which is a substring of _str_ starting from the _n_th
/// character through the _m_th character. If _m_ is `nil` the substring starts
/// from the _n_th character through to the end.
///
/// Negative numbers are treated as the _n_th or _m_th character from the
/// end.
///
/// ```lisp
/// (substring "hello" 2 3)
///   => "el"
/// (substring "hello" 2 1)
///   => nil
/// (substring "hello" 2 -2)
///   => "ell"
/// (substring "hello" -3 -1)
///   => "llo"
/// (substring "hello" -1 -3)
///   => nil
/// ```
///
/// @param str A string.
/// @param n Start character.
/// @param m End character.
///
/// @returns Returns a substring.  If start or end is out of bounds, return
/// `nil`.  If end is one less than start the zero length string is returned.
/// End equal to zero if start is equal to one is accepted.
inline lisp_t substring(const lisp_t& str, const lisp_t& n, const lisp_t& m)
{
  return details::string::substring(str, n, m);
}

/// @brief Return symbol's print name as a string.
///
/// @param sym A symbol.
///
/// @returns Returns the print name of a symbol as a string. If sym is `nil`
/// then returns the string "nil".
inline lisp_t symstr(const lisp_t& sym) { return details::string::symstr(sym); }

} // namespace lisp

#endif

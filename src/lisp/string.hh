//
// Lips, lisp shell.
// Copyright 2020-2023 Krister Joas
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

#include "types.hh"
#include "details/string.hh"

namespace lisp
{

/// @brief Concatenate strings.
inline lisp_t concat(lisp_t x) { return details::string::concat(x); }

/// @brief Compare two strings.
///
/// @param x A string.
/// @param y A string.
///
/// @returns A positive number of x is lexiographically greater than y, a
/// negative number if x is lexiographically less than y and zero if they are
/// equal.
inline lisp_t strcmp(lisp_t x, lisp_t y) { return details::string::strcmp(x, y); }

/// @brief Compare if to strings are equal.
///
/// @param x A string.
/// @param y A string.
///
/// @returns T if x is equal to y, nil otherwise.
inline lisp_t strequal(lisp_t x, lisp_t y) { return details::string::strequal(x, y); }

/// @brief Check if parameter is a string.
///
/// @param x
///
/// @returns Returns the string if it's a string, nil otherwise.
inline lisp_t stringp(lisp_t x) { return details::string::stringp(x); }

/// @brief Returns the length of a string.
///
/// @param x A string.
///
/// @returns Returns the length of the string.
inline lisp_t strlen(lisp_t x) { return details::string::strlen(x); }

/// @brief Extract a substring from start to end.
///
/// @param str
/// @param begin
/// @param end
///
/// @returns Returns a substring.  If start or end is out of bounds, return
/// nil.  If end is one less than start the zero length string is returned.
/// End equal to zero if start is equal to one is accepted.
inline lisp_t substring(lisp_t str, lisp_t begin, lisp_t end) { return details::string::substring(str, begin, end); }

/// @brief Return symbol's print name as a string.
///
/// @param sym A symbol.
///
/// @returns Returns the print name of a symbol as a string. If sym is nil then
/// returns the string "nil".
inline lisp_t symstr(lisp_t sym) { return details::string::symstr(sym); }

} // namespace lisp

#endif

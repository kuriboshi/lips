//
// Lips, lisp shell.
// Copyright 2020-2022 Krister Joas
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
inline lisp_t concat(lisp_t x) { return details::string::concat(context::current(), x); }
inline lisp_t strcmp(lisp_t x, lisp_t y) { return details::string::strcmp(context::current(), x, y); }
inline lisp_t strequal(lisp_t x, lisp_t y) { return details::string::strequal(context::current(), x, y); }
inline lisp_t stringp(lisp_t x) { return details::string::stringp(context::current(), x); }
inline lisp_t strlen(lisp_t x) { return details::string::strlen(context::current(), x); }
inline lisp_t substring(lisp_t x, lisp_t y, lisp_t z) { return details::string::substring(context::current(), x, y, z); }
inline lisp_t symstr(lisp_t x) { return details::string::symstr(context::current(), x); }
} // namespace lisp

#endif

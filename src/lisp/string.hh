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

#include "lisp.hh"

namespace lisp::string
{
void init();

LISPT symstr(lisp&, LISPT);
LISPT stringp(lisp&, LISPT);
LISPT strequal(lisp&, LISPT, LISPT);
LISPT strcmp(lisp&, LISPT, LISPT);
LISPT concat(lisp&, LISPT);
LISPT strlen(lisp&, LISPT);
LISPT substring(lisp&, LISPT, LISPT, LISPT);
} // namespace lisp::string

namespace lisp
{
inline LISPT symstr(lisp& l, LISPT x) { return string::symstr(l, x); }
inline LISPT symstr(LISPT x) { return string::symstr(lisp::current(), x); }
inline LISPT stringp(lisp& l, LISPT x) { return string::stringp(l, x); }
inline LISPT stringp(LISPT x) { return string::stringp(lisp::current(), x); }
inline LISPT strequal(lisp& l, LISPT x, LISPT y) { return string::strequal(l, x, y); }
inline LISPT strequal(LISPT x, LISPT y) { return string::strequal(lisp::current(), x, y); }
inline LISPT strcmp(lisp& l, LISPT x, LISPT y) { return string::strcmp(l, x, y); }
inline LISPT strcmp(LISPT x, LISPT y) { return string::strcmp(lisp::current(), x, y); }
inline LISPT concat(lisp& l, LISPT x) { return string::concat(l, x); }
inline LISPT concat(LISPT x) { return string::concat(lisp::current(), x); }
inline LISPT strlen(lisp& l, LISPT x) { return string::strlen(l, x); }
inline LISPT strlen(LISPT x) { return string::strlen(lisp::current(), x); }
inline LISPT substring(lisp& l, LISPT x, LISPT y, LISPT z) { return string::substring(l, x, y, z); }
inline LISPT substring(LISPT x, LISPT y, LISPT z) { return string::substring(lisp::current(), x, y, z); }
} // namespace lisp

#endif

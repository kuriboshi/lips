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

#ifndef LISP_USER_HH
#define LISP_USER_HH

#include "lisp.hh"

namespace lisp::user
{
void init();

LISPT getrep(lisp&, LISPT);
LISPT define(lisp&, LISPT, LISPT);
LISPT defineq(lisp&, LISPT);
LISPT funeq(lisp&, LISPT, LISPT);
} // namespace lisp::user

namespace lisp
{
inline LISPT getrep(lisp& l, LISPT a) { return user::getrep(l, a); }
inline LISPT getrep(LISPT a) { return user::getrep(lisp::current(), a); }
inline LISPT define(lisp& l, LISPT a, LISPT b) { return user::define(l, a, b); }
inline LISPT define(LISPT a, LISPT b) { return user::define(lisp::current(), a, b); }
inline LISPT defineq(lisp& l, LISPT a) { return user::defineq(l, a); }
inline LISPT defineq(LISPT a) { return user::defineq(lisp::current(), a); }
} // namespace lisp

#endif

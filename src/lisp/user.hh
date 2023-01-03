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

#ifndef LISP_USER_HH
#define LISP_USER_HH

#include "types.hh"
#include "details/user.hh"

namespace lisp
{
inline lisp_t define(lisp_t a, lisp_t b) { return details::user::define(a, b); }
inline lisp_t defineq(lisp_t a) { return details::user::defineq(a); }
inline lisp_t getrep(lisp_t a) { return details::user::getrep(a); }
} // namespace lisp

#endif

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

#ifndef LISP_PROP_HH
#define LISP_PROP_HH

#include "types.hh"
#include "details/prop.hh"

namespace lisp
{
inline lisp_t getplist(lisp_t a) { return details::prop::getplist(a); }
inline lisp_t getprop(lisp_t a, lisp_t b) { return details::prop::getprop(a, b); }
inline lisp_t putprop(lisp_t a, lisp_t b, lisp_t c) { return details::prop::putprop(a, b, c); }
inline lisp_t remprop(lisp_t a, lisp_t b) { return details::prop::remprop(a, b); }
inline lisp_t setplist(lisp_t a, lisp_t b) { return details::prop::setplist(a, b); }
} // namespace lisp

#endif

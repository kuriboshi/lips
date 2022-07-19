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

#include "lisp.hh"
#include "details/prop.hh"

namespace lisp
{
inline LISPT getplist(LISPT a) { return details::prop::getplist(lisp::current(), a); }
inline LISPT getprop(LISPT a, LISPT b) { return details::prop::getprop(lisp::current(), a, b); }
inline LISPT putprop(LISPT a, LISPT b, LISPT c) { return details::prop::putprop(lisp::current(), a, b, c); }
inline LISPT remprop(LISPT a, LISPT b) { return details::prop::remprop(lisp::current(), a, b); }
inline LISPT setplist(LISPT a, LISPT b) { return details::prop::setplist(lisp::current(), a, b); }
} // namespace lisp

#endif

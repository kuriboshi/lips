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

#ifndef LISP_MAP_HH
#define LISP_MAP_HH

#include "lisp.hh"
#include "details/map.hh"

namespace lisp
{
inline LISPT map(LISPT a, LISPT b, LISPT c) { return details::map::map(context::current(), a, b, c); }
inline LISPT mapc(LISPT a, LISPT b, LISPT c) { return details::map::mapc(context::current(), a, b, c); }
inline LISPT maplist(LISPT a, LISPT b, LISPT c) { return details::map::maplist(context::current(), a, b, c); }
inline LISPT mapcar(LISPT a, LISPT b, LISPT c) { return details::map::mapcar(context::current(), a, b, c); }
} // namespace lisp

#endif

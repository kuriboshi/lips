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

#ifndef LISP_LOW_HH
#define LISP_LOW_HH

#include "lisp.hh"
#include "details/low.hh"

namespace lisp
{
inline LISPT cond(LISPT a) { return details::low::cond(context::current(), a); }
inline LISPT prog1(LISPT a, LISPT b) { return details::low::prog1(context::current(), a, b); }
inline LISPT progn(LISPT a) { return details::low::progn(context::current(), a); }
inline LISPT set(LISPT a, LISPT b) { return details::low::set(context::current(), a, b); }
inline LISPT setq(LISPT a, LISPT b) { return details::low::setq(context::current(), a, b); }
inline LISPT setqq(LISPT a, LISPT b) { return details::low::set(context::current(), a, b); }
inline LISPT xwhile(LISPT a, LISPT b) { return details::low::xwhile(context::current(), a, b); }
} // namespace lisp

#endif

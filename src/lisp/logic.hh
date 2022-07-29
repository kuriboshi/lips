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

#ifndef LISP_LOGIC_HH
#define LISP_LOGIC_HH

#include "types.hh"
#include "details/logic.hh"

namespace lisp
{

inline LISPT p_and(LISPT x) { return details::logic::p_and(context::current(), x); }
inline LISPT p_or(LISPT x) { return details::logic::p_or(context::current(), x); }
inline LISPT p_not(LISPT x) { return details::logic::p_not(context::current(), x); }

} // namespace lisp

#endif

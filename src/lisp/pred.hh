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

#ifndef LISP_PRED_HH
#define LISP_PRED_HH

#include "lisp.hh"
#include "details/pred.hh"

namespace lisp
{
inline LISPT numberp(LISPT a) { return details::pred::numberp(context::current(), a); }
inline LISPT listp(LISPT a) { return details::pred::listp(context::current(), a); }
inline LISPT memb(LISPT x, LISPT y) { return details::pred::memb(context::current(), x, y); }
inline LISPT equal(LISPT l1, LISPT l2) { return details::pred::equal(context::current(), l1, l2); }
inline LISPT nlistp(LISPT a) { return details::pred::nlistp(context::current(), a); }
inline LISPT neq(LISPT a, LISPT b) { return details::pred::neq(context::current(), a, b); }
inline LISPT boundp(LISPT a) { return details::pred::boundp(context::current(), a); }
inline LISPT litatom(LISPT a) { return details::pred::litatom(context::current(), a); }
inline LISPT xtypeof(LISPT a) { return details::pred::xtypeof(context::current(), a); }
} // namespace lisp

#endif

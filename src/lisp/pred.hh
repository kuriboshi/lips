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
inline LISPT numberp(lisp& l, LISPT a) { return details::pred::numberp(l, a); }
inline LISPT numberp(LISPT a) { return details::pred::numberp(lisp::current(), a); }
inline LISPT listp(lisp& l, LISPT a) { return details::pred::listp(l, a); }
inline LISPT listp(LISPT a) { return details::pred::listp(lisp::current(), a); }
inline LISPT memb(lisp& l, LISPT x, LISPT y) { return details::pred::memb(l, x, y); }
inline LISPT memb(LISPT x, LISPT y) { return details::pred::memb(lisp::current(), x, y); }
inline LISPT equal(lisp& l, LISPT l1, LISPT l2) { return details::pred::equal(l, l1, l2); }
inline LISPT equal(LISPT l1, LISPT l2) { return details::pred::equal(lisp::current(), l1, l2); }
inline LISPT nlistp(lisp& l, LISPT a) { return details::pred::nlistp(l, a); }
inline LISPT nlistp(LISPT a) { return details::pred::nlistp(lisp::current(), a); }
inline LISPT neq(lisp& l, LISPT a, LISPT b) { return details::pred::neq(l, a, b); }
inline LISPT neq(LISPT a, LISPT b) { return details::pred::neq(lisp::current(), a, b); }
inline LISPT boundp(lisp& l, LISPT a) { return details::pred::boundp(l, a); }
inline LISPT boundp(LISPT a) { return details::pred::boundp(lisp::current(), a); }
inline LISPT litatom(lisp& l, LISPT a) { return details::pred::litatom(l, a); }
inline LISPT litatom(LISPT a) { return details::pred::litatom(lisp::current(), a); }
inline LISPT xtypeof(lisp& l, LISPT a) { return details::pred::xtypeof(l, a); }
inline LISPT xtypeof(LISPT a) { return details::pred::xtypeof(lisp::current(), a); }
} // namespace lisp

#endif

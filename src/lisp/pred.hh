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

#include "context.hh"
#include "types.hh"
#include "details/pred.hh"

namespace lisp
{
inline lisp_t numberp(lisp_t a) { return details::pred::numberp(a); }
inline lisp_t listp(lisp_t a) { return details::pred::listp(a); }
inline lisp_t memb(lisp_t x, lisp_t y) { return details::pred::memb(x, y); }
inline lisp_t equal(lisp_t l1, lisp_t l2) { return details::pred::equal(l1, l2); }
inline lisp_t nlistp(lisp_t a) { return details::pred::nlistp(a); }
inline lisp_t neq(lisp_t a, lisp_t b) { return details::pred::neq(a, b); }
inline lisp_t boundp(lisp_t a) { return details::pred::boundp(a); }
inline lisp_t litatom(lisp_t a) { return details::pred::litatom(a); }
inline lisp_t xtypeof(lisp_t a) { return details::pred::xtypeof(a); }
} // namespace lisp

#endif

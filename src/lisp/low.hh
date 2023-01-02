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

#include "types.hh"
#include "details/low.hh"

namespace lisp
{
inline lisp_t cond(lisp_t a) { return details::low::cond(a); }
inline lisp_t prog1(lisp_t a, lisp_t b) { return details::low::prog1(a, b); }
inline lisp_t progn(lisp_t a) { return details::low::progn(a); }
inline lisp_t set(lisp_t a, lisp_t b) { return details::low::set(a, b); }
inline lisp_t setq(lisp_t a, lisp_t b) { return details::low::setq(a, b); }
inline lisp_t setqq(lisp_t a, lisp_t b) { return details::low::set(a, b); }
inline lisp_t xwhile(lisp_t a, lisp_t b) { return details::low::xwhile(a, b); }
} // namespace lisp

#endif

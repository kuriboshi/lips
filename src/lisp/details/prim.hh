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

#ifndef LISP_DETAILS_PRIM_HH
#define LISP_DETAILS_PRIM_HH

#include <lisp/types.hh>

namespace lisp::details::prim
{
void init();

lisp_t car(lisp_t);
lisp_t cdr(lisp_t);
lisp_t cadr(lisp_t);
lisp_t cdar(lisp_t);
lisp_t caar(lisp_t);
lisp_t cddr(lisp_t);
lisp_t cdddr(lisp_t);
lisp_t caddr(lisp_t);
lisp_t cdadr(lisp_t);
lisp_t caadr(lisp_t);
lisp_t cddar(lisp_t);
lisp_t cadar(lisp_t);
lisp_t cdaar(lisp_t);
lisp_t caaar(lisp_t);
lisp_t rplaca(lisp_t, lisp_t);
lisp_t rplacd(lisp_t, lisp_t);
lisp_t eq(lisp_t, lisp_t);
lisp_t atom(lisp_t);
lisp_t nconc(lisp_t);
lisp_t tconc(lisp_t, lisp_t);
lisp_t attach(lisp_t, lisp_t);
lisp_t append(lisp_t);
lisp_t null(lisp_t);
lisp_t quote(lisp_t);
lisp_t lambda(lisp_t, lisp_t);
lisp_t nlambda(lisp_t, lisp_t);
lisp_t list(lisp_t);
lisp_t length(lisp_t);
lisp_t closure(lisp_t, lisp_t);

/// @brief Return the N'th element in the list LIST.  If N is greater than
/// the length of LIST, return nil.
///
lisp_t nth(lisp_t, lisp_t);
lisp_t error(lisp_t);
lisp_t exit(lisp_t);

lisp_t closobj(lisp_t);
} // namespace lisp::details::prim

#endif

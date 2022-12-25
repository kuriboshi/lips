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

#ifndef LISP_DETAILS_PRIM_HH
#define LISP_DETAILS_PRIM_HH

#include <lisp/types.hh>

namespace lisp::details::prim
{
void init();

lisp_t car(context&, lisp_t);
lisp_t cdr(context&, lisp_t);
lisp_t cadr(context&, lisp_t);
lisp_t cdar(context&, lisp_t);
lisp_t caar(context&, lisp_t);
lisp_t cddr(context&, lisp_t);
lisp_t cdddr(context&, lisp_t);
lisp_t caddr(context&, lisp_t);
lisp_t cdadr(context&, lisp_t);
lisp_t caadr(context&, lisp_t);
lisp_t cddar(context&, lisp_t);
lisp_t cadar(context&, lisp_t);
lisp_t cdaar(context&, lisp_t);
lisp_t caaar(context&, lisp_t);
lisp_t rplaca(context&, lisp_t, lisp_t);
lisp_t rplacd(context&, lisp_t, lisp_t);
lisp_t eq(context&, lisp_t, lisp_t);
lisp_t atom(context&, lisp_t);
lisp_t nconc(context&, lisp_t);
lisp_t tconc(context&, lisp_t, lisp_t);
lisp_t attach(context&, lisp_t, lisp_t);
lisp_t append(context&, lisp_t);
lisp_t null(context&, lisp_t);
lisp_t quote(context&, lisp_t);
lisp_t lambda(context&, lisp_t, lisp_t);
lisp_t nlambda(context&, lisp_t, lisp_t);
lisp_t list(context&, lisp_t);
lisp_t length(context&, lisp_t);
lisp_t closure(context&, lisp_t, lisp_t);

/// @brief Return the N'th element in the list LIST.  If N is greater than
/// the length of LIST, return nil.
///
lisp_t nth(context&, lisp_t, lisp_t);
lisp_t error(context&, lisp_t);
lisp_t exit(context&, lisp_t);

lisp_t closobj(context&, lisp_t);
} // namespace lisp::details::prim

#endif

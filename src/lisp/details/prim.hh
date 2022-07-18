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

#include <lisp/lisp.hh>

namespace lisp::details::prim
{
void init();

LISPT car(lisp&, LISPT);
LISPT cdr(lisp&, LISPT);
LISPT cadr(lisp&, LISPT);
LISPT cdar(lisp&, LISPT);
LISPT caar(lisp&, LISPT);
LISPT cddr(lisp&, LISPT);
LISPT cdddr(lisp&, LISPT);
LISPT caddr(lisp&, LISPT);
LISPT cdadr(lisp&, LISPT);
LISPT caadr(lisp&, LISPT);
LISPT cddar(lisp&, LISPT);
LISPT cadar(lisp&, LISPT);
LISPT cdaar(lisp&, LISPT);
LISPT caaar(lisp&, LISPT);
LISPT rplaca(lisp&, LISPT, LISPT);
LISPT rplacd(lisp&, LISPT, LISPT);
LISPT eq(lisp&, LISPT, LISPT);
LISPT atom(lisp&, LISPT);
LISPT nconc(lisp&, LISPT);
LISPT tconc(lisp&, LISPT, LISPT);
LISPT attach(lisp&, LISPT, LISPT);
LISPT append(lisp&, LISPT);
LISPT null(lisp&, LISPT);
LISPT quote(lisp&, LISPT);
LISPT lambda(lisp&, LISPT, LISPT);
LISPT nlambda(lisp&, LISPT, LISPT);
LISPT list(lisp&, LISPT);
LISPT length(lisp&, LISPT);
LISPT closure(lisp&, LISPT, LISPT);

/// @brief Return the N'th element in the list LIST.  If N is greater than
/// the length of LIST, return NIL.
///
LISPT nth(lisp&, LISPT, LISPT);
LISPT error(lisp&, LISPT);
LISPT uxexit(lisp&, LISPT);

LISPT closobj(lisp&, LISPT);
} // namespace lisp::details::prim

#endif

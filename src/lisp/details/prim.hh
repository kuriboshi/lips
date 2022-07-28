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

LISPT car(context&, LISPT);
LISPT cdr(context&, LISPT);
LISPT cadr(context&, LISPT);
LISPT cdar(context&, LISPT);
LISPT caar(context&, LISPT);
LISPT cddr(context&, LISPT);
LISPT cdddr(context&, LISPT);
LISPT caddr(context&, LISPT);
LISPT cdadr(context&, LISPT);
LISPT caadr(context&, LISPT);
LISPT cddar(context&, LISPT);
LISPT cadar(context&, LISPT);
LISPT cdaar(context&, LISPT);
LISPT caaar(context&, LISPT);
LISPT rplaca(context&, LISPT, LISPT);
LISPT rplacd(context&, LISPT, LISPT);
LISPT eq(context&, LISPT, LISPT);
LISPT atom(context&, LISPT);
LISPT nconc(context&, LISPT);
LISPT tconc(context&, LISPT, LISPT);
LISPT attach(context&, LISPT, LISPT);
LISPT append(context&, LISPT);
LISPT null(context&, LISPT);
LISPT quote(context&, LISPT);
LISPT lambda(context&, LISPT, LISPT);
LISPT nlambda(context&, LISPT, LISPT);
LISPT list(context&, LISPT);
LISPT length(context&, LISPT);
LISPT closure(context&, LISPT, LISPT);

/// @brief Return the N'th element in the list LIST.  If N is greater than
/// the length of LIST, return NIL.
///
LISPT nth(context&, LISPT, LISPT);
LISPT error(context&, LISPT);
LISPT uxexit(context&, LISPT);

LISPT closobj(context&, LISPT);
} // namespace lisp::details::prim

#endif

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

#ifndef LISP_DETAILS_LIST_HH
#define LISP_DETAILS_LIST_HH

#include <lisp/types.hh>

namespace lisp::details::list
{
void init();

lisp_t car(const lisp_t&);
lisp_t cdr(const lisp_t&);
lisp_t cadr(const lisp_t&);
lisp_t cdar(const lisp_t&);
lisp_t caar(const lisp_t&);
lisp_t cddr(const lisp_t&);
lisp_t cdddr(const lisp_t&);
lisp_t caddr(const lisp_t&);
lisp_t cdadr(const lisp_t&);
lisp_t caadr(const lisp_t&);
lisp_t cddar(const lisp_t&);
lisp_t cadar(const lisp_t&);
lisp_t cdaar(const lisp_t&);
lisp_t caaar(const lisp_t&);
lisp_t rplaca(lisp_t, const lisp_t&);
lisp_t rplacd(lisp_t, const lisp_t&);
lisp_t nconc(lisp_t);
lisp_t tconc(lisp_t, const lisp_t&);
lisp_t attach(const lisp_t&, lisp_t);
lisp_t append(const lisp_t&);
lisp_t null(const lisp_t&);
lisp_t list(const lisp_t&);
lisp_t length(const lisp_t&);
lisp_t nth(const lisp_t&, const lisp_t&);
} // namespace lisp::details::list

#endif

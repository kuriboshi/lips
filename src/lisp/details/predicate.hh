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

#ifndef LISP_DETAILS_PRED_HH
#define LISP_DETAILS_PRED_HH

#include <lisp/types.hh>

namespace lisp::details::predicate
{

void init();

lisp_t eq(lisp_t, lisp_t);
lisp_t atom(lisp_t);
lisp_t numberp(lisp_t);
lisp_t listp(lisp_t);
lisp_t memb(lisp_t, lisp_t);
lisp_t equal(lisp_t, lisp_t);
lisp_t nlistp(lisp_t);
lisp_t neq(lisp_t, lisp_t);
lisp_t boundp(lisp_t);
lisp_t litatom(lisp_t);
lisp_t xtypeof(lisp_t);

} // namespace lisp::details::predicate

#endif

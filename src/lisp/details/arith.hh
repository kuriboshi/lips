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

#ifndef LISP_DETAILS_ARITH_HH
#define LISP_DETAILS_ARITH_HH

#include <lisp/types.hh>

namespace lisp::details::arith
{

void init();

lisp_t abs(lisp_t);
lisp_t add1(lisp_t);
lisp_t difference(lisp_t, lisp_t);
lisp_t divide(lisp_t, lisp_t);
lisp_t eqp(lisp_t, lisp_t);
lisp_t fdifference(lisp_t, lisp_t);
lisp_t fdivide(lisp_t, lisp_t);
lisp_t fplus(lisp_t);
lisp_t ftimes(lisp_t);
lisp_t geq(lisp_t, lisp_t);
lisp_t greaterp(lisp_t, lisp_t);
lisp_t idifference(lisp_t, lisp_t);
lisp_t iminus(lisp_t);
lisp_t iplus(lisp_t);
lisp_t iquotient(lisp_t, lisp_t);
lisp_t iremainder(lisp_t, lisp_t);
lisp_t itimes(lisp_t);
lisp_t itof(lisp_t);
lisp_t leq(lisp_t, lisp_t);
lisp_t lessp(lisp_t, lisp_t);
lisp_t ltimes(lisp_t);
lisp_t minus(lisp_t);
lisp_t minusp(lisp_t);
lisp_t neqp(lisp_t, lisp_t);
lisp_t plus(lisp_t);
lisp_t sub1(lisp_t);
lisp_t zerop(lisp_t);

} // namespace lisp::details::arith

#endif

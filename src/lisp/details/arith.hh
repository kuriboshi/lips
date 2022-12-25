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

lisp_t abs(context&, lisp_t);
lisp_t add1(context&, lisp_t);
lisp_t difference(context&, lisp_t, lisp_t);
lisp_t divide(context&, lisp_t, lisp_t);
lisp_t eqp(context&, lisp_t, lisp_t);
lisp_t fdifference(context&, lisp_t, lisp_t);
lisp_t fdivide(context&, lisp_t, lisp_t);
lisp_t fplus(context&, lisp_t);
lisp_t ftimes(context&, lisp_t);
lisp_t geq(context&, lisp_t, lisp_t);
lisp_t greaterp(context&, lisp_t, lisp_t);
lisp_t idifference(context&, lisp_t, lisp_t);
lisp_t iminus(context&, lisp_t);
lisp_t iplus(context&, lisp_t);
lisp_t iquotient(context&, lisp_t, lisp_t);
lisp_t iremainder(context&, lisp_t, lisp_t);
lisp_t itimes(context&, lisp_t);
lisp_t itof(context&, lisp_t);
lisp_t leq(context&, lisp_t, lisp_t);
lisp_t lessp(context&, lisp_t, lisp_t);
lisp_t ltimes(context&, lisp_t);
lisp_t minus(context&, lisp_t);
lisp_t minusp(context&, lisp_t);
lisp_t neqp(context&, lisp_t, lisp_t);
lisp_t plus(context&, lisp_t);
lisp_t sub1(context&, lisp_t);
lisp_t zerop(context&, lisp_t);

} // namespace lisp::details::arith

#endif

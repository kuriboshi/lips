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

#ifndef LISP_DETAILS_ARITH_HH
#define LISP_DETAILS_ARITH_HH

#include <lisp/types.hh>

namespace lisp::details::arith
{

void init();

lisp_t abs(const lisp_t&);
lisp_t add1(const lisp_t&);
lisp_t difference(const lisp_t&, const lisp_t&);
lisp_t divide(const lisp_t&, const lisp_t&);
lisp_t eqp(const lisp_t&, const lisp_t&);
lisp_t fdifference(const lisp_t&, const lisp_t&);
lisp_t fdivide(const lisp_t&, const lisp_t&);
lisp_t fplus(const lisp_t&);
lisp_t ftimes(const lisp_t&);
lisp_t geq(const lisp_t&, const lisp_t&);
lisp_t greaterp(const lisp_t&, const lisp_t&);
lisp_t idifference(const lisp_t&, const lisp_t&);
lisp_t iminus(const lisp_t&);
lisp_t iplus(const lisp_t&);
lisp_t iquotient(const lisp_t&, const lisp_t&);
lisp_t iremainder(const lisp_t&, const lisp_t&);
lisp_t itimes(const lisp_t&);
lisp_t itof(const lisp_t&);
lisp_t leq(const lisp_t&, const lisp_t&);
lisp_t lessp(const lisp_t&, const lisp_t&);
lisp_t times(const lisp_t&);
lisp_t minus(const lisp_t&);
lisp_t minusp(const lisp_t&);
lisp_t neqp(const lisp_t&, const lisp_t&);
lisp_t plus(const lisp_t&);
lisp_t sub1(const lisp_t&);
lisp_t zerop(const lisp_t&);

} // namespace lisp::details::arith

#endif

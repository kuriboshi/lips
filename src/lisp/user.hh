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

#ifndef LISP_USER_HH
#define LISP_USER_HH

#include "types.hh"
#include "details/user.hh"

namespace lisp
{
/// @brief Defines a function.
///
/// The arguments are evaluated. Because the lambda and nlambda expressions are
/// functions the second argument should not be quoted. It's more common to use
/// the defineq function.
///
/// In lisp it would look like this:
/// @code{.lisp}
/// (define 'add2 (lambda (a) (plus a 2)))
/// @endcode
///
/// @param a Name of the function.
/// @param b A lambda or nlambda expression.
///
/// @returns The name of the function defined.
inline lisp_t define(lisp_t a, lisp_t b) { return details::user::define(a, b); }
/// @brief Defines one or more functions.
///
/// The argument is a list of the form
/// @code{.lisp}
/// (defineq
///   (add2
///    (lambda (a) (plus a 2)))
///   (sub2
///    (lambda (a) (difference a 2))))
/// @endcode
///
/// @param a A list of lists of two elements, a name and a lambda or nlambda
/// expression. The lambda expression is evaluated to yield a lambda object.
///
/// @returns List of function names defined.
inline lisp_t defineq(lisp_t a) { return details::user::defineq(a); }
/// @brief Get the function representation of a lambda object.
///
/// @param a A lambda object.
///
/// @returns The representation of a lambda function which if evaluated will
/// yield a lambda object.  If the parameter is not of the type lambda or if
/// it's empty then the return value is nil.
inline lisp_t getrep(lisp_t a) { return details::user::getrep(a); }
} // namespace lisp

#endif

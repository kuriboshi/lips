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

#ifndef LISP_PROP_HH
#define LISP_PROP_HH

/// @file property.hh
///
/// # Property List Functions
///
/// `lips` supports property lists on literal atoms. A property list is a list
/// of values stored in the _property cell_ of a literal atom. A property list
/// is list of alternating properties and values. For example the property list
/// `(a 1 b 2)` has two properties `a` and `b` with the values `1` and `2`
/// respectively. `eq` is used to compare properties when manipulating the
/// property list with the below functions.

#include "types.hh"
#include "details/property.hh"

namespace lisp
{
/// @brief Returns the entire property list stored in the property cell of _a_.
/// @lisp{(getplist a),Function}
inline lisp_t getplist(lisp_t a) { return details::property::getplist(a); }
/// @brief Returns the value of property _p_ stored in the property cell of the
/// literal atom _a_.
/// @lisp{(getprop a p),Function}
inline lisp_t getprop(lisp_t a, lisp_t p) { return details::property::getprop(a, p); }
/// @brief Puts the value _v_ in the property _p_ of _a_.
/// @lisp{(putprop a p v),Function}
inline lisp_t putprop(lisp_t a, lisp_t p, lisp_t v) { return details::property::putprop(a, p, v); }
/// @brief Removes the property _p_ from the literal atom _a_.
/// @lisp{(remprop a p),Function}
inline lisp_t remprop(lisp_t a, lisp_t p) { return details::property::remprop(a, p); }
/// @brief Sets the property list of _a_ to _pl_.
/// @lisp{(setplist a pl),Function}
inline lisp_t setplist(lisp_t a, lisp_t pl) { return details::property::setplist(a, pl); }
} // namespace lisp

#endif

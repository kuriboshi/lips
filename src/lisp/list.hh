//
// Lips, lisp shell.
// Copyright 2020-2024 Krister Joas
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

#ifndef LISP_LIST_HH
#define LISP_LIST_HH

/// @file list.hh
///
/// # List Functions

#include "types.hh"
#include "details/list.hh"

namespace lisp
{
/// @brief The car of the cons cell.
/// @lisp{(car a),Function}
inline lisp_t car(const lisp_t& a) { return details::list::car(a); }
/// @brief The cdr of the cons cell.
/// @lisp{(cdr a),Function}
inline lisp_t cdr(const lisp_t& a) { return details::list::cdr(a); }
/// @brief Same as (car (cdr a))
/// @lisp{(cadr a),Function}
inline lisp_t cadr(const lisp_t& a) { return details::list::cadr(a); }
/// @brief Same as (cdr (car a))
/// @lisp{(cdar a),Function}
inline lisp_t cdar(const lisp_t& a) { return details::list::cdar(a); }
/// @brief Same as (car (car a))
/// @lisp{(caar a),Function}
inline lisp_t caar(const lisp_t& a) { return details::list::caar(a); }
/// @brief Same as (cdr (cdr a))
/// @lisp{(cddr a),Function}
inline lisp_t cddr(const lisp_t& a) { return details::list::cddr(a); }
/// @brief Same as (cdr (cdr (cdr a)))
/// @lisp{(cdddr a),Function}
inline lisp_t cdddr(const lisp_t& a) { return details::list::cdddr(a); }
/// @brief Same as (car (cdr (cdr a)))
/// @lisp{(caddr a),Function}
inline lisp_t caddr(const lisp_t& a) { return details::list::caddr(a); }
/// @brief Same as (cdr (car (cdr a)))
/// @lisp{(cdadr a),Function}
inline lisp_t cdadr(const lisp_t& a) { return details::list::cdadr(a); }
/// @brief Same as (car (car (cdr a)))
/// @lisp{(caadr a),Function}
inline lisp_t caadr(const lisp_t& a) { return details::list::caadr(a); }
/// @brief Same as (cdr (cdr (car a)))
/// @lisp{(cddar a),Function}
inline lisp_t cddar(const lisp_t& a) { return details::list::cddar(a); }
/// @brief Same as (car (cdr (car a)))
/// @lisp{(cadar a),Function}
inline lisp_t cadar(const lisp_t& a) { return details::list::cadar(a); }
/// @brief Same as (cdr (car (car a)))
/// @lisp{(cdaar a),Function}
inline lisp_t cdaar(const lisp_t& a) { return details::list::cdaar(a); }
/// @brief Same as (car (car (car a)))
/// @lisp{(caaar a),Function}
inline lisp_t caaar(const lisp_t& a) { return details::list::caaar(a); }
/// @brief Same as `append` but modifies the arguments _args_.
/// @lisp{(nconc args...),NoSpread Function}
inline lisp_t nconc(lisp_t args) { return details::list::nconc(std::move(args)); }
/// @brief Destructive version of `cons` which prepends _x_ to _y_ and any
/// reference to _y_ will contain the modified list.
/// @lisp{(attach a b),Function}
inline lisp_t attach(const lisp_t& a, lisp_t b) { return details::list::attach(a, std::move(b)); }
/// @brief `t` if _x_ is `nil`, otherwise `nil`.
/// @lisp{(null a),Function}
inline lisp_t null(const lisp_t& a) { return details::list::null(a); }
/// @brief Create a list of the items _args_.
/// @lisp{(list args...),NoSpread Function}
inline lisp_t list(const lisp_t& a) { return details::list::list(a); }
/// @brief Returns the length of the list _l_.
/// @lisp{(length list),Function}
inline lisp_t length(const lisp_t& list) { return details::list::length(list); }
/// @brief Returns the _n_th tail of _x_.
/// @lisp{(nth x n),Function}
///
/// Returns `nil` if there are fewer elements in _x_ than _n_.
///
/// ```lisp
/// (nth '(a b c) 2)
///   => (b c)
/// (nth '(a b c) 4)
///   => nil
/// ```
inline lisp_t nth(const lisp_t& a, const lisp_t& b) { return details::list::nth(a, b); }
/// @brief Replaces `car` of _x_ with _y_ destructively.
/// @lisp{(rplaca x y),Function}
inline lisp_t rplaca(const lisp_t& x, const lisp_t& y) { return details::list::rplaca(x, y); }
/// @brief Replaces `cdr` of _x_ with _y_ destructively.
/// @lisp{(rplacd x y),Function}
inline lisp_t rplacd(lisp_t x, const lisp_t& y) { return details::list::rplacd(std::move(x), y); }
/// @brief Append all the arguments, i.e. _x2_ is appended to _x1_, _x3_ to
/// _x2_, and so on.
/// @lisp{(append args...),NoSpread Function}
///
/// All arguments must be lists. Any argument which is `nil` is ignored. All
/// lists are copied which means that `(append x)` makes a copy of _x_. All
/// arguments have to be lists.
///
/// ```lisp
/// (append '(a b) '(c d) '(e f))
///   => (a b c d e f)
/// ```
inline lisp_t append(const lisp_t& a) { return details::list::append(a); }
/// @brief The `car` of _l_ is a list and the `cdr` of _l_ is a pointer to the
/// first element of the list.
/// @lisp{(tconc l o),Function}
///
/// The object _o_ is added to the end of the list and the `cdr` is updated.
/// An empty _l_ should be `(nil)` but if _l_ is `nil` it is initialized to
/// `((o) o)`.  All pointers to _l_ points to the new list since the changes
/// are destructive.
inline lisp_t tconc(lisp_t l, const lisp_t& o) { return details::list::tconc(std::move(l), o); }
} // namespace lisp

#endif

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

#ifndef LISP_PRIM_HH
#define LISP_PRIM_HH

/// @file prim.hh
///
/// # Primary Functions

#include "types.hh"
#include "details/prim.hh"

namespace lisp
{
/// @brief The car of the cons cell.
/// @lisp{(car a),Function}
inline lisp_t car(lisp_t a) { return details::prim::car(a); }
/// @brief The cdr of the cons cell.
/// @lisp{(cdr a),Function}
inline lisp_t cdr(lisp_t a) { return details::prim::cdr(a); }
/// @brief Same as (car (cdr a))
/// @lisp{(cadr a),Function}
inline lisp_t cadr(lisp_t a) { return details::prim::cadr(a); }
/// @brief Same as (cdr (car a))
/// @lisp{(cdar a),Function}
inline lisp_t cdar(lisp_t a) { return details::prim::cdar(a); }
/// @brief Same as (car (car a))
/// @lisp{(caar a),Function}
inline lisp_t caar(lisp_t a) { return details::prim::caar(a); }
/// @brief Same as (cdr (cdr a))
/// @lisp{(cddr a),Function}
inline lisp_t cddr(lisp_t a) { return details::prim::cddr(a); }
/// @brief Same as (cdr (cdr (cdr a)))
/// @lisp{(cdddr a),Function}
inline lisp_t cdddr(lisp_t a) { return details::prim::cdddr(a); }
/// @brief Same as (car (cdr (cdr a)))
/// @lisp{(caddr a),Function}
inline lisp_t caddr(lisp_t a) { return details::prim::caddr(a); }
/// @brief Same as (cdr (car (cdr a)))
/// @lisp{(cdadr a),Function}
inline lisp_t cdadr(lisp_t a) { return details::prim::cdadr(a); }
/// @brief Same as (car (car (cdr a)))
/// @lisp{(caadr a),Function}
inline lisp_t caadr(lisp_t a) { return details::prim::caadr(a); }
/// @brief Same as (cdr (cdr (car a)))
/// @lisp{(cddar a),Function}
inline lisp_t cddar(lisp_t a) { return details::prim::cddar(a); }
/// @brief Same as (car (cdr (car a)))
/// @lisp{(cadar a),Function}
inline lisp_t cadar(lisp_t a) { return details::prim::cadar(a); }
/// @brief Same as (cdr (car (car a)))
/// @lisp{(cdaar a),Function}
inline lisp_t cdaar(lisp_t a) { return details::prim::cdaar(a); }
/// @brief Same as (car (car (car a)))
/// @lisp{(caaar a),Function}
inline lisp_t caaar(lisp_t a) { return details::prim::caaar(a); }
/// @brief `t` if the objects _a_ and _b_ are the same object. Integers are
/// considered 'eq' if their values are the same.
/// @lisp{(eq a b),Function}
inline lisp_t eq(lisp_t a, lisp_t b) { return details::prim::eq(a, b); }
/// @brief `t` if _a_ is `nil`, `t`, a symbol, an integer, or a floating point
/// object.
/// @lisp{(atom a),Function}
inline lisp_t atom(lisp_t a) { return details::prim::atom(a); }
/// @brief Same as `append` but modifies the arguments _args_.
/// @lisp{(nconc args...),NoSpread Function}
inline lisp_t nconc(lisp_t args) { return details::prim::nconc(args); }
/// @brief Destructive version of `cons` which prepends _x_ to _y_ and any
/// reference to _y_ will contain the modified list.
/// @lisp{(attach a b),Function}
inline lisp_t attach(lisp_t a, lisp_t b) { return details::prim::attach(a, b); }
/// @brief `t` if _x_ is `nil`, otherwise `nil`.
/// @lisp{(null a),Function}
inline lisp_t null(lisp_t a) { return details::prim::null(a); }
/// @brief Returns _x_ unevaluated.
/// @lisp{(quote a),NLambda Function}
inline lisp_t quote(lisp_t a) { return details::prim::quote(a); }
/// @brief Creates a lambda object.
/// @lisp{(lambda x . y),NoSpread Function}
///
/// The parameter _x_ is the parameters of the function being defined. If it's
/// a list of atoms the function is a spread function, if it's a single atoms
/// the function is a nospread function, if it's dotted pair the function is a
/// half spread function.
///
/// A _spread_ function binds each formal parameter to the actual parameters
/// when the function is called. Any excess parameter is ignored and any
/// missing actual parameter is bound to `nil`.
///
/// A _nospread_ function binds the formal parameter to a list of all actual
/// parameters when called.
///
/// A _half spread_ function is a combination of the above where the actual
/// parameters are bound to each formal parameter and any excess actual
/// parameters are bound to the formal parameter in the symbol in the `cdr` of
/// the list of formal parameters.
inline lisp_t lambda(lisp_t x, lisp_t y) { return details::prim::lambda(x, y); }
/// @brief Creates an nlambda function object.
/// @lisp{(nlambda x . y)
///
/// Same as `lambda` except that the function object is an nlambda function
/// object and parameters are not evaluated when the function is called.
inline lisp_t nlambda(lisp_t x, lisp_t y) { return details::prim::nlambda(x, y); }
/// @brief Create a list of the items _args_.
/// @lisp{(list args...),NoSpread Function}
inline lisp_t list(lisp_t a) { return details::prim::list(a); }
/// @brief Returns the length of the list _l_.
/// @lisp{(length list),Function}
inline lisp_t length(lisp_t list) { return details::prim::length(list); }
/// @brief Eval function that forms the closure of function _f_, with variables
/// listed in _v_ statically bound.
/// @lisp{(closure f v),Function}
///
/// This is close to function in other lisp dialects. Any closure that is
/// created within another closure and lists a variable contained in that
/// closure refers to the same variable. This makes it possible for two
/// closures to share one or more variables.
///
/// Here is an example of defining a simple function which maintains the
/// balance of a bank account.
///
/// ```lisp
/// (defineq
///   (make-account
///    (lambda (balance)
///      ((closure
///           '(progn
///             (setq withdraw
///              (closure
///                  (lambda (amount)
///                    (setq balance (difference balance amount)))
///                  '(balance)))
///             (setq deposit
///              (closure
///                  (lambda (amount)
///                    (setq balance (plus balance amount)))
///                  '(balance)))
///             (lambda ()
///               (closure
///                   (lambda (m)
///                     (cond
///                       ((eq m 'withdraw) withdraw)
///                       ((eq m 'deposit) deposit)
///                       (t nil)))
///                   '(withdraw deposit))))
///           '(balance withdraw deposit))))))
/// ```
///
/// The function `make-account` creates and returns a closure object which
/// binds the three symbols on line 24 in their lexical scope. It sets the
/// symbols `withdraw` and `deposit` each to a closure over `balance` with a
/// lambda expression which subtracts or adds an `amount` to the `balance`.
inline lisp_t closure(lisp_t a, lisp_t b) { return details::prim::closure(a, b); }
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
inline lisp_t nth(lisp_t a, lisp_t b) { return details::prim::nth(a, b); }
inline lisp_t error(lisp_t a) { return details::prim::error(a); }

/// @brief Replaces `car` of _x_ with _y_ destructively.
/// @lisp{(rplaca x y),Function}
inline lisp_t rplaca(lisp_t x, lisp_t y) { return details::prim::rplaca(x, y); }
/// @brief Replaces `cdr` of _x_ with _y_ destructively.
/// @lisp{(rplacd x y),Function}
inline lisp_t rplacd(lisp_t x, lisp_t y) { return details::prim::rplacd(x, y); }
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
inline lisp_t append(lisp_t a) { return details::prim::append(a); }
/// @brief The `car` of _l_ is a list and the `cdr` of _l_ is a pointer to the
/// first element of the list.
/// @lisp{(tconc l o),Function}
///
/// The object _o_ is added to the end of the list and the `cdr` is updated.
/// An empty _l_ should be `(nil)` but if _l_ is `nil` it is initialized to
/// `((o) o)`.  All pointers to _l_ points to the new list since the changes
/// are destructive.
inline lisp_t tconc(lisp_t l, lisp_t o) { return details::prim::tconc(l, o); }
} // namespace lisp

#endif

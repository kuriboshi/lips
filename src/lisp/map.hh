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

#pragma once

/// @file map.hh
///
/// # Map Functions
///
/// Map functions iterate over a list of items and applies a function to either
/// each item or each tail of the list.

#include "types.hh"
#include "details/map.hh"

namespace lisp
{
/// @brief Apply _fn1_ on each tail of _list_.
/// @lisp{(map list fn1 fn2),Function}
///
/// If _fn2_ is `nil`, apply the function _fn1_ on each tail of
/// _list_. First _fn1_ is applied to _list_, then `(cdr list)`,
/// and so on. If _fn2_ is not `nil`\ then _fn2_ is called instead
/// of `cdr` to get the next value on which to apply _fn1_.
/// `map` returns `nil`.
///
/// ```lisp
/// (map '(a b c) (lambda (l) (print l)))
/// (a b c)
/// (b c)
/// (c)
///   => nil
/// ```
///
/// @param list A list of items.
/// @param fn1 The function to apply on each tail of obj.
/// @param fn2 Function to apply to get the next element of the list (default is CDR).
///
/// @returns nil
inline lisp_t map(const lisp_t& list, const lisp_t& fn1, const lisp_t& fn2)
{
  return details::map::map(list, fn1, fn2);
}
/// @brief Apply _fn1_ on each _car_ of _list_.
/// @lisp{(mapc list fn1 fn2),Function}
///
/// `mapc` is the same as `map` except that _fn1_ is applied to `(car list)`
/// instead of the list. Effectively applying _fn1_ on each element of the
/// list. `mapc` returns `nil`.
///
/// @param list A list of items. If not a list the function is a no-op.
/// @param fn1 The function to apply on each CAR of the list.
/// @param fn2 Function to apply to get the next element (default is CDR).
///
/// @returns nil
inline lisp_t mapc(const lisp_t& list, const lisp_t& fn1, const lisp_t& fn2)
{
  return details::map::mapc(list, fn1, fn2);
}
/// @brief Collect the result of applying _fn1_ on each tail of _list_.
/// @lisp{(maplist list fn1 fn2),Function}
///
/// The same as `map` but collects the results from applying _fn1_ on each tail
/// and returning a list of the results.
///
/// ```lisp
/// (maplist '(a b c) (lambda (l) (length l)))
///   => (3 2 1)
/// ```
///
/// @param list A list of items.
/// @param fn1 The function to apply on each tail of the list.
/// @param fn2 Function to apply to get the next element (default is CDR).
///
/// @returns A list of the result of applying FN1 on each element in the list.
inline lisp_t maplist(const lisp_t& list, const lisp_t& fn1, const lisp_t& fn2)
{
  return details::map::maplist(list, fn1, fn2);
}
/// @brief Collect the result of applying _fn1_ on each _car_ of _list_.
/// @lisp{(mapcar list fn1 fn2),Function}
///
/// Equivalent to `mapc` but collects the results in a list like `maplist`.
///
/// ```lisp
/// (mapcar '(1 2 3) (lambda (n) (plus n 1)))
///   => (2 3 4)
/// ```
///
/// @param list A list of items.
/// @param fn1 The function to apply on each element of the list.
/// @param fn2 Function to apply to get the next element (default is CDR).
///
/// @returns A list of the result of applying FN1 on each element in the list.
inline lisp_t mapcar(const lisp_t& list, const lisp_t& fn1, const lisp_t& fn2)
{
  return details::map::mapcar(list, fn1, fn2);
}
} // namespace lisp

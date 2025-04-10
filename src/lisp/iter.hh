//
// Lips, lisp shell.
// Copyright 2020-2023, 2025 Krister Joas
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

#include <iterator>

#include "types.hh"

namespace lisp
{
/// @brief Iterates over a list
///
/// Usage:
/// ```cpp
/// auto list{"(a b c)"_l}
/// for(auto i: list)
///   f(i); // Call 'f' on each element in 'list' (i.e. a, b, and c)
/// ```
class iterator
{
public:
  using iterator_category = std::forward_iterator_tag;
  using value_type = lisp_t;
  using difference_type = std::ptrdiff_t;
  using pointer = lisp_t*;
  using reference = lisp_t&;

  iterator(lisp_t l)
    : _car(std::move(l))
  {}
  bool operator==(const iterator& x) const { return _car == x._car; }
  bool operator!=(const iterator& x) const { return _car != x._car; }
  iterator& operator++()
  {
    if(type_of(_car) == object::type::Cons)
      _car = _car->cdr();
    else
      _car = nullptr;
    return *this;
  }
  iterator operator++(int)
  {
    iterator tmp(*this);
    ++(*this);
    return tmp;
  }
  lisp_t operator*() const
  {
    if(type_of(_car) == object::type::Cons)
      return _car->car();
    return nullptr;
  }

private:
  lisp_t _car;
};

inline iterator begin(lisp_t l) { return {std::move(l)}; }
inline iterator end(const lisp_t&) { return {nullptr}; }

} // namespace lisp

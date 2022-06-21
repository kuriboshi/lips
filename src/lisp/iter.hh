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

#ifndef LISP_ITER_HH
#define LISP_ITER_HH

#include <iterator>
#include "lisp.hh"

namespace lisp
{
class iterator
{
public:
  using iterator_category = std::forward_iterator_tag;
  using value_type = LISPT;
  using difference_type = std::ptrdiff_t;
  using pointer = LISPT*;
  using reference = LISPT&;

  iterator(LISPT l)
    : _car(l)
  {}
  bool operator==(const iterator& x) const { return _car == x._car; }
  bool operator!=(const iterator& x) const { return _car != x._car; }
  iterator& operator++()
  {
    if(type_of(_car) == type::CONS)
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
  LISPT operator*() const
  {
    if(type_of(_car) == type::CONS)
      return _car->car();
    else
      return nullptr;
  }

private:
  LISPT _car;
};

inline iterator begin(LISPT l) { return iterator(l); }
inline iterator end(LISPT l) { return iterator(nullptr); }

} // namespace lisp

#endif

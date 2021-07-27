//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#pragma once

#include <iterator>

#include "lisp.hh"

namespace lisp
{
class iterator: public std::iterator<std::forward_iterator_tag, LISPT>
{
public:
  iterator(LISPT l): _car(l) {}
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
  LISPT operator*() const { return _car->car(); }

private:
  LISPT _car;
};

inline iterator begin(LISPT l) { return iterator(l); }
inline iterator end(LISPT l) { return iterator(nullptr); }

} // namespace lisp

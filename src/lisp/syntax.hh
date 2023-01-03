//
// Lips, lisp shell.
// Copyright 2022-2023 Krister Joas
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

#ifndef LISP_SYNTAX_HH
#define LISP_SYNTAX_HH

#include <cstdint>
#include <array>

#include "ref_ptr.hh"
#include "types.hh"

namespace lisp
{

class object;
using lisp_t = ref_ptr<object>;

// ^# start of line comment
// #' Function quote
// ' Quote
// " String
// ! Repeat
// ?? History
// >
// >>
// <
// |
// &
// *
// `
// ,@
// ;

class syntax
{
public:
  syntax() { reset(); }
  enum class type
  {
    OTHER = 0,
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACKET,
    RIGHT_BRACKET,
    STRING_DELIM,
    ESCAPE,
    BREAKCHAR,
    SEPARATOR,
    //
    QUOTE,
    // Integers and floating point numbers
    EXPONENT,
    SIGN,
    DIGIT,
    DECIMAL_POINT,
    // Comments
    COMMENT,
    SHELL_COMMENT,
    NEWLINE,
    // Macros
    MACRO,
    SPLICE,
    INFIX
  };
  type get(std::uint8_t index) const { return _table[index]; }
  void set(std::uint8_t index, type value) { _table[index] = value; }
  void set(std::uint8_t index, lisp_t value)
  {
    set(index, type::MACRO);
    _macro[index] = value;
  }
  lisp_t macro(ref_file_t source, std::uint8_t index);
  /// @brief Reset read table to the defaults.
  void reset()
  {
    set('(', type::LEFT_PAREN);
    set(')', type::RIGHT_PAREN);
    set('[', type::LEFT_BRACKET);
    set(']', type::RIGHT_BRACKET);
    set('"', type::STRING_DELIM);
    set('\\', type::ESCAPE);
    set(' ', type::SEPARATOR);
    set('\t', type::SEPARATOR);
    set('\n', type::NEWLINE);
    set('0', type::DIGIT);
    set('1', type::DIGIT);
    set('2', type::DIGIT);
    set('3', type::DIGIT);
    set('4', type::DIGIT);
    set('5', type::DIGIT);
    set('6', type::DIGIT);
    set('7', type::DIGIT);
    set('8', type::DIGIT);
    set('9', type::DIGIT);
    set('+', type::SIGN);
    set('-', type::SIGN);
    set('.', type::DECIMAL_POINT);
    set('e', type::EXPONENT);
    set('E', type::EXPONENT);
    set(';', type::COMMENT);
    set('#', type::SHELL_COMMENT);
    set('\'', type::QUOTE);
  }

private:
  std::array<type, 256> _table = {type::OTHER};
  std::array<lisp_t, 256> _macro = {nil};
};

} // namespace lisp

#endif

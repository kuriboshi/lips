//
// Lips, lisp shell.
// Copyright 2022-2023, 2025 Krister Joas
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

#include <cstdint>
#include <array>
#include <limits>
#include <numeric>
#include <utility>

#include "types.hh"

namespace lisp
{

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

class syntax final
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
  type get(std::uint8_t index) const { return _table.at(index); }
  void set(std::uint8_t index, type value) { _table.at(index) = value; }
  void macro(std::uint8_t index, lisp_t value)
  {
    set(index, type::MACRO);
    _macro.at(index) = std::move(value);
  }
  void splice(std::uint8_t index, lisp_t value)
  {
    set(index, type::SPLICE);
    _macro.at(index) = std::move(value);
  }
  void infix(std::uint8_t index, lisp_t value)
  {
    set(index, type::INFIX);
    _macro.at(index) = std::move(value);
  }
  lisp_t macro(ref_file_t source, std::uint8_t index);
  /// @brief Reset read table to the defaults.
  void reset()
  {
    _table.fill(type::OTHER);
    _macro.fill(nil);
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
  static constexpr const auto TABLE_SIZE = std::numeric_limits<std::uint8_t>::max() + 1;
  std::array<type, TABLE_SIZE> _table{};
  std::array<lisp_t, TABLE_SIZE> _macro{};
};

} // namespace lisp

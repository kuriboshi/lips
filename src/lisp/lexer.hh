//
// Lips, lisp shell.
// Copyright 2022 Krister Joas
//

#ifndef LISP_LEXER_HH
#define LISP_LEXER_HH

#include <iostream>
#include <string>
#include <array>
#include <vector>
#include "lisp.hh"
#include "io.hh"

namespace lisp
{

///
/// @brief Represents one token in a stream of tokens.
///
/// @details Each token can be of one of the following types.
///
/// @li @c EMPTY An empty token signalling the end of a stream or an error
///   tokenizing the stream.
/// @li @c MACRO A macro token. This includes the left and right parenthesis,
///   the left and right super parenthesis, a single quote, and a dot used to
///   create dotted pairs.
/// @li @c STRING A string. Double quotes can be embedded by using the
///   backslash, common in other Unix tools. Note that this differs from
///   Interlisp which uses a percent sign for this purpose.
/// @li @c SYMBOL A literal symbol.
/// @li @c INT An integer.
/// @li @c FLOAT A floating point value.
///
struct token_t final
{
  /// @brief The type of token.
  enum class type
  {
    EMPTY,                      // No type
    SPECIAL,                    // Special characters like '(', ')', '[', etc.
    STRING,                     // Literal string
    SYMBOL,                     // Atom or symbol
    INT,                        // Integer
    FLOAT,                      // Floating point number
    MACRO,                      // Read-macro
    SPLICE,                     // Splice read-macro
    INFIX                       // Infix read-macro
  };

  /// @brief Token type.
  enum type type;
  /// @brief A string representation of the token.
  std::string token;

  /// @brief Default constructor is the empty token.
  token_t() : type(type::EMPTY) {}
  /// @brief Construct a token of a certain type but with an empty token
  /// string.
  ///
  /// @param t The type of token.
  token_t(enum type t) : type(t) {}
  /// @brief Construct a token of a certain type with the token string
  /// representation given.
  ///
  /// @param t The type of token.
  /// @param s The token string.
  token_t(enum type t, const std::string& s) : type(t), token(s) {}
  /// @brief Default destructor.
  ~token_t() = default;
  /// @brief The operator bool for use in bool contexts.
  ///
  /// @returns True if the token is not the empty token (type EMPTY).
  explicit operator bool() const { return type != type::EMPTY; }

  /// @brief Default copy constructor.
  token_t(const token_t& t) = default;
  /// @brief Copy and move assignment operator.
  token_t& operator=(token_t t) noexcept
  {
    swap(*this, t);
    return *this;
  }
  /// @brief The move constructor.
  ///
  /// @details The moved from token becomes an empty token.
  token_t(token_t&& t) : type(t.type), token(std::move(t.token))
  {
    t.type = type::EMPTY;
  }

  /// @brief Checks that the token is of type SPECIAL and the value matches the
  /// character in the argument.
  ///
  /// @param c The macro character.
  /// @returns True if the SPECIAL character matches the @c c.
  bool is_special(char c) const
  {
    return type == type::SPECIAL && !token.empty() && token[0] == c;
  }

  /// @brief The swap function for use in the assignment operators.
  friend void swap(token_t& left, token_t& right) noexcept
  {
    using std::swap;
    swap(left.token, right.token);
    swap(left.type, right.type);
  }
};

inline bool operator==(const token_t& l, const token_t& r)
{
  return l.type == r.type && l.token == r.token;
}

inline std::ostream& operator<<(std::ostream& os, const token_t& t)
{
  switch(t.type)
  {
    case token_t::type::SPECIAL:
      os << "SPECIAL:";
      break;
    case token_t::type::STRING:
      os << "STRING:";
      break;
    case token_t::type::SYMBOL:
      os << "SYMBOL:";
      break;
    case token_t::type::INT:
      os << "INT:";
      break;
    case token_t::type::FLOAT:
      os << "FLOAT:";
      break;
    case token_t::type::MACRO:
      os << "MACRO:";
      break;
    case token_t::type::SPLICE:
      os << "SPLICE:";
      break;
    case token_t::type::INFIX:
      os << "INFIX:";
      break;
    default:
      os << "?:";
      break;
  }
  os << t.token;
  return os;
}

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
  syntax()
  {
    reset();
  }
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
  type get(std::uint8_t index) const
  {
    return _table[index];
  }
  void set(std::uint8_t index, type value)
  {
    _table[index] = value;
  }
  /// @brief Reset read table to the defaults.
  void reset() {
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
  std::array<type, 256> _table = {type::OTHER};
};

class read_macros
{
public:
  read_macros() = default;
  enum class type
  {
    MACRO,
    SPLICE,
    INFIX
  };
private:
  using macro_t = std::pair<type, std::function<LISPT(LISPT, LISPT)>>;
  std::array<macro_t, 256> matrix;
};

/// @brief A lexer of a string input.
class lexer
{
public:
  lexer(io::source& input) : _input(input), _pos(_input.begin()) {}
  /// @brief Read the next token from the input string.
  token_t read();
  void unread(token_t);
  io::source& input() const { return _input; }
  LISPT macro(token_t) { return NIL; }

private:
  io::source& _input;
  typename io::source::iterator _pos;
  token_t _token;
  bool _start_of_line{true};

  void next()
  {
    _start_of_line = _syntax.get(*_pos) == syntax::type::NEWLINE;
    ++_pos;
  }

  enum class state_t
  {
    START,                     // Starting state
    IN_STRING,                 // Inside a string until the next unescaped
                               // double quote
    IN_SYMBOL,                 // A symbol
    IN_QUOTE,                  // Next character is treated as literal
    IN_COMMENT,                // Inside comment
    IN_HASH,                   // Encountered #, wait for ', otherwise it's symbol
    // If we fail to parse an integer or floating point number the token will
    // turn into a symbol.
    IN_DOT,                    // A dot may be the start of a float or a symbol
                               // unless it's followed by a terminating
                               // character, like '(', ')', or a white space.
    IN_SIGN,                   // A lone '+' or '-' is an atom, not an integer.
    IN_INT,                    // An integer which may turn into a float if an
                               // 'e' or a decimal point is found. It may also
                               // turn into a symbol if there are any
                               // non-number, non-break characters in the
                               // sequence, e.g. '123abc'.
    IN_FLOAT,                  // Try to parse a floating point number.
    IN_EXP1,                   // The state after finding an 'e'.
    IN_EXP2,                   // Next state after finding a '+', '-', or a
                               // digit.
  };
  friend std::ostream& operator<<(std::ostream& os, enum state_t state)
  {
    switch(state)
    {
      case state_t::START:
        os << "START";
        break;
      case state_t::IN_STRING:
        os << "IN_STRING";
        break;
      case state_t::IN_SYMBOL:
        os << "IN_SYMBOL";
        break;
      case state_t::IN_QUOTE:
        os << "IN_QUOTE";
        break;
      case state_t::IN_COMMENT:
        os << "IN_COMMENT";
        break;
      case state_t::IN_DOT:
        os << "IN_DOT";
        break;
      case state_t::IN_SIGN:
        os << "IN_SIGN";
        break;
      case state_t::IN_INT:
        os << "IN_INT";
        break;
      case state_t::IN_FLOAT:
        os << "IN_FLOAT";
        break;
      case state_t::IN_EXP1:
        os << "IN_EXP1";
        break;
      case state_t::IN_EXP2:
        os << "IN_EXP2";
        break;
      case state_t::IN_HASH:
        os << "IN_HASH";
        break;
    }
    return os;
  }

  syntax _syntax;
};

}

#endif

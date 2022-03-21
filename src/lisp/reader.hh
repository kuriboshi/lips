//
// Lips, lisp shell.
//
// Copyright 2022 Krister Joas
//

#pragma once

#include <iostream>
#include <string>

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
    EMPTY,
    MACRO,
    STRING,
    SYMBOL,
    INT,
    FLOAT
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

  /// @brief Checks that the token is of type MACRO and the value matches the
  /// character in the argument.
  ///
  /// @param c The macro character.
  /// @returns True if the MACRO character matches the @c c.
  bool is_macro(char c) const
  {
    return type == type::MACRO && !token.empty() && token[0] == c;
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
    case token_t::type::MACRO:
      os << "MACRO:";
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
    default:
      os << "?:";
      break;
  }
  os << t.token;
  return os;
}

/// @brief A reader/lexer of a string input.
template<typename Input>
class reader
{
public:
  reader(Input& input) : _input(input), _pos(_input.begin()) {}
  /// @brief Read the next token from the input string.
  token_t read();
  void unread(token_t);
  token_t ratom();

private:
  Input& _input;
  typename Input::iterator _pos;
  token_t _token;

  enum class state_t
  {
    START,                     // Starting state
    IN_STRING,                 // Inside a string until the next unescaped
                               // double quote
    IN_SYMBOL,                 // A symbol
    IN_QUOTE,                  // Next character is treated as literal
    IN_COMMENT,                // Inside comment
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
    }
    return os;
  }
};

/// @brief Read the next token from the input stream.
/// @returns Returns either the token as a string or the empty token when the
///   input stream reaches the end.
template<typename Input>
token_t reader<Input>::read()
{
  if(_token)
    return std::move(_token);
  token_t token;
  state_t state{state_t::START};
  while(_pos != _input.end())
  {
    switch(state)
    {
      case state_t::START:
        switch(*_pos)
        {
          case '#':
            state = state_t::IN_COMMENT;
            break;
          case ' ': case '\n': case '\t':
            break;
          case '(': case ')': case '[': case ']':
          case '\'':
            token.type = token_t::type::MACRO;
            token.token.push_back(*_pos);
            ++_pos;
            return token;
          case '.':
            // A symbol may start with a dot so we assume it's a macro
            // character but look ahead one character.
            state = state_t::IN_DOT;
            token.type = token_t::type::MACRO;
            token.token.push_back(*_pos);
            break;
          case '"':
            state = state_t::IN_STRING;
            token.type = token_t::type::STRING;
            break;
          case '-': case '+':
            state = state_t::IN_SIGN;
            token.type = token_t::type::SYMBOL; // Assume symbol
            token.token.push_back(*_pos);
            break;
          case '0': case '1': case '2': case '3': case '4':
          case '5': case '6': case '7': case '8': case '9':
            state = state_t::IN_INT;
            token.type = token_t::type::INT;
            token.token.push_back(*_pos);
            break;
          default:
            state = state_t::IN_SYMBOL;
            token.type = token_t::type::SYMBOL;
            token.token.push_back(*_pos);
            break;
        }
        break;
      case state_t::IN_SYMBOL:
        // Any character except unquoted terminating macro characters are
        // included in the symbol. This includes double quotes.
        switch(*_pos)
        {
          case '#':
            state = state_t::IN_COMMENT;
            return token;
          case '(': case ')': case '[': case ']':
          case ' ': case '\n': case '\t':
            return token;
          case '\\':
            ++_pos;
            if(_pos == _input.end())
              return token;
            token.token.push_back(*_pos);
            break;
          default:
            token.token.push_back(*_pos);
            break;
        }
        break;
      case state_t::IN_QUOTE:
        token.token.push_back(*_pos);
        state = state_t::IN_STRING;
        break;
      case state_t::IN_STRING:
        switch(*_pos)
        {
          case '\\':
            state = state_t::IN_QUOTE;
            break;
          case '"':
            ++_pos;
            return token;
          default:
            token.token.push_back(*_pos);
            break;
        }
        break;
      case state_t::IN_COMMENT:
        if(*_pos == '\n')
          state = state_t::START;
        break;
      case state_t::IN_SIGN:
        switch(*_pos)
        {
          case '0': case '1': case '2': case '3': case '4':
          case '5': case '6': case '7': case '8': case '9':
            state = state_t::IN_INT;
            token.token.push_back(*_pos);
            break;
          default:
            state = state_t::IN_SYMBOL;
            token.type = token_t::type::SYMBOL;
            continue;
        }
        break;
      case state_t::IN_INT:
        switch(*_pos)
        {
          case '#':
            state = state_t::IN_COMMENT;
            return token;
          case '.':
            state = state_t::IN_FLOAT;
            token.type = token_t::type::FLOAT;
            break;
          case 'e': case 'E':
            state = state_t::IN_EXP1;
            break;
          case '0': case '1': case '2': case '3': case '4':
          case '5': case '6': case '7': case '8': case '9':
            break;
          case ' ': case '\t': case '\n':
          case '(': case ')': case '[': case ']':
            return token;
          default:
            token.type = token_t::type::SYMBOL;
            state = state_t::IN_SYMBOL;
            break;
        }
        token.token.push_back(*_pos);
        break;
      case state_t::IN_FLOAT:
        switch(*_pos)
        {
          case '#':
            state = state_t::IN_COMMENT;
            return token;
          case 'e': case 'E':
            state = state_t::IN_EXP1;
            break;
          case '0': case '1': case '2': case '3': case '4':
          case '5': case '6': case '7': case '8': case '9':
            break;
          default:
            state = state_t::IN_SYMBOL;
            token.type = token_t::type::SYMBOL;
            break;
        }
        token.token.push_back(*_pos);
        break;
      case state_t::IN_EXP1:
        switch(*_pos)
        {
          case '#':
            state = state_t::IN_COMMENT;
            return token;
          case '-': case '+':
          case '0': case '1': case '2': case '3': case '4':
          case '5': case '6': case '7': case '8': case '9':
            state = state_t::IN_EXP2;
            break;
          default:
            state = state_t::IN_SYMBOL;
            token.type = token_t::type::SYMBOL;
            break;
        }            
        token.token.push_back(*_pos);
        break;
      case state_t::IN_EXP2:
        switch(*_pos)
        {
          case '#':
            state = state_t::IN_COMMENT;
            return token;
          case '0': case '1': case '2': case '3': case '4':
          case '5': case '6': case '7': case '8': case '9':
            break;
          default:
            state = state_t::IN_SYMBOL;
            token.type = token_t::type::SYMBOL;
            break;
        }
        token.token.push_back(*_pos);
        break;
      case state_t::IN_DOT:
        switch(*_pos)
        {
          case '#':
            state = state_t::IN_COMMENT;
            return token;
          case '0': case '1': case '2': case '3': case '4':
          case '5': case '6': case '7': case '8': case '9':
            state = state_t::IN_FLOAT;
            continue;
          case '(': case ')': case '[': case ']':
          case '\n': case '\t': case ' ':
            return token;
          default:
            state = state_t::IN_SYMBOL;
            token.type = token_t::type::SYMBOL;
            continue;
        }
        break;
      default:
        break;
    }
    ++_pos;
  }
  return token;
}

template<typename Input>
void reader<Input>::unread(token_t token)
{
  _token = token;
}

}

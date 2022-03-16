//
// Lips, lisp shell.
//
// Copyright 2022 Krister Joas
//

#pragma once

#include <iostream>
#include <optional>
#include <string>

namespace lisp
{

struct token_t
{
  enum class type
  {
    MACRO,
    STRING,
    SYMBOL,
    INT,
    FLOAT
  } type;
  token_t() : type(type::SYMBOL) {}
  token_t(enum type t, const std::string& s) : type(t), token(s) {}
  std::string token;
  bool is_macro(char c) const
  {
    return type == type::MACRO && !token.empty() && token[0] == c;
  }
};

/// @brief A reader/lexer of a string input.
template<typename Input>
class Reader
{
public:
  Reader(Input& input) : _input(input), _pos(_input.begin()) {}
  /// @brief Read the next token from the input string.
  std::optional<token_t> read();
  void unread(token_t);

private:
  Input& _input;
  typename Input::iterator _pos;
  std::optional<token_t> _token;
};

/// @brief Read the next token from the input string.
/// @return Returns either the token as a string or nothing when the input
///   string reaches the end.
template<typename Input>
std::optional<token_t> Reader<Input>::read()
{
  if(_token)
  {
    // A simple move from _token doesn't work because the moved from object
    // will still contain a value.
    auto t = _token;
    _token.reset();
    return t;
  }
  if(_pos == _input.end())
    return {};
  token_t token;
  enum class state_t
  {
    START,                     // Starting state
    IN_STRING,                 // Inside a string until the next unescaped
                               // double quote
    IN_QUOTE,                  // Next character is treated as literal
    IN_SYMBOL,                 // A symbol
    IN_DOT,                    // A dot may be the start of a float or a symbol
                               // unless it's followed by a terminating
                               // character, like '(', ')', or a white space.
    // A non-numeric character terminates the integer, e.g 123abc is parsed as
    // two tokens '123' and 'abc'.
    IN_INT,                    // An integer which may turn into a float if an
                               // 'e' or a decimal point is found.
    // If we fail to parse a floating point the token will turn into a symbol.
    IN_FLOAT,                  // Try to parse a floating point number.
    IN_EXP1,                   // The state after finding an 'e'.
    IN_EXP2,                   // Next state after finding a '+', '-', or a
                               // digit.
  };
  state_t state{state_t::START};
  while(_pos != _input.end())
  {
    switch(state)
    {
      case state_t::START:
        switch(*_pos)
        {
          case '(': case ')': case '[': case ']':
            // These characters are terminating macro characters.
            if(!token.token.empty())
              return token;
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
          case ' ': case '\n': case '\t':
            // Terminate and return the current token.
            if(!token.token.empty())
              return token;
            break;
          case '"':
            if(!token.token.empty())
              return token;
            state = state_t::IN_STRING;
            token.type = token_t::type::STRING;
            break;
          case '-': case '+':
          case '0': case '1': case '2': case '3': case '4':
          case '5': case '6': case '7': case '8': case '9':
            if(!token.token.empty())
              return token;
            state = state_t::IN_INT;
            token.type = token_t::type::INT;
            token.token.push_back(*_pos);
            break;
          default:
            state = state_t::IN_SYMBOL;
            token.token.push_back(*_pos);
            break;
        }
        break;
      case state_t::IN_SYMBOL:
        // Any character except unquoted terminating macro characters are
        // included in the symbol. This includes double quotes.
        switch(*_pos)
        {
          case '(': case ')': case '[': case ']':
          case ' ': case '\n': case '\t':
            state = state_t::START;
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
            state = state_t::START;
            break;
          default:
            token.token.push_back(*_pos);
            break;
        }
        break;
      case state_t::IN_INT:
        switch(*_pos)
        {
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
          default:
            state = state_t::START;
            return token;
        }
        token.token.push_back(*_pos);
        break;
      case state_t::IN_FLOAT:
        switch(*_pos)
        {
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
        state = state_t::START;
        switch(*_pos)
        {
          case '0': case '1': case '2': case '3': case '4':
          case '5': case '6': case '7': case '8': case '9':
            state = state_t::IN_FLOAT;
            continue;
          case '(': case ')': case '[': case ']':
          case '\n': case '\t': case ' ':
            return token;
          default:
            token.type = token_t::type::SYMBOL;
            continue;
        }
        break;
      default:
        break;
    }
    ++_pos;
  }
  if(!token.token.empty())
    return token;
  return {};
}

template<typename Input>
void Reader<Input>::unread(token_t token)
{
  _token = token;
}

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

}

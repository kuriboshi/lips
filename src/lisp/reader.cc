//
// Lips, lisp shell.
// Copyright 2022 Krister Joas
//

#include "reader.hh"

namespace lisp
{

int Reader::next()
{
  auto curc = _file.getch();
  if(curc == EOF)
    _eof = true;
  return curc;
}

/// @brief Read the next token from the input string.
/// @return Returns either the token as a string or nothing when the input
///   string reaches the end.
std::optional<token_t> Reader::read()
{
  if(_token)
  {
    // A simple move from _token doesn't work because the moved from object
    // will still contain a value.
    auto t = _token;
    _token.reset();
    return t;
  }
  if(_eof)
    return {};
  token_t token;
  enum class state_t
  {
    NORMAL,
    IN_STRING,
    IN_QUOTE,
    IN_INT,
    IN_SYMBOL,
    IN_DOT
  };
  state_t state{state_t::NORMAL};
  while(!_eof)
  {
    switch(state)
    {
      case state_t::NORMAL:
        switch(_curc)
        {
          case '(':
          case ')':
          case '[':
          case ']':
            // These characters are terminating macro characters.
            if(!token.token.empty())
              return token;
            token.type = token_t::type::MACRO;
            token.token.push_back(_curc);
            _curc = next();
            return token;
          case '.':
            // A symbol may start with a dot so we assume it's a macro
            // character but look ahead one character.
            state = state_t::IN_DOT;
            token.type = token_t::type::MACRO;
            token.token.push_back(_curc);
            break;
          case ' ':
          case '\n':
          case '\t':
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
          case '0':
          case '1':
          case '2':
          case '3':
          case '4':
          case '5':
          case '6':
          case '7':
          case '8':
          case '9':
            if(!token.token.empty())
              return token;
            state = state_t::IN_INT;
            token.type = token_t::type::INT;
            token.token.push_back(_curc);
            break;
          default:
            state = state_t::IN_SYMBOL;
            token.token.push_back(_curc);
            break;
        }
        break;
      case state_t::IN_SYMBOL:
        // Any character except unquoted terminating macro characters are
        // included in the symbol. This includes double quotes.
        switch(_curc)
        {
          case '(':
          case ')':
          case '[':
          case ']':
          case ' ':
          case '\n':
          case '\t':
            state = state_t::NORMAL;
            return token;
            break;
          case '\\':
            _curc = next();
            if(_eof)
              return token;
            token.token.push_back(_curc);
            break;
          default:
            token.token.push_back(_curc);
            break;
        }
        break;
      case state_t::IN_QUOTE:
        token.token.push_back(_curc);
        state = state_t::IN_STRING;
        break;
      case state_t::IN_STRING:
        switch(_curc)
        {
          case '\\':
            state = state_t::IN_QUOTE;
            break;
          case '"':
            state = state_t::NORMAL;
            break;
          default:
            token.token.push_back(_curc);
            break;
        }
        break;
      case state_t::IN_INT:
        switch(_curc)
        {
          case '0':
          case '1':
          case '2':
          case '3':
          case '4':
          case '5':
          case '6':
          case '7':
          case '8':
          case '9':
            token.token.push_back(_curc);
            break;
          default:
            state = state_t::NORMAL;
            return token;
            break;
        }
        break;
      case state_t::IN_DOT:
        state = state_t::NORMAL;
        switch(_curc)
        {
          case '(':
          case ')':
          case '[':
          case ']':
          case '\n':
          case '\t':
          case ' ':
            return token;
            break;
          default:
            token.type = token_t::type::SYMBOL;
            continue;
            break;
        }
        break;
      default:
        break;
    }
    _curc = next();
  }
  if(!token.token.empty())
    return token;
  return {};
}

void Reader::unread(token_t token)
{
  _token = token;
}

}

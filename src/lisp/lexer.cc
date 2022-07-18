//
// Lips, lisp shell.
// Copyright 2022 Krister Joas
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

#include "lisp/lexer.hh"

namespace lisp
{
/// @brief Read the next token from the input stream.
/// @returns Returns either the token as a string or the empty token when the
///   input stream reaches the end.
// NOLINTNEXTLINE(readability-function-cognitive-complexity)
token_t lexer::read()
{
  if(_token)
    return std::move(_token);
  token_t token;
  state_t state{state_t::START};
  if(_start_of_line && *_pos == '#')
    state = state_t::IN_COMMENT;
  while(_pos != _input->source().end())
  {
    switch(state)
    {
      case state_t::START:
        switch(get(*_pos))
        {
          case syntax::type::SHELL_COMMENT:
            if(_start_of_line)
              state = state_t::IN_COMMENT;
            else
            {
              state = state_t::IN_HASH;
              token.type = token_t::type::SYMBOL;
              token.token.push_back(*_pos);
            }
            break;
          case syntax::type::COMMENT:
            state = state_t::IN_COMMENT;
            break;
          case syntax::type::SEPARATOR:
            break;
          case syntax::type::NEWLINE:
            _start_of_line = true;
            break;
          case syntax::type::LEFT_PAREN:
          case syntax::type::RIGHT_PAREN:
          case syntax::type::LEFT_BRACKET:
          case syntax::type::RIGHT_BRACKET:
          case syntax::type::QUOTE:
            token.type = token_t::type::SPECIAL;
            token.token.push_back(*_pos);
            next();
            return token;
          case syntax::type::DECIMAL_POINT:
            // A symbol may start with a dot so we assume it's a macro
            // character but look ahead one character.
            state = state_t::IN_DOT;
            token.type = token_t::type::SPECIAL;
            token.token.push_back(*_pos);
            break;
          case syntax::type::STRING_DELIM:
            state = state_t::IN_STRING;
            token.type = token_t::type::STRING;
            break;
          case syntax::type::SIGN:
            state = state_t::IN_SIGN;
            token.type = token_t::type::SYMBOL; // Assume symbol
            token.token.push_back(*_pos);
            break;
          case syntax::type::DIGIT:
            state = state_t::IN_INT;
            token.type = token_t::type::INT;
            token.token.push_back(*_pos);
            break;
          case syntax::type::MACRO:
            token.type = token_t::type::MACRO;
            token.token.push_back(*_pos);
            next();
            return token;
          case syntax::type::SPLICE:
            token.type = token_t::type::SPLICE;
            token.token.push_back(*_pos);
            next();
            return token;
          case syntax::type::INFIX:
            token.type = token_t::type::INFIX;
            token.token.push_back(*_pos);
            next();
            return token;
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
        switch(get(*_pos))
        {
          case syntax::type::COMMENT:
            return token;
          case syntax::type::LEFT_PAREN:
          case syntax::type::RIGHT_PAREN:
          case syntax::type::LEFT_BRACKET:
          case syntax::type::RIGHT_BRACKET:
          case syntax::type::SEPARATOR:
          case syntax::type::NEWLINE:
            return token;
          case syntax::type::ESCAPE:
            next();
            if(_pos == _input->source().end())
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
        switch(get(*_pos))
        {
          case syntax::type::ESCAPE:
            state = state_t::IN_QUOTE;
            break;
          case syntax::type::STRING_DELIM:
            next();
            return token;
          default:
            token.token.push_back(*_pos);
            break;
        }
        break;
      case state_t::IN_COMMENT:
        if(get(*_pos) == syntax::type::NEWLINE)
          state = state_t::START;
        break;
      case state_t::IN_HASH:
        switch(get(*_pos))
        {
          case syntax::type::LEFT_PAREN:
          case syntax::type::RIGHT_PAREN:
          case syntax::type::LEFT_BRACKET:
          case syntax::type::RIGHT_BRACKET:
          case syntax::type::SEPARATOR:
          case syntax::type::NEWLINE:
            return token;
          case syntax::type::QUOTE:
            token.token.push_back(*_pos);
            next();
            return token;
          default:
            token.token.push_back(*_pos);
            break;
        }
        break;
      case state_t::IN_SIGN:
        switch(get(*_pos))
        {
          case syntax::type::DIGIT:
            state = state_t::IN_INT;
            token.type = token_t::type::INT;
            token.token.push_back(*_pos);
            break;
          default:
            state = state_t::IN_SYMBOL;
            token.type = token_t::type::SYMBOL;
            continue;
        }
        break;
      case state_t::IN_INT:
        switch(get(*_pos))
        {
          case syntax::type::COMMENT:
            return token;
          case syntax::type::DECIMAL_POINT:
            state = state_t::IN_FLOAT;
            token.type = token_t::type::FLOAT;
            break;
          case syntax::type::EXPONENT:
            state = state_t::IN_EXP1;
            token.type = token_t::type::FLOAT;
            break;
          case syntax::type::DIGIT:
            break;
          case syntax::type::LEFT_PAREN:
          case syntax::type::RIGHT_PAREN:
          case syntax::type::LEFT_BRACKET:
          case syntax::type::RIGHT_BRACKET:
          case syntax::type::SEPARATOR:
          case syntax::type::NEWLINE:
            return token;
          default:
            token.type = token_t::type::SYMBOL;
            state = state_t::IN_SYMBOL;
            break;
        }
        token.token.push_back(*_pos);
        break;
      case state_t::IN_FLOAT:
        switch(get(*_pos))
        {
          case syntax::type::COMMENT:
            return token;
          case syntax::type::EXPONENT:
            state = state_t::IN_EXP1;
            break;
          case syntax::type::DIGIT:
            break;
          case syntax::type::LEFT_PAREN:
          case syntax::type::RIGHT_PAREN:
          case syntax::type::LEFT_BRACKET:
          case syntax::type::RIGHT_BRACKET:
          case syntax::type::SEPARATOR:
          case syntax::type::NEWLINE:
            return token;
          default:
            state = state_t::IN_SYMBOL;
            token.type = token_t::type::SYMBOL;
            break;
        }
        token.token.push_back(*_pos);
        break;
      case state_t::IN_EXP1:
        switch(get(*_pos))
        {
          case syntax::type::COMMENT:
            return token;
          case syntax::type::SIGN:
          case syntax::type::DIGIT:
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
        switch(get(*_pos))
        {
          case syntax::type::COMMENT:
            return token;
          case syntax::type::DIGIT:
            break;
          default:
            state = state_t::IN_SYMBOL;
            token.type = token_t::type::SYMBOL;
            break;
        }
        token.token.push_back(*_pos);
        break;
      case state_t::IN_DOT:
        switch(get(*_pos))
        {
          case syntax::type::COMMENT:
            return token;
          case syntax::type::DIGIT:
            state = state_t::IN_FLOAT;
            continue;
          case syntax::type::LEFT_PAREN:
          case syntax::type::RIGHT_PAREN:
          case syntax::type::LEFT_BRACKET:
          case syntax::type::RIGHT_BRACKET:
          case syntax::type::SEPARATOR:
          case syntax::type::NEWLINE:
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
    next();
  }
  return token;
}

void lexer::unread(token_t token) { _token = token; }
} // namespace lisp

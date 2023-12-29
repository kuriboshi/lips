//
// Lips, lisp shell.
// Copyright 1988, 2020-2023 Krister Joas
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

//
// This file contains all functions dealing with low level terminal and file
// i/o.  Terminal i/o uses its own buffering and line editing.  It sets the
// terminal in cbreak and no echo mode.
//

#include <unistd.h>
#include <term.h>
#include <poll.h>

#include <iostream>
#include <array>
#include <csignal>
#include <cstdlib>
#include <cstring>
#include <cctype>
#include <ranges>
#include <algorithm>

#include <lisp/lisp.hh>
#include "main.hh"
#include "top.hh"
#include "glob.hh"
#include "os.hh"
#include "term.hh"

using namespace lisp;

namespace
{
char* getstr(const char* id, char** area)
{
  // The function tgetstr has a bad signature.
  return tgetstr(const_cast<char*>(id), area); // NOLINT(cppcoreguidelines-pro-type-const-cast)
}
} // namespace

void term_source::init_keymap()
{
  key_tab.fill(function::T_INSERT);
  key_tab[CERASE] = function::T_ERASE;
  key_tab[CTRL('H')] = function::T_ERASE;
  key_tab[CRPRNT] = function::T_RETYPE;
  key_tab[CTRL('L')] = function::T_CLEAR;
  key_tab[CKILL] = function::T_KILL;
  key_tab[CEOF] = function::T_EOF;
  key_tab[CTRL('I')] = function::T_TAB;
  key_tab[static_cast<int>('(')] = function::T_LEFTPAR;
  key_tab[static_cast<int>(')')] = function::T_RIGHTPAR;
  key_tab[static_cast<int>('\n')] = function::T_NEWLINE;
  key_tab[static_cast<int>('\\')] = function::T_ESCAPE;
  key_tab[static_cast<int>('"')] = function::T_STRING;
}

void term_source::init_term()
{
  static bool initialized = false;

  if(!initialized)
  {
    tcgetattr(0, &_oldterm);
    _newterm = _oldterm;
    _newterm.c_lflag &= static_cast<unsigned>(~ECHO);
    _newterm.c_lflag &= static_cast<unsigned>(~ICANON);
    _newterm.c_lflag |= ISIG;
    _newterm.c_cc[VMIN] = 1;
    _newterm.c_cc[VTIME] = 0;
    std::array<char, 128> termcap{}; // Buffer for terminal capabilties.
    auto* termc = termcap.data();
    if(auto* term = getenv("TERM"); term != nullptr)
    {
      if(tgetent(nullptr, term) == 1)
      {
        _termcap.clear = getstr("cl", &termc);
        _termcap.cleol = getstr("ce", &termc);
        _termcap.curfwd = getstr("nd", &termc);
        _termcap.curup = getstr("up", &termc);
        _termcap.nocap =
          _termcap.clear.empty() || _termcap.cleol.empty() || _termcap.curfwd.empty() || _termcap.curup.empty();
      }
    }
    init_keymap();
    initialized = true;
  }
  tcsetattr(0, TCSANOW, &_newterm);
}

term_source::~term_source() { end_term(); }

void term_source::end_term() { tcsetattr(0, TCSANOW, &_oldterm); }

void term_source::pputc(char c, FILE* file)
{
  auto is_control = [](auto c) { return std::iscntrl(c) != 0 && c != '\n' && c != '\t'; };
  // if(std::iscntrl(c) != 0 && c != '\n' && c != '\t')
  if(is_control(c))
  {
    putc('^', file);
    putc(c + at_char, file);
  }
  else
    putc(c, file);
}

void term_source::putch(char c, FILE* file, bool esc)
{
  if((c == '(' || c == '"' || c == ')' || c == '\\') && esc)
    pputc('\\', file);
  pputc(c, file);
}

char term_source::getch()
{
  while(true)
  {
    if(_position < _linepos)
      return _linebuffer.at(_position++);
    auto line = getline();
    end_term();
    if(line)
      return EOF;
    end_term();
  }
}

void term_source::ungetch(char)
{
  if(_position > 0)
    --_position;
}

bool term_source::first_left_paren(const std::string& str)
{
  auto pos = std::ranges::find_if_not(str, [](auto c) { return std::isspace(c) != 0; });
  if(pos == std::end(str))
    return false;
  return *pos == '(';
}

void term_source::delonechar()
{
  --_linepos;
  putc('\b', stdout);
  putc(' ', stdout);
  putc('\b', stdout);
  if(is_control(_linebuffer.at(_linepos)))
  {
    putc('\b', stdout);
    putc(' ', stdout);
    putc('\b', stdout);
  }
}

bool term_source::onlyblanks()
{
  for(int i = _linepos; i > 0; --i)
  {
    if(std::isspace(_linebuffer.at(i)) == 0)
      return false;
  }
  return true;
}

int term_source::outc(int c)
{
  putc(c, stdout);
  return c;
}

// NOLINTNEXTLINE(readability-function-cognitive-complexity)
void term_source::retype(int all)
{
  int nl = 0;

  if(!_termcap.nocap)
  {
    int l = 0;
    for(int i = 0; i < _linepos; ++i)
    {
      if(_linebuffer.at(i) == '\n')
      {
        nl = i;
        ++l;
      }
    }
    for(l = (all != 0 ? l : 1); l != 0; --l)
    {
      if(all == 2)
      {
        putc('\r', stdout);
        nput(_termcap.cleol);
      }
      nput(_termcap.curup);
    }
    putc('\r', stdout);
    if(all != 0)
      nl = 0;
    if(nl == 0)
      std::cout << _current_prompt;
    if(all != 2)
    {
      for(int i = nl; i < _linepos; ++i)
      {
        if(_linebuffer.at(i) == '\n')
          nput(_termcap.cleol);
        else
          pputc(_linebuffer.at(i), stdout);
      }
    }
    nput(_termcap.cleol);
  }
  else
  {
    if(all == 0)
    {
      putc('\r', stdout);
      int i = _linepos;
      for(; i >= 0 && _linebuffer.at(i) != '\n'; --i)
        ;
      if(i == 0)
        std::cout << _current_prompt;
      for(++i; i < _linepos; ++i)
        pputc(_linebuffer.at(i), stdout);
    }
    else if(all == 1)
    {
      pputc(CRPRNT, stdout);
      pputc('\n', stdout);
      std::cout << _current_prompt;
      for(int i = 0; i < _linepos; ++i)
        pputc(_linebuffer.at(i), stdout);
    }
    else
    {
      pputc(CKILL, stdout);
      pputc('\n', stdout);
      std::cout << _current_prompt;
    }
  }
}

char* term_source::mkexstr()
{
  int i = _linepos;

  _last = _word.data() + BUFSIZ - 1;
  *_last-- = '\0';
  *_last-- = '*';
  while(std::isspace(_linebuffer.at(i - 1)) == 0 && i > 0)
    *_last-- = _linebuffer.at(--i);
  return ++_last;
}

void term_source::fillrest(const char* word)
{
  for(word += strlen(_last) - 1; *word != 0; ++word)
  {
    pputc(*word, stdout);
    _linebuffer.at(_linepos++) = *word;
  }
}

bool term_source::checkchar(lisp_t words, std::size_t pos, char* c)
{
  auto w = words;
  *c = (w->car()->getstr())[pos];
  for(; !is_nil(w); w = w->cdr())
  {
    if(*c != (w->car()->getstr())[pos])
      return false;
  }
  return true;
}

void term_source::complete(lisp_t words)
{
  char c = 1;

  auto pos = strlen(_last) - 1;
  while(c != '\0' && checkchar(words, pos++, &c))
  {
    pputc(c, stdout);
    _linebuffer.at(_linepos++) = c;
  }
}

lisp_t term_source::strip(lisp_t files, const char* prefix, const char* suffix)
{
  lisp_t stripped;

  if(strncmp(files->car()->getstr().c_str(), prefix, strlen(prefix) - 1) != 0)
    return files;
  for(stripped = cons(nil, nil); !is_nil(files); files = files->cdr())
  {
    const auto* s = files->car()->getstr().c_str() + strlen(prefix) - strlen(suffix);
    // s[0] = '~';
    tconc(stripped, mkstring(s));
  }
  return stripped->car();
}

// NOLINTNEXTLINE(readability-function-cognitive-complexity)
std::pair<term_source::cursor_position, term_source::cursor_position> term_source::scan(int begin)
{
  int line{0};
  int cpos{0};
  paren_blink state{paren_blink::NORMAL};
  int parcount{0};
  bool pars{false};
  cursor_position parpos{0, 0, nullptr};
  cursor_position currentpos{0, 0, nullptr};
  for(int pos{begin}; pos >= 0; --pos)
  {
    auto cur = _linebuffer.at(pos);
    ++cpos;
    int escape{0};
    if(cur == '"' && state == paren_blink::INSTRING)
      state = paren_blink::EXITSTRING;
    else if(cur == '"' && state == paren_blink::NORMAL)
      state = paren_blink::STARTSTRING;
    else if(cur == '(' && state != paren_blink::INSTRING && state != paren_blink::STARTSTRING)
      state = paren_blink::LEFTPAR;
    else if(cur == ')' && state != paren_blink::INSTRING && state != paren_blink::STARTSTRING)
      state = paren_blink::RIGHTPAR;
    else if(cur == '\n')
    {
      if(parpos.line == line)
      {
        parpos.cpos = cpos - parpos.cpos;
        parpos.line_start = &_linebuffer.at(pos);
      }
      if(currentpos.line_start == nullptr)
        currentpos.line_start = &_linebuffer.at(pos);
      cpos = 0;
      ++line;
    }
    while(_linebuffer.at(pos) == '\\')
    {
      ++escape;
      --pos;
      ++cpos;
    }
    if((escape % 2) == 1)
    {
      switch(state)
      {
        case paren_blink::EXITSTRING:
          state = paren_blink::INSTRING;
          break;
        case paren_blink::STARTSTRING:
          state = paren_blink::NORMAL;
          break;
        default:
          break;
      }
    }
    else
    {
      switch(state)
      {
        case paren_blink::EXITSTRING:
          state = paren_blink::NORMAL;
          break;
        case paren_blink::STARTSTRING:
          state = paren_blink::INSTRING;
          break;
        case paren_blink::LEFTPAR:
          state = paren_blink::NORMAL;
          --parcount;
          break;
        case paren_blink::RIGHTPAR:
          state = paren_blink::NORMAL;
          ++parcount;
          break;
        default:
          break;
      }
    }
    if(!pars && parcount == 0)
    {
      parpos.line_start = &_linebuffer.at(pos);
      parpos.cpos = cpos;
      parpos.line = line;
      pars = true;
    }
    if(line == 0)
      ++currentpos.cpos;
  }
  currentpos.line = line;
  if(line == 0)
  {
    currentpos.cpos += static_cast<int>(_current_prompt.length());
    currentpos.line_start = _linebuffer.data();
  }
  parpos.line = line - parpos.line;
  if(parpos.line == 0)
    parpos.cpos = cpos - parpos.cpos + static_cast<int>(_current_prompt.length());
  return {parpos, currentpos};
}

void term_source::nput(const std::string& str, int ntim)
{
  for(; ntim > 0; --ntim)
    tputs(str.c_str(), 1, outc);
}

void term_source::blink()
{
  if(_termcap.nocap)
    return; // Requires termcap and enough capability
  auto [parpos, currentpos] = scan(_linepos - 1);
  if(parpos.line_start == nullptr)
    return;

  const auto ldiff = currentpos.line - parpos.line;
  const auto cdiff = parpos.cpos - currentpos.cpos;
  nput(_termcap.curup, ldiff);
  if(cdiff < 0)
  {
    if(-cdiff < parpos.cpos)
      nput("\b", -cdiff);
    else
    {
      putc('\r', stdout);
      nput(_termcap.curfwd, parpos.cpos);
    }
  }
  else
    nput(_termcap.curfwd, cdiff);
  fflush(stdout);

  // Blink for 1s or until key pressed
  struct pollfd pfd = {1, POLLIN, 0};
  poll(&pfd, 1, blink_time);

  _linebuffer.at(_linepos) = '\0';
  if(ldiff == 0)
  {
    for(int i = 0; parpos.line_start[i] != 0; ++i)
      pputc(parpos.line_start[i], stdout);
  }
  else
  {
    if(currentpos.line == 0)
      std::cout << _current_prompt;
    for(int i = 0; currentpos.line_start[i] != 0; ++i)
      pputc(currentpos.line_start[i], stdout);
  }
  fflush(stdout);
}

void term_source::clearscr() { nput(_termcap.clear); }

// NOLINTNEXTLINE(readability-function-cognitive-complexity)
std::optional<std::string> term_source::getline()
{
  char c{0};
  bool instring{false};
  int escaped{0};
  int parcount{0};
  _linepos = 0;
  _position = 0;

  init_term();

  if(_options.command)
  {
    std::cerr << "Unbalanced parenthesis\n";
    end_term();
    ::exit(1);
  }
  const int origpar = parcount;
  while(true)
  {
    if(escaped != 0)
      --escaped;
    fflush(stdout);
    if(readchar(stdin, &c) == 0)
    {
      end_term();
      return {};
    }
    switch(key_tab.at(static_cast<unsigned char>(c)))
    {
      case function::T_EOF:
        if(_linepos == 0)
        {
          _linebuffer.at(_linepos++) = EOF;
          end_term();
          return {};
        }
        pputc(c, stdout);
        _linebuffer.at(_linepos++) = EOF;
        break;
      case function::T_KILL:
        retype(2);
        _linepos = 0;
        parcount = origpar;
        escaped = 0;
        instring = false;
        break;
      case function::T_RETYPE:
        retype(1);
        break;
      case function::T_CLEAR:
        clearscr();
        retype(1);
        break;
      case function::T_TAB:
      {
        auto* s = mkexstr();
        auto t = glob::extilde(s);
        if(!t)
        {
          putc(BELL, stdout);
          break;
        }
        auto ex = glob::expandfiles(*t, true);
        if(type_of(ex) == object::type::Cons && strlen(s) > 1)
          ex = strip(ex, t->c_str(), s);
        if(type_of(ex) == object::type::Cons && is_nil(ex->cdr()))
        {
          fillrest(ex->car()->getstr().c_str());
        }
        else
        {
          if(type_of(ex) == object::type::Cons)
            complete(ex);
          putc(BELL, stdout);
        }
        break;
      }
      case function::T_ERASE:
        escaped = 0;
        if(_linepos > 0 && _linebuffer.at(_linepos - 1) == '\n')
        {
          --_linepos;
          retype(0);
        }
        else if(_linepos > 0)
        {
          delonechar();
          if(_linebuffer.at(_linepos) == '\\')
            escaped = 2;
          else
          {
            if(!instring && _linebuffer.at(_linepos) == '(')
              --parcount;
            if(!instring && _linebuffer.at(_linepos) == ')')
              ++parcount;
            if(_linebuffer.at(_linepos) == '"')
              instring = !instring;
          }
        }
        break;
      case function::T_STRING:
        _linebuffer.at(_linepos++) = c;
        pputc(c, stdout);
        if(escaped == 0)
          instring = !instring;
        break;
      case function::T_ESCAPE:
        _linebuffer.at(_linepos++) = c;
        pputc(c, stdout);
        if(escaped == 0)
          escaped = 2;
        break;
      case function::T_LEFTPAR:
        if(!instring && escaped == 0)
          ++parcount;
        pputc(c, stdout);
        _linebuffer.at(_linepos++) = c;
        break;
      case function::T_RIGHTPAR:
        if((escaped != 0) || instring)
        {
          pputc(c, stdout);
          _linebuffer.at(_linepos++) = c;
          break;
        }
        --parcount;
        pputc(c, stdout);
        _linebuffer.at(_linepos++) = c;
        if(parcount <= 0)
        {
          _linebuffer.at(_linepos) = '\0';
          std::string ret{_linebuffer.data()};
          if(parcount < 0)
            ret.insert(0, 1, '(');
          if(first_left_paren(ret))
          {
            pputc('\n', stdout);
            end_term();
            return ret;
          }
        }
        blink();
        break;
      case function::T_NEWLINE:
        pputc('\n', stdout);
        if(_linepos == 0 || onlyblanks())
        {
          _linepos = 0;
          _linebuffer.at(_linepos++) = '(';
          _linebuffer.at(_linepos++) = ')';
        }
        _linebuffer.at(_linepos++) = '\n';
        if(parcount <= 0 && !instring)
        {
          end_term();
          _linebuffer.at(_linepos++) = '\0';
          return _linebuffer.data();
        }
        break;
      case function::T_INSERT:
        pputc(c, stdout);
        _linebuffer.at(_linepos++) = c;
        break;
    }
  }
}

lisp_t term_source::readline(std::string prompt)
{
  _current_prompt = prompt;
  std::cout << "\r";
  std::cout << _current_prompt;
  auto line = getline();
  if(line)
  {
    lexer lexer{*line};
    parser parser(lexer);
    auto head = parser.parse();
    if(listp(head))
      return cons(head, nil);
    if(head == nil || head == C_EOF)
      return head;
    lisp_t tail;
    while(true)
    {
      auto o = parser.parse();
      if(o == C_EOF)
        break;
      if(tail == nil)
        tail = cdr(head = cons(head, cons(o, nil)));
      else
        tail = cdr(rplacd(tail, cons(o, nil)));
    }
    if(tail == nil)
      return cons(head, nil);
    return head;
  }
  return C_EOF;
}

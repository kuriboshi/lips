//
// Lips, lisp shell.
// Copyright 1988, 2020-2024 Krister Joas
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
#include <cctype>
#include <csignal>
#include <cstdlib>
#include <cstring>
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

void termcap::init()
{
  std::array<char, 128> terminfo{}; // Buffer for terminal capabilties.
  if(auto* term = getenv("TERM"); term != nullptr)
  {
    if(tgetent(nullptr, term) == 1)
    {
      auto* termc = terminfo.data();
      auto getcap = [&termc](const char* capstr, const char* alt = nullptr) -> const char* {
        if(auto* cap = getstr(capstr, &termc); cap != nullptr)
          return cap;
        if(alt != nullptr)
          return alt;
        return "";
      };
      _bell = getcap("bl", "\a");
      _clear = getcap("cl");
      _cleol = getcap("ce");
      _cr = getcap("cr", "\r");
      _curfwd = getcap("nd");
      _curleft = getcap("le", "\b");
      _curup = getcap("up");
      _newline = getcap("nw", "\n");
      _nocap =
        std::ranges::any_of(std::vector<std::string>{_bell, _clear, _cleol, _cr, _curfwd, _curleft, _curup, _newline},
          [](const auto& arg) { return arg.empty(); });
    }
  }
}

void termcap::nput(const std::string& str, int n)
{
  for(; n > 0; --n)
    tputs(str.c_str(), 1, [](int c) -> int { std::cout << static_cast<char>(c); return c; });
}

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
    _termcap.init();
    init_keymap();
    initialized = true;
  }
  tcsetattr(0, TCSANOW, &_newterm);
}

term_source::~term_source() { end_term(); }

void term_source::end_term() { tcsetattr(0, TCSANOW, &_oldterm); }

void term_source::pputc(char c, std::ostream& file)
{
  auto is_control = [](auto c) { return std::iscntrl(c) != 0 && c != '\n' && c != '\t'; };
  // if(std::iscntrl(c) != 0 && c != '\n' && c != '\t')
  if(is_control(c))
    file << '^' << static_cast<char>(c + at_char);
  else
    file << c;
}

void term_source::putch(char c, std::ostream& file, bool esc)
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
  _termcap.cursor_left();
  std::cout << ' ';
  _termcap.cursor_left();
  if(is_control(_linebuffer.at(_linepos)))
  {
    _termcap.cursor_left();
    std::cout << ' ';
    _termcap.cursor_left();
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

void term_source::retype(int all)
{
  if(_termcap.nocap())
    return;
  int nl{0};
  int l{0};
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
      _termcap.carriage_return();
      _termcap.clr_eol();
    }
    _termcap.cursor_up();
  }
  _termcap.carriage_return();
  if(all != 0)
    nl = 0;
  if(nl == 0)
    std::cout << _current_prompt;
  if(all != 2)
  {
    for(int i = nl; i < _linepos; ++i)
    {
      if(_linebuffer.at(i) == '\n')
        _termcap.clr_eol();
      else
        pputc(_linebuffer.at(i), std::cout);
    }
  }
  _termcap.clr_eol();
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
    pputc(*word, std::cout);
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
    pputc(c, std::cout);
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

void term_source::blink()
{
  if(_termcap.nocap())
    return; // Requires termcap and enough capability
  auto [parpos, currentpos] = scan(_linepos - 1);
  if(parpos.line_start == nullptr)
    return;

  const auto ldiff = currentpos.line - parpos.line;
  const auto cdiff = parpos.cpos - currentpos.cpos;
  _termcap.cursor_up(ldiff);
  if(cdiff < 0)
  {
    if(-cdiff < parpos.cpos)
      _termcap.cursor_left(-cdiff);
    else
    {
      _termcap.carriage_return();
      _termcap.cursor_right(parpos.cpos);
    }
  }
  else
    _termcap.cursor_right(cdiff);
  std::cout.flush();

  // Blink for 1s or until key pressed
  struct pollfd pfd = {1, POLLIN, 0};
  poll(&pfd, 1, blink_time);

  _linebuffer.at(_linepos) = '\0';
  if(ldiff == 0)
  {
    for(int i = 0; parpos.line_start[i] != 0; ++i)
      pputc(parpos.line_start[i], std::cout);
  }
  else
  {
    if(currentpos.line == 0)
      std::cout << _current_prompt;
    for(int i = 0; currentpos.line_start[i] != 0; ++i)
      pputc(currentpos.line_start[i], std::cout);
  }
  std::cout.flush();
}

void term_source::clearscr() { _termcap.clear_screen(); }

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
    std::cout.flush();
    if(!readchar(&c))
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
        pputc(c, std::cout);
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
          _termcap.bell();
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
          _termcap.bell();
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
        pputc(c, std::cout);
        if(escaped == 0)
          instring = !instring;
        break;
      case function::T_ESCAPE:
        _linebuffer.at(_linepos++) = c;
        pputc(c, std::cout);
        if(escaped == 0)
          escaped = 2;
        break;
      case function::T_LEFTPAR:
        if(!instring && escaped == 0)
          ++parcount;
        pputc(c, std::cout);
        _linebuffer.at(_linepos++) = c;
        break;
      case function::T_RIGHTPAR:
        if((escaped != 0) || instring)
        {
          pputc(c, std::cout);
          _linebuffer.at(_linepos++) = c;
          break;
        }
        --parcount;
        pputc(c, std::cout);
        _linebuffer.at(_linepos++) = c;
        if(parcount <= 0)
        {
          _linebuffer.at(_linepos) = '\0';
          std::string ret{_linebuffer.data()};
          if(parcount < 0)
            ret.insert(0, 1, '(');
          if(first_left_paren(ret))
          {
            _termcap.newline();
            end_term();
            return ret;
          }
        }
        blink();
        break;
      case function::T_NEWLINE:
        _termcap.newline();
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
        pputc(c, std::cout);
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

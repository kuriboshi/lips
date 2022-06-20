//
// Lips, lisp shell.
// Copyright 1988, 2020-2022 Krister Joas
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

#include <lisp/lisp.hh>
#include "main.hh"
#include "top.hh"
#include "glob.hh"
#include "os.hh"
#include "term.hh"

using namespace lisp;

void term_source::clearlbuf()
{
  linepos = 0;
  parcount = 0;
}

/*
 * Set up keymap.
 */
void term_source::init_keymap()
{
  for(int i = NUM_KEYS - 1; i != 0; i--) key_tab[i] = term_fun::T_INSERT;
  key_tab[CERASE] = term_fun::T_ERASE;
  key_tab[CTRL('H')] = term_fun::T_ERASE;
  key_tab[CRPRNT] = term_fun::T_RETYPE;
  key_tab[CTRL('L')] = term_fun::T_CLEAR;
  key_tab[CKILL] = term_fun::T_KILL;
  key_tab[CEOF] = term_fun::T_EOF;
  key_tab[CTRL('I')] = term_fun::T_TAB;
  key_tab[static_cast<int>('(')] = term_fun::T_LEFTPAR;
  key_tab[static_cast<int>(')')] = term_fun::T_RIGHTPAR;
  key_tab[static_cast<int>('\n')] = term_fun::T_NEWLINE;
  key_tab[static_cast<int>('\\')] = term_fun::T_ESCAPE;
  key_tab[static_cast<int>('"')] = term_fun::T_STRING;
}

struct termios term_source::oldterm;

/* Init terminal to CBREAK and no ECHO.  */
void term_source::init_term()
{
  static bool initialized = false;

  if(!initialized)
  {
    tcgetattr(0, &oldterm);
    newterm = oldterm;
    newterm.c_lflag &= (unsigned)~ECHO;
    newterm.c_lflag &= (unsigned)~ICANON;
    newterm.c_lflag |= ISIG;
    newterm.c_cc[VMIN] = 1;
    newterm.c_cc[VTIME] = 0;
    curup = nullptr;
    curfwd = nullptr;
    char* termc = static_cast<char*>(tcap);
    if(auto* term = getenv("TERM"); term != nullptr)
    {
      std::array<char, 1024> bp{};
      if(tgetent(bp.data(), term) == 1)
      {
        clear = tgetstr(const_cast<char*>("cl"), &termc); // NOLINT
        curup = tgetstr(const_cast<char*>("up"), &termc); // NOLINT
        curdn = "\n";
        curfwd = tgetstr(const_cast<char*>("nd"), &termc); // NOLINT
        cleol = tgetstr(const_cast<char*>("ce"), &termc); // NOLINT
        nocap = (curup == nullptr || curdn == nullptr || curfwd == nullptr || cleol == nullptr);
      }
    }
    init_keymap();
    initialized = true;
  }
  tcsetattr(0, TCSANOW, &newterm);
}

term_source::~term_source() { end_term(); }

/* Reset terminal to previous value */
void term_source::end_term() { tcsetattr(0, TCSANOW, &oldterm); }

/*
 * Put a character on stdout prefixing it with a ^ if it's
 * a control character.
 */
void term_source::pputc(int c, FILE* file)
{
  if(c < 0x20 && c != '\n' && c != '\t')
  {
    putc('^', file);
    putc(c + 0x40, file);
  }
  else
    putc(c, file);
}

/*
 * Put a character c, on stream file, escaping enabled if esc != 0.
 */
void term_source::putch(int c, FILE* file, bool esc)
{
  if((c == '(' || c == '"' || c == ')' || c == '\\') && esc)
    pputc('\\', file);
  pputc(c, file);
}

/*
 * Get a character.  Buffer input with procedure getline, and get characters
 * from linebuffer.
 */
int term_source::getch()
{
  while(true)
  {
    if(position < linepos)
      return linebuffer[position++];
    auto line = getline();
    end_term();
    if(line)
      return EOF;
    end_term();
  }
}

/*
 * Unget a character.  If reading from a terminal, just push it back in the
 * buffer, if not, do an ungetc.
 */
void term_source::ungetch(int)
{
  if(position > 0)
    --position;
}

/*
 * Skips separators in the beginning of the line and returns true if the first
 * non-separator character is a left parenthesis, zero otherwise.
 */
bool term_source::firstnotlp()
{
  int i = 0;
  for(; i < position && std::isspace(linebuffer[i]) != 0; i++)
    ;
  return linebuffer[i] != '(';
}

/*
 * Delete one character the easy way by sending backspace - space - backspace.
 * Do it twice if it was a control character.
 */
void term_source::delonechar()
{
  --linepos;
  putc('\b', stdout);
  putc(' ', stdout);
  putc('\b', stdout);
  if(linebuffer[linepos] < 0x20)
  {
    putc('\b', stdout);
    putc(' ', stdout);
    putc('\b', stdout);
  }
}

/*
 * Returns zero if the line contains only separators.
 */
bool term_source::onlyblanks()
{
  for(int i = linepos; i > 0; --i)
  {
    if(std::isspace(linebuffer[i]) == 0)
      return false;
  }
  return true;
}

/*
 * Output a character on stdout, used only in tputs.
 */
int term_source::outc(int c)
{
  putc(c, stdout);
  return c;
}

/* 
 * retype - If ALL is 0 then retype only current line.  If ALL is 1 then retype
 *          complete line, including prompt.  It ALL is 2 just delete all
 *          lines.  Used for ctrl-u kill.
 */
// NOLINTNEXTLINE(readability-function-cognitive-complexity)
void term_source::retype(int all)
{
  int nl = 0;

  if(!nocap)
  {
    int l = 0;
    for(int i = 0; i < linepos; i++)
    {
      if(linebuffer[i] == '\n')
      {
        nl = i;
        ++l;
      }
    }
    for(l = (all != 0 ? l : 1); l != 0; l--)
    {
      if(all == 2)
      {
        putc('\r', stdout);
        tputs(cleol, 1, outc);
      }
      tputs(curup, 1, outc);
    }
    putc('\r', stdout);
    if(all != 0)
      nl = 0;
    if(nl == 0)
      std::cout << current_prompt;
    if(all != 2)
    {
      for(int i = nl; i < linepos; i++)
      {
        if(linebuffer[i] == '\n')
          tputs(cleol, 1, outc);
        else
          pputc(linebuffer[i], stdout);
      }
    }
    tputs(cleol, 1, outc);
  }
  else
  {
    if(all == 0)
    {
      putc('\r', stdout);
      int i = linepos;
      for(; i >= 0 && linebuffer[i] != '\n'; --i)
        ;
      if(i == 0)
        std::cout << current_prompt;
      for(++i; i < linepos; i++) pputc(linebuffer[i], stdout);
    }
    else if(all == 1)
    {
      pputc(CRPRNT, stdout);
      pputc('\n', stdout);
      std::cout << current_prompt;
      for(int i = 0; i < linepos; ++i) pputc(linebuffer[i], stdout);
    }
    else
    {
      pputc(CKILL, stdout);
      pputc('\n', stdout);
      std::cout << current_prompt;
    }
  }
}

/*
 * Stuff for file name completion.
 */
static std::array<char, BUFSIZ> word{};
static char* last;

char* term_source::mkexstr()
{
  int i = linepos;

  last = word.data() + BUFSIZ - 1;
  *last-- = '\0';
  *last-- = '*';
  while(std::isspace(linebuffer[i - 1]) == 0 && i > 0) *last-- = linebuffer[--i];
  return ++last;
}

void term_source::fillrest(const char* word)
{
  for(word += strlen(last) - 1; *word != 0; word++)
  {
    pputc(*word, stdout);
    linebuffer[linepos++] = *word;
  }
}

bool term_source::checkchar(LISPT words, std::size_t pos, char* c)
{
  LISPT w = words;
  *c = (w->car()->getstr())[pos];
  for(; !is_NIL(w); w = w->cdr())
  {
    if(*c != (w->car()->getstr())[pos])
      return false;
  }
  return true;
}

void term_source::complete(LISPT words)
{
  char c = 1;

  auto pos = strlen(last) - 1;
  while(c != '\0' && checkchar(words, pos++, &c))
  {
    pputc(c, stdout);
    linebuffer[linepos++] = c;
  }
}

LISPT term_source::strip(LISPT files, const char* prefix, const char* suffix)
{
  LISPT stripped;

  if(strncmp(files->car()->getstr().c_str(), prefix, strlen(prefix) - 1) != 0)
    return files;
  for(stripped = cons(NIL, NIL); !is_NIL(files); files = files->cdr())
  {
    const auto* s = files->car()->getstr().c_str() + strlen(prefix) - strlen(suffix);
    // s[0] = '~';
    tconc(stripped, mkstring(s));
  }
  return stripped->car();
}

/*
 * Scans backwards and try to find a matching left parenthesis skipping strings
 * and escapes.  It records its finding in parpos.  It also updates where the
 * cursor is now in currentpos, so it can find its way back.  BEGIN is the
 * position in linebuffer from where to start searching.
 */
// NOLINTNEXTLINE(readability-function-cognitive-complexity)
void term_source::scan(int begin)
{
  int line = 0;
  int cpos = 0;
  paren_blink state = paren_blink::NORMAL;
  int parcount = 0;
  bool pars = false;
  parpos = {0, 0, nullptr};
  currentpos = {0, 0, nullptr};
  for(int pos = begin; pos >= 0; --pos)
  {
    int cur = linebuffer[pos];
    ++cpos;
    int escape = 0;
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
        parpos.line_start = &linebuffer[pos];
      }
      if(currentpos.line_start == nullptr)
        currentpos.line_start = &linebuffer[pos];
      cpos = 0;
      line++;
    }
    while(linebuffer[pos] == '\\')
    {
      escape++;
      pos--;
      cpos++;
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
          parcount--;
          break;
        case paren_blink::RIGHTPAR:
          state = paren_blink::NORMAL;
          parcount++;
          break;
        default:
          break;
      }
    }
    if(!pars && parcount == 0)
    {
      parpos.line_start = &linebuffer[pos];
      parpos.cpos = cpos;
      parpos.line = line;
      pars = true;
    }
    if(line == 0)
      currentpos.cpos++;
  }
  currentpos.line = line;
  if(line == 0)
  {
    currentpos.cpos += static_cast<int>(current_prompt.length());
    currentpos.line_start = &linebuffer[0];
  }
  parpos.line = line - parpos.line;
  if(parpos.line == 0)
    parpos.cpos = cpos - parpos.cpos + current_prompt.length(); // NOLINT
}

/*
 * Puts the string STR on stdout NTIM times using tputs.
 */
void term_source::nput(const char* str, int ntim)
{
  for(; ntim > 0; ntim--)
  {
    tputs(str, 1, outc);
  }
}

/*
 * Blink matching paren.
 */
void term_source::blink()
{
  if(nocap)
    return; // Requires termcap and enough capability
  scan(linepos - 1);

  int ldiff = currentpos.line - parpos.line;
  int cdiff = parpos.cpos - currentpos.cpos;
  nput(curup, ldiff);
  if(cdiff < 0)
  {
    if(-cdiff < parpos.cpos)
      nput("\b", -cdiff);
    else
    {
      putc('\r', stdout);
      nput(curfwd, parpos.cpos); /* This is really silly.  */
    }
  }
  else
    nput(curfwd, cdiff);
  fflush(stdout);

  // Blink for 1s or until key pressed
  struct pollfd pfd = {1, POLLIN, 0};
  poll(&pfd, 1, 1000);

  linebuffer[linepos] = '\0';
  if(ldiff == 0)
  {
    for(int i = 0; parpos.line_start[i]; i++) pputc(parpos.line_start[i], stdout); // NOLINT
  }
  else
  {
    if(currentpos.line == 0)
      std::cout << current_prompt;
    for(int i = 0; currentpos.line_start[i]; i++) pputc(currentpos.line_start[i], stdout); // NOLINT
  }
  fflush(stdout);
}

void term_source::clearscr() {
#ifdef HAVE_CURSES
  tputs(clear, 1, outc);
#endif
}

//
// Get a line from stdin.  Do line editing functions such as kill line, retype
// line and delete character.  Count parethesis pairs and terminate line if
// matching right paren.  Typing just a return puts a right paren in the buffer
// as well as the newline.  Returns empty optional on EOF.
//
// NOLINTNEXTLINE(readability-function-cognitive-complexity)
std::optional<std::string> term_source::getline()
{
  char c = 0;
  bool instring = false;
  int escaped = 0;

  init_term();

  if(options.command)
  {
    std::cerr << "Unbalanced parenthesis\n";
    end_term();
    exit(1);
  }
  position = 0;
  linepos = 0;
  int origpar = parcount;
  while(true)
  {
    if(escaped != 0)
      escaped--;
    fflush(stdout);
    if(readchar(stdin, &c) == 0)
    {
      end_term();
      return {};
    }
    switch(key_tab[static_cast<int>(c)])
    {
      case term_fun::T_EOF:
        if(linepos == 0)
        {
          linebuffer[linepos++] = EOF;
          end_term();
          return {};
        }
        pputc(c, stdout);
        linebuffer[linepos++] = EOF;
        break;
      case term_fun::T_KILL:
        retype(2);
        linepos = 0;
        parcount = origpar;
        escaped = 0;
        instring = false;
        break;
      case term_fun::T_RETYPE:
        retype(1);
        break;
      case term_fun::T_CLEAR:
        clearscr();
        retype(1);
        break;
      case term_fun::T_TAB:
      {
        auto* s = mkexstr();
        auto t = glob::extilde(s);
        if(!t)
        {
          putc(BELL, stdout);
          break;
        }
        auto ex = glob::expandfiles(*t, true);
        if(type_of(ex) == type::CONS && strlen(s) > 1)
          ex = strip(ex, t->c_str(), s);
        if(type_of(ex) == type::CONS && is_NIL(ex->cdr()))
        {
          fillrest(ex->car()->getstr().c_str());
        }
        else
        {
          if(type_of(ex) == type::CONS)
            complete(ex);
          putc(BELL, stdout);
        }
        break;
      }
      case term_fun::T_ERASE:
        escaped = 0;
        if(linepos > 0 && linebuffer[linepos - 1] == '\n')
        {
          --linepos;
          retype(0);
        }
        else if(linepos > 0)
        {
          delonechar();
          if(linebuffer[linepos] == '\\')
            escaped = 2;
          else
          {
            if(!instring && linebuffer[linepos] == '(')
              parcount--;
            if(!instring && linebuffer[linepos] == ')')
              parcount++;
            if(linebuffer[linepos] == '"')
              instring = !instring;
          }
        }
        break;
      case term_fun::T_STRING:
        linebuffer[linepos++] = c;
        pputc(c, stdout);
        if(escaped == 0)
          instring = !instring;
        break;
      case term_fun::T_ESCAPE:
        linebuffer[linepos++] = c;
        pputc(c, stdout);
        if(escaped == 0)
          escaped = 2;
        break;
      case term_fun::T_LEFTPAR:
        if(!instring && escaped == 0)
          parcount++;
        pputc(c, stdout);
        linebuffer[linepos++] = c;
        break;
      case term_fun::T_RIGHTPAR:
        if((escaped != 0) || instring)
        {
          pputc(c, stdout);
          linebuffer[linepos++] = c;
          break;
        }
        parcount--;
        pputc(c, stdout);
        linebuffer[linepos++] = c;
        if(parcount <= 0)
        {
          if(parcount < 0)
          {
            linebuffer[0] = '(';
            parcount = 0; /* in case it was negative */
          }
          else if(firstnotlp())
            break; /* paren expression not first (for readline) */
          linebuffer[linepos++] = '\n';
          pputc('\n', stdout);
          end_term();
          linebuffer[linepos++] = '\0';
          return linebuffer;
        }
        blink();
        break;
      case term_fun::T_NEWLINE:
        pputc('\n', stdout);
        if(linepos == 0 || onlyblanks())
        {
          linepos = 0;
          linebuffer[linepos++] = '(';
          linebuffer[linepos++] = ')';
        }
        linebuffer[linepos++] = '\n';
        if(parcount <= 0 && !instring)
        {
          end_term();
          linebuffer[linepos++] = '\0';
          return linebuffer;
        }
        break;
      case term_fun::T_INSERT:
        pputc(c, stdout);
        linebuffer[linepos++] = c;
        break;
    }
  }
}

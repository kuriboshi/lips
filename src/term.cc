/*
 * Lips, lisp shell.
 * Copyright 1988, 2020 Krister Joas
 *
 */

/*
 * This file contains all functions dealing with low level terminal and file
 * i/o.  Terminal i/o uses its own buffering and line editing.  It sets the
 * terminal in cbreak and no echo mode.
 */

#include <unistd.h>
#ifdef TERMCAP
#include <term.h>
#endif

#include <csignal>
#include <cstdlib>
#include <cstring>
#include <csetjmp>

#include <libisp.hh>
#include "top.hh"
#include "main.hh"
#include "glob.hh"
#include "os.hh"
#include "term.hh"

extern lisp::lisp* L;

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
  int i;

  for(i = NUM_KEYS - 1; i; i--) key_tab[i] = term_fun::T_INSERT;
  key_tab[CERASE] = term_fun::T_ERASE;
  key_tab[CTRL('H')] = term_fun::T_ERASE;
  key_tab[CRPRNT] = term_fun::T_RETYPE;
  key_tab[CTRL('L')] = term_fun::T_CLEAR;
  key_tab[CKILL] = term_fun::T_KILL;
  key_tab[CEOF] = term_fun::T_EOF;
  key_tab[CTRL('I')] = term_fun::T_TAB;
  key_tab[(int)'('] = term_fun::T_LEFTPAR;
  key_tab[(int)')'] = term_fun::T_RIGHTPAR;
  key_tab[(int)'\n'] = term_fun::T_NEWLINE;
  key_tab[(int)'\\'] = term_fun::T_ESCAPE;
  key_tab[(int)'"'] = term_fun::T_STRING;
}

term_source::term_source() {}

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
#ifdef TERMCAP
    curup = nullptr;
    curfwd = nullptr;
    char* termc = tcap;
    if(auto* term = getenv("TERM"); term != nullptr)
    {
      char bp[1024];
      if(tgetent(bp, term) == 1)
      {
        clear = tgetstr(const_cast<char*>("cl"), &termc);
        curup = tgetstr(const_cast<char*>("up"), &termc);
        curdn = "\n";
        curfwd = tgetstr(const_cast<char*>("nd"), &termc);
        cleol = tgetstr(const_cast<char*>("ce"), &termc);
        if(!curup || !curdn || !curfwd || !cleol)
          nocap = true;
        else
          nocap = false;
      }
    }
#endif
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
void term_source::putch(int c, FILE* file, int esc)
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
    else
    {
      init_term();
      if(getline() == nullptr)
      {
        end_term();
        return EOF;
      }
      end_term();
    }
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
  for(; i < position && issepr(*L, (int)linebuffer[i]); i++)
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
    if(!issepr(*L, (int)linebuffer[i]))
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
void term_source::retype(int all)
{
#ifdef TERMCAP
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
    for(l = (all ? l : 1); l; l--)
    {
      if(all == 2)
      {
        putc('\r', stdout);
        tputs(cleol, 1, outc);
      }
      tputs(curup, 1, outc);
    }
    putc('\r', stdout);
    if(all)
      nl = 0;
    if(nl == 0)
      fputs(current_prompt, stdout);
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
#endif
  {
    if(all == 0)
    {
      putc('\r', stdout);
      int i = linepos;
      for(; i >= 0 && linebuffer[i] != '\n'; --i)
        ;
      if(i == 0)
        fputs(current_prompt, stdout);
      for(++i; i < linepos; i++) pputc(linebuffer[i], stdout);
    }
    else if(all == 1)
    {
      pputc(CRPRNT, stdout);
      pputc('\n', stdout);
      fputs(current_prompt, stdout);
      for(int i = 0; i < linepos; ++i) pputc(linebuffer[i], stdout);
    }
    else
    {
      pputc(CKILL, stdout);
      pputc('\n', stdout);
      fputs(current_prompt, stdout);
    }
  }
}

/*
 * Stuff for file name completion.
 */
static char word[BUFSIZ];
static char* last;

char* term_source::mkexstr()
{
  int i = linepos;

  last = word + BUFSIZ - 1;
  *last-- = '\0';
  *last-- = '*';
  while(!issepr(*L, (int)linebuffer[i - 1]) && i > 0) *last-- = linebuffer[--i];
  return ++last;
}

void term_source::fillrest(const char* word)
{
  for(word += strlen(last) - 1; *word; word++)
  {
    pputc(*word, stdout);
    linebuffer[linepos++] = *word;
  }
}

using LISPT = lisp::LISPT;

bool term_source::checkchar(LISPT words, int pos, int* c)
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
  int pos;
  int c = 1;

  pos = strlen(last) - 1;
  while(c != '\0' && checkchar(words, pos++, &c))
  {
    pputc(c, stdout);
    linebuffer[linepos++] = c;
  }
}

LISPT term_source::strip(LISPT files, const char* prefix, const char* suffix)
{
  LISPT stripped;
  const char* s;

  if(strncmp(files->car()->getstr(), prefix, strlen(prefix) - 1) != 0)
    return files;
  for(stripped = cons(*L, lisp::C_NIL, lisp::C_NIL); !is_NIL(files); files = files->cdr())
  {
    s = files->car()->getstr() + strlen(prefix) - strlen(suffix);
    // s[0] = '~';
    tconc(*L, stripped, mkstring(*L, s));
  }
  return stripped->car();
}

/*
 * Scans backwards and try to find a matching left parenthesis skipping strings
 * and escapes.  It records its finding in parpos.  It also updates where the
 * cursor is now in currentpos, so it can find its way back.  BEGIN is the
 * position in linebuffer from where to start searching.
 */
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
    currentpos.cpos += strlen(current_prompt);
    currentpos.line_start = linebuffer;
  }
  parpos.line = line - parpos.line;
  if(parpos.line == 0)
    parpos.cpos = cpos - parpos.cpos + strlen(current_prompt);
}

/*
 * Puts the string STR on stdout NTIM times using tputs.
 */
void term_source::nput(const char* str, int ntim)
{
#ifdef TERMCAP
  for(; ntim > 0; ntim--) tputs(str, 1, outc);
#endif
}

/*
 * Blink matching paren.
 */
void term_source::blink()
{
  if(nocap)
    return; // Requires termcap and enough capability
  scan(linepos - 1);
#ifdef TERMCAP
  int i;

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
  struct timeval timeout;
  timeout.tv_sec = 1L;
  timeout.tv_usec = 0L;
  fd_set rfds;
  FD_SET(1, &rfds);
  select(1, &rfds, nullptr, nullptr, &timeout);
  linebuffer[linepos] = '\0';
  if(ldiff == 0)
  {
    for(i = 0; parpos.line_start[i]; i++) pputc(parpos.line_start[i], stdout);
  }
  else
  {
    if(currentpos.line == 0)
      fputs(current_prompt, stdout);
    for(i = 0; currentpos.line_start[i]; i++) pputc(currentpos.line_start[i], stdout);
  }
  fflush(stdout);
#endif
}

void term_source::clearscr() { tputs(clear, 1, outc); }

/*
 * Get a line from stdin.  Do line editing functions such as kill line, retype
 * line and delete character.  Count parethesis pairs and terminate line if
 * matching right paren.  Typing just a return puts a right paren in the buffer
 * as well as the newline.  Returns zero if anything goes wrong.
 */
const char* term_source::getline()
{
  char c;
  const char *s, *t;
  int origpar;
  int instring = 0;
  int escaped = 0;
  LISPT ex;

  if(options.command)
  {
    fprintf(stderr, "Unbalanced parenthesis\n");
    end_term();
    exit(1);
  }
  position = 0;
  linepos = 0;
  origpar = parcount;
  while(1)
  {
    if(escaped)
      escaped--;
    fflush(stdout);
    if(lisp::readchar(stdin, &c) == 0)
      return nullptr;
    switch(key_tab[(int)c])
    {
      case term_fun::T_EOF:
        if(linepos == 0)
        {
          linebuffer[linepos++] = EOF;
          return nullptr;
        }
        pputc(c, stdout);
        linebuffer[linepos++] = EOF;
        break;
      case term_fun::T_KILL:
        retype(2);
        linepos = 0;
        parcount = origpar;
        escaped = 0;
        instring = 0;
        break;
      case term_fun::T_RETYPE:
        retype(1);
        break;
      case term_fun::T_CLEAR:
        clearscr();
        retype(1);
        break;
      case term_fun::T_TAB:
        s = mkexstr();
        t = extilde(s, 0);
        if(t == nullptr)
        {
          putc(BELL, stdout);
          break;
        }
        ex = expandfiles(t, 0, 0, 1);
        if(type_of(ex) == lisp::CONS && strlen(s) > 1)
          ex = strip(ex, t, s);
        if(type_of(ex) == lisp::CONS && is_NIL(ex->cdr()))
          fillrest(ex->car()->getstr());
        else
        {
          if(type_of(ex) == lisp::CONS)
            complete(ex);
          putc(BELL, stdout);
        }
        break;
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
        if(!escaped)
          instring = !instring;
        break;
      case term_fun::T_ESCAPE:
        linebuffer[linepos++] = c;
        pputc(c, stdout);
        if(!escaped)
          escaped = 2;
        break;
      case term_fun::T_LEFTPAR:
        if(!instring && !escaped)
          parcount++;
        pputc(c, stdout);
        linebuffer[linepos++] = c;
        break;
      case term_fun::T_RIGHTPAR:
        if(escaped || instring)
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
          return linebuffer;
        }
        else
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
          return linebuffer;
        break;
      case term_fun::T_INSERT:
        pputc(c, stdout);
        linebuffer[linepos++] = c;
        break;
    }
  }
}

/*
 * Return true if currently at end of line, or at end of line after skipping
 * blanks.
 */
bool term_source::eoln()
{
  for(int i = position; i < linepos; ++i)
  {
    if(linebuffer[i] != ' ' && linebuffer[i] != '\t' && linebuffer[i] != '\n')
      return false;
    if(linebuffer[i] == '\n')
      return true;
  }
  return true;
}

struct termios term_source::oldterm;

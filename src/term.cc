/*
 * Lips, lisp shell.
 * Copyright 1988, 2020 Krister Joas
 *
 * $Id$
 */
/*
 * This file contains all functions dealing with low level terminal
 * and file i/o.  Terminal i/o uses its own buffering and line editing.
 * It sets the terminal in cbreak and no echo mode.
 */
#include <signal.h>
#include <unistd.h>
#include <stdlib.h>
#include <termios.h>
#include <string.h>
#ifdef TERMCAP
#include <term.h>
#endif

#include <libisp.hh>
#include "top.hh"
#include "main.hh"
#include "glob.hh"

#define FFLUSH(file) fflush(file)

extern void finish(int);
extern int readchar(FILE*, char*);

#define NUM_KEYS 256
#define COMMENTCHAR '#'
#ifndef BELL
#define BELL '\007'
#endif

/*
 * Terminal functions.  Each constant stands for a function provided
 * by the line editor.
 */
enum term_fun
{
  T_INSERT = 0,
  T_ERASE,
  T_RETYPE,
  T_KILL,
  T_EOF,
  T_TAB,
  T_LEFTPAR,
  T_RIGHTPAR,
  T_NEWLINE,
  T_STRING,
  T_ESCAPE
};

/*
 * Variables for terminal characteristics, old and new.
 */
static struct termios newterm, oldterm;

static char linebuffer[BUFSIZ];         /* Line buffer for terminal input.  */
static int parcount = 0;                /* Counts paranthesis.  */
static int linepos = 0;                 /* End of line buffer.  */
static int position = 0;                /* Current position in line buffer.  */
static enum term_fun key_tab[NUM_KEYS]; /* Table specifying key functions.  */

#ifdef TERMCAP
static char tcap[128];             /* Buffer for terminal capabilties.  */
static const char *curup, *curfwd; /* Various term cap strings.  */
static const char *cleol, *curdn;
static int nocap = 0; /* Nonzero if insufficient term cap. */
#endif

int lips_getline(FILE*);

void cleanup(int)
{
  finish(0);
}

void clearlbuf()
{
  linepos = 0;
  parcount = 0;
}

void loadbuf(char* str)
{
  strcpy(linebuffer, str);
  strcat(linebuffer, "\n");
  linepos = strlen(linebuffer);
}

/*
 * Set up keymap.
 */
void init_keymap()
{
  int i;

  for(i = NUM_KEYS - 1; i; i--) key_tab[i] = T_INSERT;
  key_tab[CERASE] = T_ERASE;
  key_tab[CTRL('h')] = T_ERASE;
  key_tab[CRPRNT] = T_RETYPE;
  key_tab[CKILL] = T_KILL;
  key_tab[CEOF] = T_EOF;
  key_tab[CTRL('i')] = T_TAB;
  key_tab[(int)'('] = T_LEFTPAR;
  key_tab[(int)')'] = T_RIGHTPAR;
  key_tab[(int)'\n'] = T_NEWLINE;
  key_tab[(int)'\\'] = T_ESCAPE;
  key_tab[(int)'"'] = T_STRING;
}

/* Init terminal to CBREAK and no ECHO.  */
void init_term()
{
  static int initialized = 0;
#ifdef TERMCAP
  char bp[1024];
  char* termc = tcap;
  char* term;
#endif

  if(!initialized)
  {
    tcgetattr(0, &oldterm);
    signal(SIGINT, cleanup);  /* temporary handle */
    signal(SIGTERM, cleanup); /* exit gracefully */
    newterm = oldterm;
    newterm.c_lflag &= (unsigned)~ECHO;
    newterm.c_lflag &= (unsigned)~ICANON;
    newterm.c_lflag |= ISIG;
    newterm.c_cc[VMIN] = 1;
    newterm.c_cc[VTIME] = 0;
#ifdef TERMCAP
    curup = NULL;
    curfwd = NULL;
    if((term = getenv("TERM")) != NULL)
      if(tgetent(bp, term) == 1)
      {
        curup = tgetstr(const_cast<char*>("up"), &termc);
        curdn = "\n";
        curfwd = tgetstr(const_cast<char*>("nd"), &termc);
        cleol = tgetstr(const_cast<char*>("ce"), &termc);
        if(!curup || !curfwd || !cleol)
          nocap = 1;
        else
          nocap = 0;
      }
#endif
    init_keymap();
    initialized = 1;
  }
  tcsetattr(0, TCSANOW, &newterm);
}

/* Reset terminal to previous value */
void end_term()
{
  tcsetattr(0, TCSANOW, &oldterm);
}

/*
 * Put a character on stdout prefixing it with a ^ if it's
 * a control character.
 */
void pputc(int c, FILE* file)
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
void putch(int c, FILE* file, int esc)
{
  if((c == '(' || c == '"' || c == ')' || c == '\\') && esc)
    pputc('\\', file);
  pputc(c, file);
}

/*
 * Get a character from stream file.  If it's from a
 * terminal, buffer input with procedure getline, and get
 * characters from linebuffer.  If it's not from a terminal
 * do io the standard way.
 */
int getch(FILE* file)
{
  int c;

  if(!isatty(fileno(file)))
  {
    c = getc(file);
    if(c == COMMENTCHAR) /* Skip comments.  */
      while((c = getc(file)) != '\n')
        ;
    return c;
  }
gotlin:
  if(position < linepos)
    return linebuffer[position++];
  else
  {
    init_term();
    if(lips_getline(file) == 0)
    {
      end_term();
      return EOF;
    }
    end_term();
    goto gotlin;
  }
}

/*
 * Unget a character.  If reading from a terminal, 
 * just push it back in the buffer, if not, do an ungetc.
 */
void ungetch(int c, FILE* file)
{
  if(isatty(fileno(file)))
  {
    if(position > 0)
      position--;
  }
  else
    ungetc(c, file);
}

/*
 * Skips separators in the beginning of the line and returns 1 if
 * the first non-separator character is a left parenthesis, zero
 * otherwise.
 */
static int firstnotlp()
{
  int i;

  for(i = 1; i < position && issepr((int)linebuffer[i]); i++)
    ;
  if(linebuffer[i] == '(')
    return 0;
  else
    return 1;
}

/*
 * Delete one character the easy way by sending backspace - space -
 * backspace.  Do it twice if it was a control character.
 */
static void delonechar()
{
  linepos--;
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
static int onlyblanks()
{
  int i = linepos;

  while(i > 0)
  {
    if(!issepr((int)linebuffer[i]))
      return 0;
    i--;
  }
  return 1;
}

/*
 * Output a character on stdout, used only in tputs.
 */
int outc(int c)
{
  putc(c, stdout);
  return c;
}

/* 
 * retype - If ALL is 0 then retype only current line.  If ALL is 1 then 
 *          retype complete line, including prompt.  It ALL is 2 just 
 *          delete all lines.  Used for ctrl-u kill.
 */
static void retype(int all)
{
  int i;
#ifdef TERMCAP
  int nl = 0, l = 1;

  if(!nocap)
  {
    l = 0;
    for(i = 1; i < linepos; i++)
      if(linebuffer[i] == '\n')
        nl = i, l++;
    for(l = all ? l : 1; l; l--)
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
      for(i = nl + 1; i < linepos; i++)
      {
        if(linebuffer[i] == '\n')
          tputs(cleol, 1, outc);
        pputc(linebuffer[i], stdout);
      }
    tputs(cleol, 1, outc);
  }
  else
#endif
  {
    if(all == 0)
    {
      putc('\r', stdout);
      for(i = linepos - 1; i > 0 && linebuffer[i] != '\n'; i--)
        ;
      if(i == 0)
        fputs(current_prompt, stdout);
      for(i++; i < linepos; i++) pputc(linebuffer[i], stdout);
    }
    else if(all == 1)
    {
      pputc(CRPRNT, stdout);
      pputc('\n', stdout);
      fputs(current_prompt, stdout);
      for(i = 1; i < linepos; i++) pputc(linebuffer[i], stdout);
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

char* mkexstr()
{
  int i = linepos - 1;

  last = word + BUFSIZ - 1;
  *last-- = '\0';
  *last-- = '*';
  while(!issepr((int)linebuffer[i]) && i >= 0) *last-- = linebuffer[i--];
  return ++last;
}

static void fillrest(const char* word)
{
  for(word += strlen(last) - 1; *word; word++)
  {
    pputc(*word, stdout);
    linebuffer[linepos++] = *word;
  }
}

static int checkchar(LISPT words, int pos, int* c)
{
  LISPT l;

  l = words;
  *c = (GETSTR(CAR(l)))[pos];
  for(; !ISNIL(l); l = CDR(l))
  {
    if(*c != (GETSTR(CAR(l)))[pos])
      return 0;
  }
  return 1;
}

static void complete(LISPT words)
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

static LISPT strip(LISPT files, const char* prefix, const char* suffix)
{
  LISPT stripped;
  const char* s;

  if(strncmp(GETSTR(CAR(files)), prefix, strlen(prefix) - 1) != 0)
    return files;
  for(stripped = cons(C_NIL, C_NIL); !ISNIL(files); files = CDR(files))
  {
    s = GETSTR(CAR(files)) + strlen(prefix) - strlen(suffix);
    // s[0] = '~';
    tconc(stripped, mkstring(s));
  }
  return CAR(stripped);
}

/*
 * Routines for paren blinking.
 */
#define NORMAL 0
#define INSTRING 1
#define EXITSTRING 2
#define STARTSTRING 3
#define LEFTPAR 4
#define RIGHTPAR 5

struct curpos
{
  int cpos;
  int line;
  char* line_start;
};

static struct curpos parpos;     /* Saves position of matching par.  */
static struct curpos currentpos; /* Current position.  */

/*
 * Scans backwards and tries to find a matching left parenthesis
 * skipping strings and escapes.  It records its finding in parpos.
 * It also updates where the cursor is now in currentpos, so it
 * can find its way back.  BEGIN is the position in linebuffer from
 * where to start searching.
 */
static void scan(int begin)
{
  int line, cpos;
  int pos;
  int escape;
  int state;
  int parcount, pars;

  line = 0;
  cpos = 0;
  state = NORMAL;
  parcount = 0;
  pars = 0;
  parpos.cpos = 0;
  parpos.line = 0;
  currentpos.cpos = 0;
  currentpos.line = 0;
  currentpos.line_start = NULL;
  for(pos = begin; pos > 0; pos--)
  {
    int cur = linebuffer[pos];
    cpos++;
    escape = 0;
    if(cur == '"' && state == INSTRING)
      state = EXITSTRING;
    else if(cur == '"' && state == NORMAL)
      state = STARTSTRING;
    else if(cur == '(' && state != INSTRING && state != STARTSTRING)
      state = LEFTPAR;
    else if(cur == ')' && state != INSTRING && state != STARTSTRING)
      state = RIGHTPAR;
    else if(cur == '\n')
    {
      if(parpos.line == line)
      {
        parpos.cpos = cpos - parpos.cpos - 1;
        parpos.line_start = &linebuffer[pos + 1];
      }
      if(currentpos.line_start == NULL)
        currentpos.line_start = &linebuffer[pos + 1];
      cpos = 0;
      line++;
    }
    while(linebuffer[pos - 1] == '\\')
    {
      escape++;
      pos--;
      cpos++;
    }
    if((escape % 2) == 1)
    {
      switch(state)
      {
        case EXITSTRING:
          state = INSTRING;
          break;
        case STARTSTRING:
          state = NORMAL;
          break;
        default:
          break;
      }
    }
    else
    {
      switch(state)
      {
        case EXITSTRING:
          state = NORMAL;
          break;
        case STARTSTRING:
          state = INSTRING;
          break;
        case LEFTPAR:
          state = NORMAL;
          parcount--;
          break;
        case RIGHTPAR:
          state = NORMAL;
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
      pars = 1;
    }
    if(line == 0)
      currentpos.cpos++;
  }
  currentpos.line = line;
  if(line == 0)
  {
    currentpos.cpos += strlen(current_prompt);
    currentpos.line_start = linebuffer + 1;
  }
  parpos.line = line - parpos.line;
  if(parpos.line == 0)
    parpos.cpos = cpos - parpos.cpos + strlen(current_prompt);
}

/*
 * Puts the string STR on stdout NTIM times using tputs.
 */
void nput(const char* str, int ntim)
{
#ifdef TERMCAP
  for(; ntim > 0; ntim--) tputs(str, 1, outc);
#endif
}

/*
 * Blink matching paren.
 */
void blink()
{
#ifdef TERMCAP
  int ldiff;
  int cdiff;
  struct timeval timeout;
  fd_set rfds;
  int i;

  if(nocap)
    return; /* Sorry, no blink.  */
  ldiff = currentpos.line - parpos.line;
  cdiff = parpos.cpos - currentpos.cpos;
  nput(curup, ldiff);
  if(cdiff < 0)
  {
    if(-cdiff < parpos.cpos)
      nput("\b", -cdiff);
    else
    {
      putc('\r', stdout);
      nput(curfwd, parpos.cpos); /* This is realy silly.  */
    }
  }
  else
    nput(curfwd, cdiff);
  fflush(stdout);
  timeout.tv_sec = 1L;
  timeout.tv_usec = 0L;
  FD_SET(1, &rfds);
  select(1, &rfds, NULL, NULL, &timeout);
  nput(curdn, ldiff); /* Goes to beginning of line.  */
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

/*
 * Get a line from stdin.  Do line editing functions such as kill line, 
 * retype line and delete character.  Count parethesis pairs and
 * terminate line if matching right paren.  Typing just a return
 * puts a right paren in the buffer as well as the newline.
 * Returns zero if anything goes wrong.
 */
int lips_getline(FILE* file)
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
  linepos = 1;
  origpar = parcount;
  linebuffer[0] = ' ';
  while(1)
  {
    if(escaped)
      escaped--;
    FFLUSH(stdout);
    if(readchar(file, &c) == 0)
      return 0;
    switch(key_tab[(int)c])
    {
      case T_EOF:
        if(linepos == 1)
        {
          linebuffer[linepos++] = EOF;
          return 1;
        }
        pputc(c, stdout);
        linebuffer[linepos++] = EOF;
        break;
      case T_KILL:
        retype(2);
        linepos = 1;
        parcount = origpar;
        escaped = 0;
        instring = 0;
        break;
      case T_RETYPE:
        retype(1);
        break;
      case T_TAB:
        s = mkexstr();
        t = extilde(s, 0);
        if(t == NULL)
        {
          putc(BELL, stdout);
          break;
        }
        ex = expandfiles(t, 0, 0, 1);
        if(TYPEOF(ex) == CONS && strlen(s) > 1)
          ex = strip(ex, t, s);
        if(TYPEOF(ex) == CONS && ISNIL(CDR(ex)))
          fillrest(GETSTR(CAR(ex)));
        else
        {
          if(TYPEOF(ex) == CONS)
            complete(ex);
          putc(BELL, stdout);
        }
        break;
      case T_ERASE:
        escaped = 0;
        if(linepos > 1 && linebuffer[linepos - 1] == '\n')
        {
          linepos--;
          retype(0);
        }
        else if(linepos > 1)
        {
          delonechar();
          if(linebuffer[linepos - 1] == '\\')
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
      case T_STRING:
        linebuffer[linepos++] = c;
        pputc(c, stdout);
        if(!escaped)
          instring = !instring;
        break;
      case T_ESCAPE:
        linebuffer[linepos++] = c;
        pputc(c, stdout);
        if(!escaped)
          escaped = 2;
        break;
      case T_LEFTPAR:
        if(!instring && !escaped)
          parcount++;
        pputc(c, stdout);
        linebuffer[linepos++] = c;
        break;
      case T_RIGHTPAR:
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
          return 1;
        }
        else
        {
          scan(linepos - 1);
          blink();
        }
        break;
      case T_NEWLINE:
        pputc('\n', stdout);
        if(linepos == 1 || onlyblanks())
        {
          linebuffer[0] = '(';
          linebuffer[linepos++] = ')';
        }
        linebuffer[linepos++] = '\n';
        if(parcount <= 0 && !instring)
          return 1;
        break;
      case T_INSERT:
        pputc(c, stdout);
        linebuffer[linepos++] = c;
        break;
    }
  }
}

/*
 * Return 1 if currently at end of line, or
 * at end of line after skipping blanks.
 */
int eoln(FILE* file)
{
  int i;

  if(!isatty(fileno(file)))
    return 0;
  for(i = position; i < linepos; i++)
  {
    if(linebuffer[i] != ' ' && linebuffer[i] != '\t' && linebuffer[i] != '\n')
      return 0;
    if(linebuffer[i] == '\n')
      return 1;
  }
  return 1;
}

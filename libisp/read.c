/*
 * Lips, lisp shell.
 * Copyright 1988, 2020 Krister Joas
 *
 * $Id$
 */
#include <ctype.h>
#include <stdlib.h>
#include "lisp.h"

#define NUL '\0'
#define MAXATOMSIZE 128         /* max length of atom read can handle */

#define PUSHR(w)     rstack = cons(w, rstack)
#define POPR(w)      w = CAR(rstack); \
                     rstack = CDR(rstack)

#define CHECKEOF(c)  if ((c) == EOF) \
                     { \
                       if (line || ISNIL(CAR(top))) \
                         return C_EOF; \
                       else \
                         return error(UNEXPECTED_EOF, C_NIL); \
                     }

#define GETCH(file)  do \
                       curc = getch(file); \
                     while (curc != EOF && issepr(curc)); \
                     CHECKEOF(curc);

#ifndef lint
static char rcsid[] = "$Id$";
#endif

extern int getch();
extern void ungetch();
extern int eoln();
extern void putch();

extern LISPT history, currentbase;

extern LISPT histget();

static LISPT rmsquote();
static LISPT rmredir();
static LISPT rmpipe();
/* static LISPT rmbg(); */
static LISPT rmdquote();
static LISPT rmexcl();
LISPT prin0();
static LISPT rmuser();         /* Installed for user read macros. */

LISPT top;               /* used for threading the input structure */
LISPT rstack;            /* partially built structure read stack */
long printlevel = 0;     /* maximum print level */
long thisplevel;         /* during print, print level */
int echoline;            /* is 1 if ! has been used */
struct rtinfo currentrt = 
{
  {
/* NUL SOH STX ETX EOT ENQ ACK BEL */
    0, 0, 0, 0, 0, 0, 0, 0,
/* BS  HT  NL  VT  NP  CR  SO  SI  */
    0, SEPR, SEPR, 0, 0, 0, 0, 0,
/* DLE DC1 DC2 DC3 DC4 NAK SYN ETB */
    0, 0, 0, 0, 0, 0, 0, 0,
/* CAN EM  SUB ESC FS  GS  RS  US  */
    0, 0, 0, 0, 0, 0, 0, 0,
/* SP  !   "   #   $   %   &   '   */
    SEPR, SPLICE, INSERT, 0, 0, 0, BRK, INSERT,
/* (   )   *   +   ,   -   .   /   */
    BRK, BRK, 0, 0, 0, 0, 0, 0,
/* 0   1   2   3   4   5   6   7   */
    0, 0, 0, 0, 0, 0, 0, 0,
/* 8   9   :   ;   <   =   >   ?   */
    0, 0, 0, 0, 0, 0, 0, 0,
/* @   A   B   C   D   E   F   G   */
    0, 0, 0, 0, 0, 0, 0, 0,
/* H   I   J   K   L   M   N   O   */
    0, 0, 0, 0, 0, 0, 0, 0,
/* P   Q   R   S   T   U   V   W   */
    0, 0, 0, 0, 0, 0, 0, 0,
/* X   Y   Z   [   \   ]   ^   _   */
    0, 0, 0, 0, 0, 0, 0, 0,
/* `   a   b   c   d   e   f   g   */
    0, 0, 0, 0, 0, 0, 0, 0,
/* h   i   j   k   l   m   n   o   */
    0, 0, 0, 0, 0, 0, 0, 0,
/* p   q   r   s   t   u   v   w   */
    0, 0, 0, 0, 0, 0, 0, 0,
/* x   y   z   {   |   }   ~   DEL */
    0, 0, 0, 0, 0, 0, 0, 0 },
  { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, rmexcl, rmdquote, 0, 0, 0, 0, rmsquote, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }
};

static LISPT userreadmacros[128];
static char buf[MAXATOMSIZE];
/*
 * This state table parses a floating point number.
 */
static int nxtstate[4][10] = {
  { 1,-1,-1,-1,-1,-1,-1, 8,-1,-1}, 
  { 2, 2, 2, 4, 4, 6, 6, 9, 9, 9}, 
  {-1,-1, 7, 7, 7,-1, 7,-1,-1,-1}, 
  { 5, 5, 3,-1,-1,-1,-1,-1,-1,-1}
};
static char digits[] = {
  '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
  'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j',
  'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't',
  'u', 'v', 'w', 'x', 'y', 'z'};

/*
 * INTEGERP returns nonzero if the characters in buffer BUF
 * represents an integer, and the result as a long in res.
 */
static int integerp(buf, res)
  char *buf;
  long *res;
{
  int d = 0, sign = 1;
  
  *res = 0;
  if (*buf == '-')
    sign = *buf++ == '-' ? -1 : 1;
  if (!*buf) d = 1;
  for (; *buf; buf++)
    {
      if (!isdigit(*buf)) d++;
      else *res = *res * 10 + *buf - '0';
    }
  *res *= sign;
  return !d;
}

/*
 * Returns nonzero if buffer BUF is a floating point constant.
 */
static int floatp(buf)
  char *buf;
{
#ifdef FLOATING
  int state;

  state = 0;
  while (state >= 0 && *buf)
    {
      switch (*buf)
        {
        case '+':
        case '-':
          state = nxtstate[0][state];
          break;
        case '0': case '1': case '2': case '3': case '4':
        case '5': case '6': case '7': case '8': case '9':
          state = nxtstate[1][state];
          break;
        case 'e':
        case 'E':
          state = nxtstate[2][state];
          break;
        case '.':
          state = nxtstate[3][state];
          break;
        default:
          state = -1;
        }
      buf++;
    }
  if (state == 3 || state == 4 || state == 6 || state == 9)
    return 1;
  else
    return 0;
#else
  return 0;
#endif
}

/*
 * Find out if the buffer can be interpreted as numbers of
 * any kind.
 */
static LISPT parsebuf(buf)
  char *buf;
{
  long longval;

  if (integerp(buf, &longval))
    return mknumber(longval);
#ifdef FLOATING
  else if (floatp(buf))
    return mkfloat(atof(buf));
#endif /* FLOATING */
  else
    return mkatom(buf);
}

/*
 * Read an atom from FILE.
 */
LISPT
ratom(file)
  FILE *file;
{
  int c;
  int pos = 0;
  
  c = getch(file);
  while (1)
    {
      if (c == EOF)
        return C_EOF;
      else if (issepr(c)) ;
      else if (isbrk(c))
        {
          buf[pos++] = c;
          buf[pos] = NUL;
          return mkatom(buf);
        }
      else
        {
          while (1)
            {
              if (c == EOF)
                return parsebuf(buf);
              if (c == '\\')
                c = getch(file);
              if (pos < MAXATOMSIZE)
                buf[pos++] = c;
              c = getch(file);
              if (isbrk(c))
                {
                  ungetch(c, file);
                  buf[pos] = NUL;
                  return parsebuf(buf);
                }
              else if (issepr(c))
                {
                  buf[pos] = NUL;
                  return parsebuf(buf);
                }
            }
        }
      c = getch(file);
    }
}
  
/*
 * Splice list l into c keeping cdr of c. If l is not a
 * list put it in car of c and return c, otherwise return
 * last cell of l with cdr set to original (cdr c).
 * If tailp is true, don't clobber car of c.
 */
static LISPT
splice(c, l, tailp)
  LISPT c, l;
  int tailp;
{
  LISPT t, t2;

  t = CDR(c);
  if (TYPEOF(l) != CONS)
    {
      if (tailp) (void) rplacd(c, cons(l, t));
      else (void) rplaca(c, l);
      return c;
    }
  if (!tailp)
    {
      (void) rplaca(c, CAR(l));
      l = CDR(l);
    }
  if (ISNIL(l)) return c;
  (void) rplacd(c, l);
  for (; TYPEOF(l) == CONS; l = CDR(l)) t2 = l;
  return rplacd(t2, t);
}

/*
 * LISPREAD reads a lisp expression from file FILE. If LINE
 * is nonzero then it is assumed that it was called from READLINE.
 * READLINE initializes by itself TOP so that an extra level of
 * parentheses is in effect. An explicit stack is used to store
 * TOP when LISPREAD recurses.
 */
/*
 * If you don't like goto's, keep your eyes shut.
 */
LISPT
lispread(file, line)
  FILE *file;
  int line;
{
  LISPT curr, temp, curatom;
  int curc;
  
  if (!line)
    {
      top = cons(C_NIL, C_NIL);
      curr = top;
    }
  else curr = CAR(top);
head:
  GETCH(file);
  if (isinsert(curc))
    {
      PUSHR(top);
      (void) rplaca(curr, (*currentrt.rmacros[curc])(file));
      POPR(top);
      goto check;
    }
  else if (issplice(curc))
    {
      PUSHR(top);
      temp = (*currentrt.rmacros[curc])(file);
      POPR(top);
      curr = splice(curr, temp, 0);
      goto check;
    }
  else if (curc == '(')
    {
head2:
      (void) rplaca(curr, cons(C_NIL, C_NIL));
      (void) rplacd(CAR(curr), curr);
      curr = CAR(curr);
      goto head;
    }
  else if (curc == ')')
    {
      curr = CDR(curr);
      (void) rplaca(curr, C_NIL);
      goto check;
    }
  else
    {
      ungetch(curc, file);
      curatom = ratom(file);
      (void) rplaca(curr, curatom);
check:
      if (ISNIL(CDR(curr)))
        {
          temp = CAR(top);
          top = C_NIL;
          return temp;
        }
      else if (line && eoln(file) && EQ (CDR(curr), top)) goto addparen;
      else goto tail;
    }
tail:  
  if (line && eoln(file) && EQ (CDR(curr), top)) goto addparen;
  GETCH(file);
  if (isinsert(curc))
    {
      temp = CDR(curr);
      (void) rplacd(curr, cons(C_NIL, temp));
      curr = CDR(curr);
      PUSHR(top);
      (void) rplaca(curr, (*currentrt.rmacros[curc])(file));
      POPR(top);
      goto tail;
    }
  else if (issplice(curc))
    {
      PUSHR(top);
      temp = (*currentrt.rmacros[curc])(file);
      POPR(top);
      curr = splice(curr, temp, 1);
      goto tail;
    }
  else if (isinfix(curc))
    {
      curr = (*currentrt.rmacros[curc])(file, curr, curc);
      goto head;
    }
  else if (curc == ')')
    {
addparen:
      temp = CDR(curr);
      (void) rplacd(curr, C_NIL);
      curr = temp;
      goto check;
    }
  else if (curc == '(')
    {
      temp = CDR(curr);
      (void) rplacd(curr, cons(C_NIL, C_NIL));
      curr = CDR(curr);
      (void) rplacd(curr, temp);
      goto head2;
    }
  else if (curc == '.')
    {
      curc = getch(file);
      CHECKEOF(curc);
      if (!issepr(curc) && !isbrk(curc))
        {
          ungetch(curc, file);
          ungetch('.', file);   /* cross your fingers */
          goto atom;
        }
      if (curc == ')' || eoln(file))
        {
          ungetch(curc, file);
          curatom = C_DOT;
          goto insert;
        }
      if (isbrk(curc))
        ungetch(curc, file);
      curatom = ratom(file);
      temp = CDR(curr);
      GETCH(file);
      if (curc != ')')
        {
          (void) rplacd(curr, cons(C_DOT, cons(C_NIL, temp)));
          curr = CDR(CDR(curr));
          (void) rplaca(curr, curatom);
          goto another;
        }
      (void) rplacd(curr, curatom);
      curr = temp;
      goto check;
    }
  else
    {
another:
      ungetch(curc, file);
atom:
      curatom = ratom(file);
insert:
      temp = CDR(curr);
      (void) rplacd(curr, cons(C_NIL, temp));
      curr = CDR(curr);
      (void) rplaca(curr, curatom);
      goto tail;
    }
}

/*
 * Read macros.
 */
/*
 * Redo read macro:
 *   !!      - last command
 *   !-n     - the n'th previous command
 *   !n      - command n
 *   !s      - command with prefix s
 *   !$      - last argument
 *   !*      - all arguments
 * others could be added easily.
 */
/*ARGSUSED*/
static LISPT rmexcl(file)
  FILE *file;
{
  int c;
  LISPT tmp, at, l;

  c = getch(file);
  if (issepr(c)) return C_EXCL;
  echoline = 1;
  tmp = histget(0L, history);
  if (TYPEOF(CAR(tmp)) == CONS && ISNIL(CDR(tmp)))
    tmp = CAR(tmp);
  switch (c)
    {
    case '!':
      return histget(0L, history);
      break;
    case '$':
      while (TYPEOF(CDR(tmp)) == CONS)
        tmp = CDR(tmp);
      return tmp;
      break;
    case '*':
      return CDR(tmp);
      break;
    case '\n':
      echoline = 0;
      return C_EXCL;
      break;
    default:
      ungetch(c, file);
      at = ratom(file);
      if (TYPEOF(at) == INTEGER)
        {
          tmp = histget(INTVAL(at), history);
          return tmp;
        }
      if (TYPEOF(at) == SYMBOL)
        {
          for (l = history; !ISNIL(l); l = CDR(l))
            {
              tmp = histget(0L, l);
              if (!ISNIL(tmp) && TYPEOF(CAR(tmp)) == CONS
                  && ISNIL(CDR(tmp)))
                tmp = CAR(tmp);
              if (!strncmp(GETSTR(CAR(tmp)), GETSTR(at),
                           strlen(GETSTR(at))))
                return histget(0L, l);
            }
          return C_NIL;
        }
      else
        {
          (void) error(EVENT_NOT_FOUND, at);
          return C_NIL;
        }
    }
  return C_NIL;
}

/*
 * Handles user macros.
 */
/*ARGSUSED*/
static LISPT rmuser(file, curr, curc)
  FILE *file;
  LISPT curr;
  char curc;
{
  if (ISNIL(userreadmacros[curc])) return curr;
  else
    curr = apply(userreadmacros[curc], curr);
  return curr;
}

/*ARGSUSED*/
static LISPT rmdquote(file)
  FILE *file;
{
  char buf[MAXATOMSIZE];
  char c;
  int pos = 0;

  c = getch(file);
  while (c != '"' && pos < MAXATOMSIZE)
    {
      if (c == '\\')
        c = getch(file);
      buf[pos++] = c;
      c = getch(file);
    }
  buf[pos] = NUL;
  return mkstring(buf);
}  

#ifdef COMMENT
/*ARGSUSED*/
static LISPT rmbg(file, curr, curc)
  FILE *file;
  LISPT curr;
  char curc;
{
  (void) rplaca(CDR(curr), cons(C_BACK, CAR(CDR(curr))));
  (void) rplacd(curr, cons(C_NIL, CDR(curr)));
  return CDR(curr);
}
#endif

static LISPT rmsquote(file)
  FILE *file;
{
  int c;

  if ((c = getch(file)) == ')' || issepr(c))
    {
      ungetch(c, file);
      return C_QUOTE;
    }
  ungetch(c, file);
  return cons(C_QUOTE, cons(lispread(file, 0), C_NIL));
}

/*ARGSUSED*/
static LISPT rmpipe(file, curr, curc)
  FILE *file;
  LISPT curr;
  char curc;
{
  LISPT t1, t2;

  t1 = CDR(curr);
  (void) rplaca(t1, cons(C_PIPE, 
                         cons(CAR(CDR(curr)), 
                              cons(t2 = cons(C_NIL, t1), C_NIL))));
  (void) rplacd(curr, C_NIL);
  return t2;
}

/*ARGSUSED*/
static LISPT rmredir(file, curr, curc)
  FILE *file;
  LISPT curr;
  char curc;
{
  LISPT t1, t2;
  char c;
  
  t1 = CDR(curr);
  c = getch(file);
  (void) rplaca(t1, cons((curc == '<') ? C_FROM :
                         ((c == '>') ? C_TOTO : C_TO),
                         cons(CAR(CDR(curr)),
                              t2 = cons(C_NIL, t1))));
  if (!(c == '>' || curc == '>')) ungetch(c, file);
  (void) rplacd(curr, C_NIL);
  return t2;
}

LISPT
readline(file)
  FILE *file;
{
  register LISPT rd;

  top = cons(C_NIL, C_NIL);     /* Init first paren level */
  (void) rplaca(top, cons(C_NIL, top));
  rd = lispread(file, 1);
  return rd;
}

/* print the string s, on stream file */
static void ps(s, file, esc)
  char *s;
  FILE *file;
  int esc;
{
  while (*s)
    putch(*s++, file, esc);
}

static void pi(i, base, file)
  long i;
  long base;
  FILE *file;
{
  char ss[33];
  int sign;
  int j = 31;
  
  ss[32] = 0;
  sign = (i < 0) ? -1 : 1;
  i = sign * i;
  if (!i) ps("0", file, 0);
  else
    {
      while (i)
        {
          ss[j--] = digits[i % base];
          i /= base;
        }
    }
  if (sign == -1) ss[j--] = '-';
  ps(ss+j+1, file, 0);
}

static void pf(d, file)
  double d;
  FILE *file;
{
  char ss[30];

#ifdef SARGASSO
  (void) sprintf(ss, "%g", d);
#else
  (void) sprintf(ss, "%#g", d);
#endif
  ps(ss, file, 0);
}

LISPT
patom(x, file, esc)
  LISPT x;
  FILE *file;
  int esc;
{
  ps(SYMVAL(x).pname, file, esc);
  return x;
}

LISPT
terpri(file)
  FILE *file;
{
  putch('\n', file, 0);
  return C_NIL;
}

LISPT
prinbody(x, file, esc)
  LISPT x;
  FILE *file;
  int esc;
{
  LISPT xx;

  xx = x;
nxtelt:
  (void) prin0(CAR(xx), file, esc);
  if (EQ(CDR(xx), C_NIL)) ;
  else if (TYPEOF(CDR(xx)) == CONS)
    {
      putch(' ', file, 0);
      xx = CDR(xx);
      goto nxtelt;
    }
  else
    {
      putch(' ', file, 0);
      putch('.', file, 0);
      putch(' ', file, 0);
      (void) prin0(CDR(xx), file, esc);
    }
  return x;
}

LISPT
prin0(x, file, esc)
  LISPT x;
  FILE *file;
  int esc;
{
  switch (TYPEOF(x))
    {
    case CONS:
      thisplevel++;
      if (thisplevel <= printlevel || printlevel <= 0)
        {
          putch('(', file, 0);
          (void) prinbody(x, file, esc);
          putch(')', file, 0);
        }
      else
        putch('&', file, 0);
      thisplevel--;
      break;
    case SYMBOL:
      return patom(x, file, esc);
      break;
    case CPOINTER:
      if (CPOINTVAL (x) != NULL)
        {
          ps("#<pointer", file, 0);
          goto ppoint;
        }
      break;
    case NIL:
      ps("nil", file, 0);
      break;
    case TRUE:
      putch('t', file, 0);
      break;
    case INTEGER:
      pi(INTVAL(x), INTVAL(currentbase), file);
      break;
    case FLOAT:
      pf(FLOATVAL(x), file);
      break;
    case STRING:
      if (esc)
        {
          putch('"', file, 0);
          ps(STRINGVAL(x), file, esc);
          putch('"', file, 0);
        }
      else
        ps(STRINGVAL(x), file, 0);
      break;
    case CLOSURE:
      ps("#<closure", file, 0);
      goto ppoint;
    case LAMBDA:
      ps("#<lambda", file, 0);
      goto ppoint;
    case NLAMBDA:
      ps("#<nlambda", file, 0);
      goto ppoint;
    case INDIRECT:
      ps("#<indirect", file, 0);
      goto ppoint;
    case SUBR:
      ps("#<subr", file, 0);
      goto ppoint;
    case FSUBR:
      ps("#<fsubr", file, 0);
      goto ppoint;
    case UNBOUND:
      ps("#<unbound>", file, 0);
      break;
    case ENVIRON:
      ps("#<environ", file, 0);
      goto ppoint;
    case FREE:
      ps("#<free", file, 0);
      goto ppoint;
    case ENDOFFILE:
      ps("#<endoffile", file, 0);
      goto ppoint;
    case FILET:
      ps("#<file", file, 0);
      goto ppoint;
    case ERROR:
      ps("#<error", file, 0);
ppoint:
      ps(" ", file, 0);
      pi((long) INTVAL (x), 16L, file);
      ps(">", file, 0);
      break;
    default:
      ps("#<illegal ", file, 0);
      pi(TYPEOF(x), INTVAL(currentbase), file);
      pi((long) INTVAL (x), 16L, file);
      ps(">", file, 0);
    }
  return x;
}

LISPT
print(x, file)
  LISPT x;
  FILE *file;
{
  thisplevel = 0;
  (void) prin0(x, file, 1);
  (void) terpri(file);
  return x;
}

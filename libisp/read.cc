/*
 * Lips, lisp shell.
 * Copyright 1988, 2020 Krister Joas
 *
 * $Id$
 */

#include "libisp.hh"

extern int getch(FILE*);
extern void ungetch(int, FILE*);
extern int eoln(FILE*);
extern void putch(int, FILE*, int);
extern lisp::LISPT histget(long, lisp::LISPT);
extern lisp::LISPT history;

namespace {
constexpr int NUL = '\0';
constexpr int MAXATOMSIZE = 128; /* max length of atom read can handle */
}

#define CHECKEOF(c) \
  if((c) == EOF) \
  { \
    if(line || ISNIL(top->car())) \
      return C_EOF; \
    else \
      return error(UNEXPECTED_EOF, C_NIL); \
  }

#define GETCH(file) \
  do \
    curc = getch(file); \
  while(curc != EOF && issepr(curc)); \
  CHECKEOF(curc);

namespace lisp {

inline void pushr(LISPT w) { rstack = cons(w, rstack); }
inline void popr(LISPT& w) { w = rstack->car(); rstack = rstack->cdr(); }

static char buf[MAXATOMSIZE];

static LISPT rmsquote(FILE*, LISPT, char);
static LISPT rmdquote(FILE*, LISPT, char);
static LISPT rmexcl(FILE*, LISPT, char);

#if 0
static LISPT rmpipe(FILE*, LISPT, char);
static LISPT rmredir(FILE*, LISPT, char);
static LISPT rmbg(FILE*, LISPT, char);
static LISPT userreadmacros[128];
static LISPT rmuser(FILE*, LISPT, char); /* Installed for user read macros. */
#endif

LISPT prin0(LISPT, FILE*, int);

LISPT top;           /* used for threading the input structure */
LISPT rstack;        /* partially built structure read stack */
long printlevel = 0; /* maximum print level */
long thisplevel;     /* during print, print level */
int echoline;        /* is 1 if ! has been used */

/* clang-format off */
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

/*
 * This state table parses a floating point number.
 */
#if FLOATING
static int nxtstate[4][10] = {
  { 1,-1,-1,-1,-1,-1,-1, 8,-1,-1}, 
  { 2, 2, 2, 4, 4, 6, 6, 9, 9, 9}, 
  {-1,-1, 7, 7, 7,-1, 7,-1,-1,-1}, 
  { 5, 5, 3,-1,-1,-1,-1,-1,-1,-1}
};
#endif
static char digits[] = {
  '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
  'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j',
  'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't',
  'u', 'v', 'w', 'x', 'y', 'z'};
/* clang-format on */

/*
 * INTEGERP returns nonzero if the characters in buffer BUF
 * represents an integer, and the result as a long in res.
 */
static int integerp(char* buf, long* res)
{
  int d = 0;
  int sign = 1;

  *res = 0;
  if(*buf == '-')
    sign = *buf++ == '-' ? -1 : 1;
  if(!*buf)
    d = 1;
  for(; *buf; buf++)
  {
    if(!isdigit(*buf))
      d++;
    else
      *res = *res * 10 + *buf - '0';
  }
  *res *= sign;
  return !d;
}

/*
 * Returns nonzero if buffer BUF is a floating point constant.
 */
#ifdef FLOATING
static int floatp(char* buf)
{
  int state = 0;
  while(state >= 0 && *buf)
  {
    switch(*buf)
    {
      case '+':
      case '-':
        state = nxtstate[0][state];
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
  if(state == 3 || state == 4 || state == 6 || state == 9)
    return 1;
  else
    return 0;
}
#endif

/*
 * Find out if the buffer can be interpreted as numbers of
 * any kind.
 */
static LISPT parsebuf(char* buf)
{
  long longval;

  if(integerp(buf, &longval))
    return mknumber(longval);
#ifdef FLOATING
  else if(floatp(buf))
    return mkfloat(atof(buf));
#endif /* FLOATING */
  else
    return mkatom(buf);
}

/*
 * Read an atom from FILE.
 */
LISPT ratom(FILE* file)
{
  int pos = 0;

  int c = getch(file);
  while(1)
  {
    if(c == EOF)
      return C_EOF;
    else if(issepr(c))
      ;
    else if(isbrk(c))
    {
      buf[pos++] = c;
      buf[pos] = NUL;
      return mkatom(buf);
    }
    else
    {
      while(1)
      {
        if(c == EOF)
          return parsebuf(buf);
        if(c == '\\')
          c = getch(file);
        if(pos < MAXATOMSIZE)
          buf[pos++] = c;
        c = getch(file);
        if(isbrk(c))
        {
          ungetch(c, file);
          buf[pos] = NUL;
          return parsebuf(buf);
        }
        else if(issepr(c))
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
static LISPT splice(LISPT c, LISPT l, int tailp)
{
  LISPT t = c->cdr();
  if(TYPEOF(l) != CONS)
  {
    if(tailp)
      rplacd(c, cons(l, t));
    else
      rplaca(c, l);
    return c;
  }
  if(!tailp)
  {
    rplaca(c, l->car());
    l = l->cdr();
  }
  if(ISNIL(l))
    return c;
  rplacd(c, l);
  LISPT t2 = C_NIL;
  for(; TYPEOF(l) == CONS; l = l->cdr()) t2 = l;
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
LISPT lispread(FILE* file, int line)
{
  LISPT curr, temp, curatom;
  int curc;

  if(!line)
  {
    top = cons(C_NIL, C_NIL);
    curr = top;
  }
  else
    curr = top->car();
head:
  GETCH(file);
  if(isinsert(curc))
  {
    pushr(top);
    rplaca(curr, (*currentrt.rmacros[curc])(file, curr, curc));
    popr(top);
    goto check;
  }
  else if(issplice(curc))
  {
    pushr(top);
    temp = (*currentrt.rmacros[curc])(file, curr, curc);
    popr(top);
    curr = splice(curr, temp, 0);
    goto check;
  }
  else if(curc == '(')
  {
  head2:
    rplaca(curr, cons(C_NIL, C_NIL));
    rplacd(curr->car(), curr);
    curr = curr->car();
    goto head;
  }
  else if(curc == ')')
  {
    curr = curr->cdr();
    rplaca(curr, C_NIL);
    goto check;
  }
  else
  {
    ungetch(curc, file);
    curatom = ratom(file);
    rplaca(curr, curatom);
  check:
    if(ISNIL(curr->cdr()))
    {
      temp = top->car();
      top = C_NIL;
      return temp;
    }
    else if(line && eoln(file) && EQ(curr->cdr(), top))
      goto addparen;
    else
      goto tail;
  }
tail:
  if(line && eoln(file) && EQ(curr->cdr(), top))
    goto addparen;
  GETCH(file);
  if(isinsert(curc))
  {
    temp = curr->cdr();
    rplacd(curr, cons(C_NIL, temp));
    curr = curr->cdr();
    pushr(top);
    rplaca(curr, (*currentrt.rmacros[curc])(file, curr, curc));
    popr(top);
    goto tail;
  }
  else if(issplice(curc))
  {
    pushr(top);
    temp = (*currentrt.rmacros[curc])(file, curr, curc);
    popr(top);
    curr = splice(curr, temp, 1);
    goto tail;
  }
  else if(isinfix(curc))
  {
    curr = (*currentrt.rmacros[curc])(file, curr, curc);
    goto head;
  }
  else if(curc == ')')
  {
  addparen:
    temp = curr->cdr();
    rplacd(curr, C_NIL);
    curr = temp;
    goto check;
  }
  else if(curc == '(')
  {
    temp = curr->cdr();
    rplacd(curr, cons(C_NIL, C_NIL));
    curr = curr->cdr();
    rplacd(curr, temp);
    goto head2;
  }
  else if(curc == '.')
  {
    curc = getch(file);
    CHECKEOF(curc);
    if(!issepr(curc) && !isbrk(curc))
    {
      ungetch(curc, file);
      ungetch('.', file); /* cross your fingers */
      goto atom;
    }
    if(curc == ')' || eoln(file))
    {
      ungetch(curc, file);
      curatom = C_DOT;
      goto insert;
    }
    if(isbrk(curc))
      ungetch(curc, file);
    curatom = ratom(file);
    temp = curr->cdr();
    GETCH(file);
    if(curc != ')')
    {
      rplacd(curr, cons(C_DOT, cons(C_NIL, temp)));
      curr = curr->cdr()->cdr();
      rplaca(curr, curatom);
      goto another;
    }
    rplacd(curr, curatom);
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
    temp = curr->cdr();
    rplacd(curr, cons(C_NIL, temp));
    curr = curr->cdr();
    rplaca(curr, curatom);
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
static LISPT rmexcl(FILE* file, LISPT, char)
{
  LISPT at, l;

  int c = getch(file);
  if(issepr(c))
    return C_EXCL;
  echoline = 1;
  LISPT tmp = histget(0L, history);
  if(TYPEOF(tmp->car()) == CONS && ISNIL(tmp->cdr()))
    tmp = tmp->car();
  switch(c)
  {
    case '!':
      return histget(0L, history);
      break;
    case '$':
      while(TYPEOF(tmp->cdr()) == CONS) tmp = tmp->cdr();
      return tmp;
      break;
    case '*':
      return tmp->cdr();
      break;
    case '\n':
      echoline = 0;
      return C_EXCL;
      break;
    default:
      ungetch(c, file);
      at = ratom(file);
      if(TYPEOF(at) == INTEGER)
      {
        tmp = histget(at->intval(), history);
        return tmp;
      }
      if(TYPEOF(at) == SYMBOL)
      {
        for(l = history; !ISNIL(l); l = l->cdr())
        {
          tmp = histget(0L, l);
          if(!ISNIL(tmp) && TYPEOF(tmp->car()) == CONS && ISNIL(tmp->cdr()))
            tmp = tmp->car();
          if(!strncmp(tmp->car()->getstr(), at->getstr(), strlen(at->getstr())))
            return histget(0L, l);
        }
        return C_NIL;
      }
      else
      {
        error(EVENT_NOT_FOUND, at);
        return C_NIL;
      }
  }
  return C_NIL;
}

static LISPT rmdquote(FILE* file, LISPT, char)
{
  char buf[MAXATOMSIZE];
  char c;
  int pos = 0;

  c = getch(file);
  while(c != '"' && pos < MAXATOMSIZE)
  {
    if(c == '\\')
      c = getch(file);
    buf[pos++] = c;
    c = getch(file);
  }
  buf[pos] = NUL;
  return mkstring(buf);
}

static LISPT rmsquote(FILE* file, LISPT _0, char _1)
{
  int c;

  if((c = getch(file)) == ')' || issepr(c))
  {
    ungetch(c, file);
    return C_QUOTE;
  }
  ungetch(c, file);
  return cons(C_QUOTE, cons(lispread(file, 0), C_NIL));
}

#if 0
static LISPT rmpipe(FILE*, LISPT curr, char)
{
  LISPT t1, t2;

  t1 = CDR(curr);
  rplaca(
    t1, cons(C_PIPE, cons(CAR(CDR(curr)), cons(t2 = cons(C_NIL, t1), C_NIL))));
  rplacd(curr, C_NIL);
  return t2;
}

static LISPT rmredir(FILE* file, LISPT curr, char curc)
{
  LISPT t1, t2;
  char c;

  t1 = CDR(curr);
  c = getch(file);
  rplaca(t1,
    cons((curc == '<') ? C_FROM : ((c == '>') ? C_TOTO : C_TO),
      cons(CAR(CDR(curr)), t2 = cons(C_NIL, t1))));
  if (!(c == '>' || curc == '>'))
    ungetch(c, file);
  rplacd(curr, C_NIL);
  return t2;
}

static LISPT rmbg(FILE*, LISPT curr, char)
{
  rplaca(CDR(curr), cons(C_BACK, CAR(CDR(curr))));
  rplacd(curr, cons(C_NIL, CDR(curr)));
  return CDR(curr);
}

/*
 * Handles user macros.
 */
static LISPT rmuser(FILE*, LISPT curr, char curc)
{
  if (ISNIL(userreadmacros[(int)curc]))
    return curr;
  else

    curr = apply(userreadmacros[(int)curc], curr);
  return curr;
}
#endif

LISPT readline(FILE* file)
{
  LISPT rd;

  top = cons(C_NIL, C_NIL); /* Init first paren level */
  rplaca(top, cons(C_NIL, top));
  rd = lispread(file, 1);
  return rd;
}

/* print the string s, on stream file */
static void ps(const char* s, FILE* file, int esc)
{
  while(*s) putch(*s++, file, esc);
}

static void pi(long i, long base, FILE* file)
{
  char ss[33];
  int sign;
  int j = 31;

  ss[32] = 0;
  sign = (i < 0) ? -1 : 1;
  i = sign * i;
  if(!i)
    ps("0", file, 0);
  else
  {
    while(i)
    {
      ss[j--] = digits[i % base];
      i /= base;
    }
  }
  if(sign == -1)
    ss[j--] = '-';
  ps(ss + j + 1, file, 0);
}

static void pf(double d, FILE* file)
{
  char ss[30];

  sprintf(ss, "%#g", d);
  ps(ss, file, 0);
}

LISPT patom(LISPT x, FILE* file, int esc)
{
  ps(x->symval().pname, file, esc);
  return x;
}

LISPT terpri(FILE* file)
{
  putch('\n', file, 0);
  return C_NIL;
}

LISPT prinbody(LISPT x, FILE* file, int esc)
{
  LISPT xx;

  xx = x;
nxtelt:
  prin0(xx->car(), file, esc);
  if(EQ(xx->cdr(), C_NIL))
    ;
  else if(TYPEOF(xx->cdr()) == CONS)
  {
    putch(' ', file, 0);
    xx = xx->cdr();
    goto nxtelt;
  }
  else
  {
    putch(' ', file, 0);
    putch('.', file, 0);
    putch(' ', file, 0);
    prin0(xx->cdr(), file, esc);
  }
  return x;
}

LISPT prin0(LISPT x, FILE* file, int esc)
{
  switch(TYPEOF(x))
  {
    case CONS:
      thisplevel++;
      if(thisplevel <= printlevel || printlevel <= 0)
      {
        putch('(', file, 0);
        prinbody(x, file, esc);
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
      if(x->cpointval() != nullptr)
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
      pi(x->intval(), currentbase->intval(), file);
      break;
    case FLOAT:
      pf(x->floatval(), file);
      break;
    case STRING:
      if(esc)
      {
        putch('"', file, 0);
        ps(x->stringval(), file, esc);
        putch('"', file, 0);
      }
      else
        ps(x->stringval(), file, 0);
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
      pi((long)x->intval(), 16L, file);
      ps(">", file, 0);
      break;
    default:
      ps("#<illegal ", file, 0);
      pi(TYPEOF(x), currentbase->intval(), file);
      pi((long)x->intval(), 16L, file);
      ps(">", file, 0);
  }
  return x;
}

LISPT print(LISPT x, FILE* file)
{
  thisplevel = 0;
  prin0(x, file, 1);
  terpri(file);
  return x;
}

}

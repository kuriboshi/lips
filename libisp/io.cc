/*
 * Lips, lisp shell.
 * Copyright 1988, 2020 Krister Joas
 *
 */

#include "libisp.hh"

extern lisp::LISPT histget(int, lisp::LISPT);
extern lisp::LISPT history;

namespace lisp
{
inline bool checkeof(lisp& l, int c, bool line)
{
  if(c == EOF)
  {
    if(line || is_NIL(l.top->car()))
      return true;
    l.error(UNEXPECTED_EOF, C_NIL);
  }
  return false;
}

inline std::pair<bool, int> getchar(lisp& l, file_t& file, bool line)
{
  int curc = 0;
  do
    curc = file.getch();
  while(curc != EOF && issepr(l, curc));
  if(checkeof(l, curc, line))
    return std::make_pair(true, curc);
  return std::make_pair(false, curc);
}

/* clang-format off */
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
bool io::integerp(char* buf, int* res)
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
bool io::floatp(char* buf)
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
    return true;
  return false;
}
#endif

/*
 * Find out if the buffer can be interpreted as numbers of
 * any kind.
 */
LISPT io::parsebuf(char* buf)
{
  int longval;

  if(integerp(buf, &longval))
    return mknumber(l, longval);
#ifdef FLOATING
  else if(floatp(buf))
    return mkfloat(l, atof(buf));
#endif /* FLOATING */
  return mkatom(l, buf);
}

/*
 * Read an atom from FILE.
 */
LISPT io::ratom(file_t& file)
{
  char buffer[MAXATOMSIZE];
  int pos = 0;

  int c = file.getch();
  while(1)
  {
    if(c == EOF)
      return C_EOF;
    else if(issepr(l, c))
      ;
    else if(isbrk(l, c))
    {
      buffer[pos++] = c;
      buffer[pos] = NUL;
      return mkatom(l, buffer);
    }
    else
    {
      while(1)
      {
        if(c == EOF)
          return parsebuf(buffer);
        if(c == '\\')
          c = file.getch();
        if(pos < MAXATOMSIZE)
          buffer[pos++] = c;
        c = file.getch();
        if(isbrk(l, c))
        {
          file.ungetch(c);
          buffer[pos] = NUL;
          return parsebuf(buffer);
        }
        else if(issepr(l, c))
        {
          buffer[pos] = NUL;
          return parsebuf(buffer);
        }
      }
    }
    c = file.getch();
  }
}

/*
 * Splice list y into x keeping cdr of x. If y is not a list put it in car of x
 * and return x, otherwise return last cell of y with cdr set to original (cdr
 * x).  If tailp is true, don't clobber car of x.
 */
LISPT io::splice(LISPT x, LISPT y, bool tailp)
{
  LISPT t = x->cdr();
  if(type_of(x) != CONS)
  {
    if(tailp)
      rplacd(l, x, a.cons(y, t));
    else
      rplaca(l, x, y);
    return x;
  }
  if(!tailp)
  {
    rplaca(l, x, y->car());
    y = y->cdr();
  }
  if(is_NIL(y))
    return x;
  rplacd(l, x, y);
  LISPT t2 = C_NIL;
  for(; type_of(y) == CONS; y = y->cdr()) t2 = y;
  return rplacd(l, t2, t);
}

/*
 * LISPREAD reads a lisp expression from file FILE. If LINE
 * is true then it is assumed that it was called from READLINE.
 * READLINE initializes by itself TOP so that an extra level of
 * parentheses is in effect. An explicit stack is used to store
 * TOP when LISPREAD recurses.
 */
/*
 * If you don't like goto's, keep your eyes shut.
 */
LISPT io::lispread(file_t& file, bool line)
{
  LISPT curr, temp, curatom;
  int curc;

  if(!line)
  {
    l.top = cons(l, C_NIL, C_NIL);
    curr = l.top;
  }
  else
    curr = l.top->car();
head:
  {
    auto [eof, c] = getchar(l, file, line);
    if(eof)
      return C_EOF;
    curc = c;
  }
  if(isinsert(l, curc))
  {
    pushr(l.top);
    rplaca(l, curr, (*l.currentrt.rmacros[curc])(l, file, curr, curc));
    popr(l.top);
    goto check;
  }
  else if(issplice(l, curc))
  {
    pushr(l.top);
    temp = (*l.currentrt.rmacros[curc])(l, file, curr, curc);
    popr(l.top);
    curr = splice(curr, temp, 0);
    goto check;
  }
  else if(curc == '(')
  {
  head2:
    rplaca(l, curr, cons(l, C_NIL, C_NIL));
    rplacd(l, curr->car(), curr);
    curr = curr->car();
    goto head;
  }
  else if(curc == ')')
  {
    curr = curr->cdr();
    rplaca(l, curr, C_NIL);
    goto check;
  }
  else
  {
    file.ungetch(curc);
    curatom = ratom(file);
    rplaca(l, curr, curatom);
  check:
    if(is_NIL(curr->cdr()))
    {
      temp = l.top->car();
      l.top = C_NIL;
      return temp;
    }
#if 0
    else if(line && file.eoln() && EQ(curr->cdr(), l.top))
      goto addparen;
#endif
    goto tail;
  }
tail:
#if 0
  if(line && file.eoln() && EQ(curr->cdr(), l.top))
    goto addparen;
#endif
  {
    auto [eof, c] = getchar(l, file, line);
    if(eof)
      return C_EOF;
    curc = c;
  }
  if(isinsert(l, curc))
  {
    temp = curr->cdr();
    rplacd(l, curr, cons(l, C_NIL, temp));
    curr = curr->cdr();
    pushr(l.top);
    rplaca(l, curr, (*l.currentrt.rmacros[curc])(l, file, curr, curc));
    popr(l.top);
    goto tail;
  }
  else if(issplice(l, curc))
  {
    pushr(l.top);
    temp = (*l.currentrt.rmacros[curc])(l, file, curr, curc);
    popr(l.top);
    curr = splice(curr, temp, 1);
    goto tail;
  }
  else if(isinfix(l, curc))
  {
    curr = (*l.currentrt.rmacros[curc])(l, file, curr, curc);
    goto head;
  }
  else if(curc == ')')
  {
    // addparen:
    temp = curr->cdr();
    rplacd(l, curr, C_NIL);
    curr = temp;
    goto check;
  }
  else if(curc == '(')
  {
    temp = curr->cdr();
    rplacd(l, curr, cons(l, C_NIL, C_NIL));
    curr = curr->cdr();
    rplacd(l, curr, temp);
    goto head2;
  }
  else if(curc == '.')
  {
    curc = file.getch();
    if(checkeof(l, curc, line))
      return C_EOF;
    if(!issepr(l, curc) && !isbrk(l, curc))
    {
      file.ungetch(curc);
      file.ungetch('.'); /* cross your fingers */
      goto atom;
    }
    if(curc == ')' || file.eoln())
    {
      file.ungetch(curc);
      curatom = C_DOT;
      goto insert;
    }
    if(isbrk(l, curc))
      file.ungetch(curc);
    curatom = ratom(file);
    temp = curr->cdr();
    {
      auto [eof, c] = getchar(l, file, line);
      if(eof)
        return C_EOF;
      curc = c;
    }
    if(curc != ')')
    {
      rplacd(l, curr, cons(l, C_DOT, cons(l, C_NIL, temp)));
      curr = curr->cdr()->cdr();
      rplaca(l, curr, curatom);
      goto another;
    }
    rplacd(l, curr, curatom);
    curr = temp;
    goto check;
  }
  else
  {
  another:
    file.ungetch(curc);
  atom:
    curatom = ratom(file);
  insert:
    temp = curr->cdr();
    rplacd(l, curr, cons(l, C_NIL, temp));
    curr = curr->cdr();
    rplaca(l, curr, curatom);
    goto tail;
  }
}

/*
 * Read macros.
 */
LISPT io::rmdquote(lisp& l, file_t& file, LISPT, char)
{
  char buffer[MAXATOMSIZE];
  char c;
  int pos = 0;

  c = file.getch();
  while(c != '"' && pos < MAXATOMSIZE)
  {
    if(c == '\\')
      c = file.getch();
    buffer[pos++] = c;
    c = file.getch();
  }
  buffer[pos] = NUL;
  return mkstring(l, buffer);
}

LISPT io::rmsquote(lisp& l, file_t& file, LISPT, char)
{
  int c;

  if((c = file.getch()) == ')' || issepr(l, c))
  {
    file.ungetch(c);
    return C_QUOTE;
  }
  file.ungetch(c);
  return cons(l, C_QUOTE, cons(l, io(l).lispread(file, false), C_NIL));
}

#if 0
LISPT io::rmpipe(lisp& l, file_t&, LISPT curr, char)
{
  LISPT t1, t2;

  t1 = curr->cdr();
  rplaca(
    t1, cons(l, C_PIPE, cons(l, curr->cdr()->car(), cons(l, t2 = cons(l, C_NIL, t1), C_NIL))));
  rplacd(l, curr, C_NIL);
  return t2;
}

LISPT io::rmredir(lisp& l, file_t& file, LISPT curr, char curc)
{
  LISPT t1, t2;
  char c;

  t1 = curr->cdr();
  c = file.getch();
  rplaca(l, t1,
    cons(l, (curc == '<') ? C_FROM : ((c == '>') ? C_TOTO : C_TO),
      cons(l, curr->cdr()->car(), t2 = cons(l, C_NIL, t1))));
  if (!(c == '>' || curc == '>'))
    file.ungetch(c);
  rplacd(l, curr, C_NIL);
  return t2;
}

LISPT io::rmbg(lisp& l, file_t*, LISPT curr, char)
{
  rplaca(l, curr->cd(), cons(l, C_BACK, curr->cdr()->car()));
  rplacd(l, curr, cons(l, C_NIL, curr->cdr()));
  return curr->cdr();
}

/*
 * Handles user macros.
 */
LISPT io::rmuser(lisp& l, file_t&, LISPT curr, char curc)
{
  if (is_NIL(userreadmacros[static_cast<int>(curc)]))
    return curr;
  curr = apply(userreadmacros[static_cast<int>(curc)], curr);
  return curr;
}
#endif

LISPT io::readline(file_t& file)
{
#if 0
  l.top = cons(l, C_NIL, C_NIL); /* Init first paren level */
  rplaca(l, l.top, cons(l, C_NIL, l.top));
  LISPT rd = lispread(file, true);
#else
  LISPT rd = lispread(file, false);
#endif
  return rd;
}

/* print the string s, on stream file */
void ps(const char* s, file_t& file, bool esc)
{
  while(*s) file.putch(*s++, esc);
}

void pi(int i, int base, file_t& file)
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

void pf(double d, file_t& file)
{
  char ss[30];

  sprintf(ss, "%#g", d);
  ps(ss, file, 0);
}

LISPT io::patom(LISPT x, file_t& file, bool esc)
{
  ps(x->symval().pname, file, esc);
  return x;
}

LISPT io::terpri(file_t& file)
{
  file.putch('\n');
  return C_NIL;
}

LISPT io::prinbody(LISPT x, file_t& file, bool esc)
{
  LISPT xx;

  xx = x;
nxtelt:
  prin0(xx->car(), file, esc);
  if(EQ(xx->cdr(), C_NIL))
    ;
  else if(type_of(xx->cdr()) == CONS)
  {
    file.putch(' ');
    xx = xx->cdr();
    goto nxtelt;
  }
  else
  {
    file.putch(' ');
    file.putch('.');
    file.putch(' ');
    prin0(xx->cdr(), file, esc);
  }
  return x;
}

LISPT io::prin0(LISPT x, file_t& file, bool esc)
{
  switch(type_of(x))
  {
    case CONS:
      l.thisplevel++;
      if(l.thisplevel <= l.printlevel || l.printlevel <= 0)
      {
        file.putch('(');
        prinbody(x, file, esc);
        file.putch(')');
      }
      else
        file.putch('&');
      l.thisplevel--;
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
      file.putch('t');
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
        file.putch('"');
        ps(x->stringval(), file, esc);
        file.putch('"');
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
      pi(x->intval(), 16L, file);
      ps(">", file, 0);
      break;
    default:
      ps("#<illegal ", file, 0);
      pi(type_of(x), currentbase->intval(), file);
      pi(x->intval(), 16L, file);
      ps(">", file, 0);
  }
  return x;
}

LISPT io::print(LISPT x, file_t& file)
{
  l.thisplevel = 0;
  prin0(x, file, true);
  terpri(file);
  return x;
}

void io::init(lisp& l)
{
  l.set_read_table('"', INSERT, io::rmdquote);
  l.set_read_table('\'', INSERT, io::rmsquote);
}

} // namespace lisp

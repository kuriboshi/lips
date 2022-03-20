/*
 * Lips, lisp shell.
 * Copyright 1988, 2020-2022 Krister Joas
 */
#include "io.hh"
#include "alloc.hh"
#include "prim.hh"
#include "reader.hh"
#include "parser.hh"

namespace lisp
{
/* clang-format off */
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
/* clang-format on */

void io::pushr(lisp& l, LISPT w) { l.rstack = cons(l, w, l.rstack); }

void io::popr(lisp& l, LISPT& w)
{
  w = l.rstack->car();
  l.rstack = l.rstack->cdr();
}

//
// INTEGERP returns nonzero if the characters in buffer BUF represents an
// integer, and the result as a long in res.
//
bool io::integerp(const std::string& buf, int& res)
{
  res = 0;
  if(buf.empty())
    return false;

  auto i = buf.begin();
  int sign = 1;
  if(*i == '-')
  {
    sign = -1;
    ++i;
  }
  else if(*i == '+')
    ++i;
  if(i == buf.end())
    return false;
  for(; i != buf.end(); ++i)
  {
    if(!isdigit(*i))
      return false;
    res = res * 10 + *i - '0';
  }
  res *= sign;
  return i == buf.end();
}

//
// Returns nonzero if buffer BUF is a floating point constant.
//
bool io::floatp(const std::string& buf)
{
  int state = 0;
  auto i = buf.begin();
  while(state >= 0 && i != buf.end())
  {
    switch(*i)
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
    ++i;
  }
  if(state == 3 || state == 4 || state == 6 || state == 9)
    return true;
  return false;
}

//
// Find out if the buffer can be interpreted as numbers of any kind.
//
LISPT io::parsebuf(lisp& l, const std::string& buf)
{
  int longval;
  if(integerp(buf, longval))
    return mknumber(l, longval);
  else if(floatp(buf))
    return mkfloat(l, std::stod(buf));
  return mkatom(l, buf);
}

//
// Read an atom from FILE.
//
LISPT io::ratom(lisp& l, file_t& file)
{
  std::string buffer;
  while(true)
  {
    int c = file.getch();
    if(c == EOF)
      return C_EOF;
    else if(issepr(l, c))
      ;
    else if(isbrk(l, c))
    {
      buffer.push_back(c);
      return mkatom(l, buffer);
    }
    else
    {
      while(true)
      {
        if(c == EOF)
          return parsebuf(l, buffer);
        if(c == '\\')
          c = file.getch();
        buffer.push_back(c);
        c = file.getch();
        if(isbrk(l, c))
        {
          file.ungetch(c);
          return parsebuf(l, buffer);
        }
        else if(issepr(l, c))
          return parsebuf(l, buffer);
      }
    }
  }
}

/*
 * Splice list y into x keeping cdr of x. If y is not a list put it in car of x
 * and return x, otherwise return last cell of y with cdr set to original (cdr
 * x).  If tailp is true, don't clobber car of x.
 */
LISPT io::splice(lisp& l, LISPT x, LISPT y, bool tailp)
{
  LISPT t = x->cdr();
  if(type_of(x) != type::CONS)
  {
    if(tailp)
      rplacd(l, x, cons(l, y, t));
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
  LISPT t2 = NIL;
  for(; type_of(y) == type::CONS; y = y->cdr()) t2 = y;
  return rplacd(l, t2, t);
}

//
// LISPREAD reads a lisp expression from file FILE.
//
LISPT io::lispread(lisp& l, file_t& file)
{
  Reader reader(file.source());
  return Parser(reader).parse();
}

//
// Read macros.
//
LISPT io::rmdquote(lisp& l, file_t& file, LISPT, char)
{
  std::string buffer;
  auto c = file.getch(true);
  while(c != '"')
  {
    if(c == '\\')
      c = file.getch(true);
    buffer.push_back(c);
    c = file.getch(true);
  }
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
  return cons(l, C_QUOTE, cons(l, lispread(l, file), NIL));
}

#if 0
LISPT io::rmpipe(lisp& l, file_t&, LISPT curr, char)
{
  LISPT t1, t2;

  t1 = curr->cdr();
  rplaca(
    t1, cons(l, C_PIPE, cons(l, curr->cdr()->car(), cons(l, t2 = cons(l, NIL, t1), NIL))));
  rplacd(l, curr, NIL);
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
      cons(l, curr->cdr()->car(), t2 = cons(l, NIL, t1))));
  if (!(c == '>' || curc == '>'))
    file.ungetch(c);
  rplacd(l, curr, NIL);
  return t2;
}

LISPT io::rmbg(lisp& l, file_t*, LISPT curr, char)
{
  rplaca(l, curr->cd(), cons(l, C_BACK, curr->cdr()->car()));
  rplacd(l, curr, cons(l, NIL, curr->cdr()));
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

LISPT io::readline(lisp& l, file_t& file)
{
  auto line = file.getline();
  if(line)
  {
    Reader reader(*line);
    Parser parser(reader);
    auto head = parser.parse();
    if(head && head->empty())
      return head;
    if(listp(head) || head == NIL)
      return head;
    LISPT tail;
    while(true)
    {
      auto o = parser.parse();
      if(o && o->empty())
        break;
      if(tail == NIL)
        tail = cdr(head = cons(head, cons(o, NIL)));
      else
        tail = cdr(rplacd(tail, cons(o, NIL)));
    }
    if(tail == NIL)
      return cons(l, head, NIL);
    return head;
  }
  return NIL;
}

LISPT io::getline(lisp& l, LISPT file)
{
  check(file, type::FILET);
  auto line = file->fileval().getline();
  if(line)
    return mkstring(l, *line);
  return NIL;
}

/* print the string s, on stream file */
static void ps(const std::string& s, file_t& file, bool esc)
{
  for(auto c: s)
    file.putch(c, esc);
}

static void pi(std::int64_t i, int base, file_t& file)
{
  char ss[33];
  int sign;
  int j = 31;

  ss[32] = 0;
  sign = (i < 0) ? -1 : 1;
  i = sign * i;
  if(!i)
    ps("0", file, false);
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
  ps(ss + j + 1, file, false);
}

static void pf(double d, file_t& file)
{
  auto ss = fmt::format("{:#g}", d);
  ps(ss.c_str(), file, false);
}

// Print pointer type object
static void pp(const char* s, file_t& file, LISPT x)
{
  ps(s, file, false);
  ps(" ", file, false);
  pi(reinterpret_cast<std::int64_t>(&*x), 16L, file);
  ps(">", file, false);
}

LISPT io::patom(lisp&, LISPT x, file_t& file, bool esc)
{
  ps(x->symbol().pname.name, file, esc);
  return x;
}

LISPT io::terpri(lisp&, file_t& file)
{
  file.putch('\n');
  file.flush();
  return NIL;
}

LISPT io::prinbody(lisp& l, LISPT x, file_t& file, bool esc)
{
  LISPT xx = x;
nxtelt:
  prin0(l, xx->car(), file, esc);
  if(EQ(xx->cdr(), NIL))
    ;
  else if(type_of(xx->cdr()) == type::CONS)
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
    prin0(l, xx->cdr(), file, esc);
  }
  return x;
}

LISPT io::prin0(lisp& l, LISPT x, file_t& file, bool esc)
{
  switch(type_of(x))
  {
    case type::CONS:
      l.thisplevel++;
      if(l.thisplevel <= l.printlevel || l.printlevel <= 0)
      {
        file.putch('(');
        prinbody(l, x, file, esc);
        file.putch(')');
      }
      else
        file.putch('&');
      l.thisplevel--;
      break;
    case type::SYMBOL:
      return patom(l, x, file, esc);
      break;
    case type::NIL:
      ps("nil", file, false);
      break;
    case type::EMPTY:
      ps("", file, false);
      break;
    case type::T:
      file.putch('t');
      break;
    case type::INTEGER:
      pi(x->intval(), l.currentbase()->intval(), file);
      break;
    case type::FLOAT:
      pf(x->floatval(), file);
      break;
    case type::STRING:
      if(esc)
      {
        file.putch('"');
        ps(x->stringval(), file, esc);
        file.putch('"');
      }
      else
        ps(x->stringval(), file, false);
      break;
    case type::CLOSURE:
      pp("#<closure", file, x);
      break;
    case type::LAMBDA:
      pp("#<lambda", file, x);
      break;
    case type::NLAMBDA:
      pp("#<nlambda", file, x);
      break;
    case type::INDIRECT:
      pp("#<indirect", file, x);
      break;
    case type::SUBR:
      pp("#<subr", file, x);
      break;
    case type::FSUBR:
      pp("#<fsubr", file, x);
      break;
    case type::UNBOUND:
      ps("#<unbound>", file, false);
      break;
    case type::ENVIRON:
      pp("#<environ", file, x);
      break;
    case type::FREE:
      pp("#<free", file, x);
      break;
    case type::ENDOFFILE:
      pp("#<endoffile", file, x);
      break;
    case type::FILET:
      pp("#<file", file, x);
      break;
    case type::ERROR:
      pp("#<error", file, x);
      break;
    default:
      ps("#<illegal type_of:", file, false);
      pi(to_underlying(type_of(x)), l.currentbase()->intval(), file);
      pp("", file, x);
  }
  return x;
}

LISPT io::print(lisp& l, LISPT x, file_t& file)
{
  l.thisplevel = 0;
  prin0(l, x, file, true);
  terpri(l, file);
  return x;
}

void io::set_read_table(lisp& l)
{
  l.set_read_table('"', char_class::INSERT, io::rmdquote);
  l.set_read_table('\'', char_class::INSERT, io::rmsquote);
}

bool io::checkeof(lisp& l, int c, bool line)
{
  if(c == EOF)
  {
    if(line || is_NIL(l.top->car()))
      return true;
    l.fatal(UNEXPECTED_EOF);
  }
  return false;
}

std::pair<bool, int> io::getchar(lisp& l, file_t& file, bool line)
{
  int curc = 0;
  do
    curc = file.getch();
  while(curc != EOF && issepr(l, curc));
  if(checkeof(l, curc, line))
    return std::make_pair(true, curc);
  return std::make_pair(false, curc);
}

file_source::file_source(const std::string& name)
{
  _file = std::make_unique<std::ifstream>();
  _file->open(name, std::ios_base::in);
  if(_file->fail())
    _file->open((name + ".lisp"), std::ios_base::in);
  if(_file->fail())
    _file->open((name + ".lsp"), std::ios_base::in);
  // TODO: Throw different exception
  if(_file->fail())
    throw lisp_error("Can't open file " + name);
}

file_sink::file_sink(const std::string& name, bool append)
{
  _file = std::make_unique<std::ofstream>(name, append ? std::ios_base::ate : std::ios_base::out);
  if(_file->fail())
    throw lisp_error("Can't open file " + name);
}

} // namespace lisp

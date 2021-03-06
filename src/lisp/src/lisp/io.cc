/*
 * Lips, lisp shell.
 * Copyright 1988, 2020 Krister Joas
 *
 */

#include <doctest/doctest.h>
#include "libisp.hh"

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
LISPT io::parsebuf(const std::string& buf)
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
LISPT io::ratom(file_t& file)
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
          return parsebuf(buffer);
        if(c == '\\')
          c = file.getch();
        buffer.push_back(c);
        c = file.getch();
        if(isbrk(l, c))
        {
          file.ungetch(c);
          return parsebuf(buffer);
        }
        else if(issepr(l, c))
          return parsebuf(buffer);
      }
    }
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
  if(type_of(x) != lisp_type::CONS)
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
  LISPT t2 = C_NIL;
  for(; type_of(y) == lisp_type::CONS; y = y->cdr()) t2 = y;
  return rplacd(l, t2, t);
}

/*
 * LISPREAD reads a lisp expression from file FILE.  If LINE is true then it is
 * assumed that it was called from READLINE.  READLINE initializes by itself
 * TOP so that an extra level of parentheses is in effect.  An explicit stack
 * is used to store TOP when LISPREAD recurses.
 */
/*
 * If you don't like goto's, keep your eyes shut.
 */
LISPT io::lispread(file_t& file, bool line)
{
  LISPT curr, temp, curatom;
  if(!line)
  {
    l.top = cons(C_NIL, C_NIL);
    curr = l.top;
  }
  else
    curr = l.top->car();
head:
  auto [eof, c] = getchar(l, file, line);
  if(eof)
    return C_EOF;
  auto curc = c;
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
    else if(line && file.eoln() && EQ(curr->cdr(), l.top))
      goto addparen;
    goto tail;
  }
tail:
  if(line && file.eoln() && EQ(curr->cdr(), l.top))
    goto addparen;
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
  addparen:
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
  l.top = cons(l, C_NIL, C_NIL); /* Init first paren level */
  rplaca(l, l.top, cons(l, C_NIL, l.top));
  LISPT rd = lispread(file, true);
  return rd;
}

/* print the string s, on stream file */
static void ps(const std::string& s, file_t& file, bool esc)
{
  for(auto c: s)
    file.putch(c, esc);
}

static void pi(int i, int base, file_t& file)
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

static void pf(double d, file_t& file)
{
  auto ss = fmt::format("{:#g}", d);
  ps(ss.c_str(), file, 0);
}

// Print pointer type object
static void pp(const char* s, file_t& file, LISPT x)
{
  ps(s, file, 0);
  ps(" ", file, 0);
  pi((long)&x, 16L, file);
  ps(">", file, 0);
}

LISPT io::patom(LISPT x, file_t& file, bool esc)
{
  ps(x->symbol().pname, file, esc);
  return x;
}

LISPT io::terpri(file_t& file)
{
  file.putch('\n');
  return C_NIL;
}

LISPT io::prinbody(LISPT x, file_t& file, bool esc)
{
  LISPT xx = x;
nxtelt:
  prin0(xx->car(), file, esc);
  if(EQ(xx->cdr(), C_NIL))
    ;
  else if(type_of(xx->cdr()) == lisp_type::CONS)
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
    case lisp_type::CONS:
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
    case lisp_type::SYMBOL:
      return patom(x, file, esc);
      break;
    case lisp_type::CPOINTER:
      if(x->cpointval() != nullptr)
        pp("#<pointer", file, x);
      break;
    case lisp_type::NIL:
      ps("nil", file, false);
      break;
    case lisp_type::T:
      file.putch('t');
      break;
    case lisp_type::INTEGER:
      pi(x->intval(), l.currentbase->intval(), file);
      break;
    case lisp_type::FLOAT:
      pf(x->floatval(), file);
      break;
    case lisp_type::STRING:
      if(esc)
      {
        file.putch('"');
        ps(x->stringval(), file, esc);
        file.putch('"');
      }
      else
        ps(x->stringval(), file, false);
      break;
    case lisp_type::CLOSURE:
      pp("#<closure", file, x);
      break;
    case lisp_type::LAMBDA:
      pp("#<lambda", file, x);
      break;
    case lisp_type::NLAMBDA:
      pp("#<nlambda", file, x);
      break;
    case lisp_type::INDIRECT:
      pp("#<indirect", file, x);
      break;
    case lisp_type::SUBR:
      pp("#<subr", file, x);
      break;
    case lisp_type::UNBOUND:
      ps("#<unbound>", file, false);
      break;
    case lisp_type::ENVIRON:
      pp("#<environ", file, x);
      break;
    case lisp_type::FREE:
      pp("#<free", file, x);
      break;
    case lisp_type::ENDOFFILE:
      pp("#<endoffile", file, x);
      break;
    case lisp_type::FILET:
      pp("#<file", file, x);
      break;
    case lisp_type::ERROR:
      pp("#<error", file, x);
      break;
    default:
      ps("#<illegal ", file, false);
      pi(to_underlying(type_of(x)), l.currentbase->intval(), file);
      pp("", file, x);
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
  l.set_read_table('"', char_class::INSERT, io::rmdquote);
  l.set_read_table('\'', char_class::INSERT, io::rmsquote);
}

bool io::checkeof(lisp& l, int c, bool line)
{
  if(c == EOF)
  {
    if(line || is_NIL(l.top->car()))
      return true;
    l.error(UNEXPECTED_EOF, C_NIL);
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

TEST_CASE("Basic I/O")
{
  lisp lisp;

  lisp.primout(std::make_unique<file_t>(std::make_unique<string_sink>()));
  lisp.primout().format("hello world {}", 123);
  CHECK(to_string(lisp.primout().sink()) == std::string("hello world 123"));
}

TEST_CASE("Read lisp objects")
{
  lisp lisp;

  SUBCASE("Read from utf-8")
  {
    std::string s_nihongo{"\"日本語\"\n"};
    file_t in{s_nihongo};
    auto nihongo = lispread(lisp, in, false);
    file_t out(std::make_unique<string_sink>());
    print(lisp, nihongo, out);
    CHECK(to_string(out.sink()) == s_nihongo);
  }

  SUBCASE("Read from utf-8 2")
  {
    std::string s_nihongo{R"((((field "payee") (re "ライゼボツクス") (category "Housing/Storage")) ((field "payee") (re "ビューカード") (category "Transfer/viewcard")) ((field "payee") (re "楽天コミュニケー") (category "Utilities/Phone")))
)"};
    file_t in{s_nihongo};
    auto nihongo = lispread(lisp, in, false);
    file_t out(std::make_unique<string_sink>());
    print(lisp, nihongo, out);
    CHECK(to_string(out.sink()) == s_nihongo);
  }

  SUBCASE("Read utf-8 from file")
  {
    std::string s_nihongo{R"((((field "payee") (re "ライゼボツクス") (category "Housing/Storage")) ((field "payee") (re "ビューカード") (category "Transfer/viewcard")) ((field "payee") (re "楽天コミュニケー") (category "Utilities/Phone")))
)"};
    {
      std::ofstream o{"test.lisp"};
      o << s_nihongo;
    }
    auto f = file_t(std::make_unique<file_source>("test.lisp"));
    auto nihongo = lispread(lisp, f, false);
    file_t out(std::make_unique<string_sink>());
    print(lisp, nihongo, out);
    CHECK(to_string(out.sink()) == s_nihongo);
  }

  SUBCASE("lispread vs. readline")
  {
    std::string s0{R"((hello world))"};
    auto f0 = file_t(s0);
    auto result0 = lispread(lisp, f0, false);
    std::string s1{R"(hello world)"};
    auto f1 = file_t(s1);
    auto result1 = readline(lisp, f1);
    CHECK(equal(lisp, result0, result1) != C_NIL);
  }
}

} // namespace lisp

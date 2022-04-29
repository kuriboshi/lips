//
// Lips, lisp shell.
// Copyright 1988, 2020-2022 Krister Joas
//
#include "io.hh"
#include "alloc.hh"
#include "prim.hh"
#include "reader.hh"
#include "parser.hh"

namespace lisp
{
// clang-format off
static char digits[] = {
  '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
  'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j',
  'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't',
  'u', 'v', 'w', 'x', 'y', 'z'};
// clang-format on
}

namespace lisp::io
{
///
/// @brief Read an atom from FILE.
///
/// @details Reads one token from the file and creates a lisp object from that
/// token.
///
/// @param l The lisp interpreter to use.
/// @param file The source file.
///
/// @returns A lisp object which is either an integer, float, symbol, or
/// string. This differs from Interlisp which will never return a
/// string. Instead the first double quote is returned as a symbol.
///
LISPT ratom(lisp& l, file_t& file)
{
  reader reader{file.source()};
  auto token = reader.read();
  parser parser{reader};
  return parser.create(token);
}

//
// LISPREAD reads a lisp expression from file FILE.
//
LISPT lispread(lisp& l, file_t& file)
{
  reader reader(file.source());
  return parser(reader).parse();
}

LISPT readline(lisp& l, file_t& file)
{
  auto line = file.getline();
  if(line)
  {
    reader reader(*line);
    parser parser(reader);
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
  return C_EOF;
}

LISPT getline(lisp& l, LISPT file)
{
  check(file, type::FILET);
  auto line = file->file().getline();
  if(line)
    return mkstring(l, *line);
  return NIL;
}

// Print the string s, on stream file
inline void ps(const std::string& s, file_t& file, bool esc)
{
  for(auto c: s) file.putch(c, esc);
}

inline void pi(std::int64_t i, int base, file_t& file)
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

inline void pf(double d, file_t& file)
{
  auto ss = fmt::format("{:#g}", d);
  ps(ss.c_str(), file, false);
}

// Print pointer type object
inline void pp(const char* s, file_t& file, LISPT x)
{
  ps(s, file, false);
  ps(" ", file, false);
  pi(reinterpret_cast<std::int64_t>(&*x), 16L, file);
  ps(">", file, false);
}

LISPT patom(lisp&, LISPT x, file_t& file, bool esc)
{
  ps(x->symbol().pname, file, esc);
  return x;
}

LISPT prinbody(lisp& l, LISPT x, file_t& file, bool esc)
{
  auto i = x;
  for(;;)
  {
    io::prin0(l, i->car(), file, esc);
    if(is_NIL(i->cdr()))
      break;
    if(type_of(i->cdr()) == type::CONS)
    {
      file.putch(' ');
      i = i->cdr();
    }
    else
    {
      file.putch(' ');
      file.putch('.');
      file.putch(' ');
      io::prin0(l, i->cdr(), file, esc);
      break;
    }
  }
  return x;
}

LISPT prin0(lisp& l, LISPT x, file_t& file, bool esc)
{
  switch(type_of(x))
  {
    case type::CONS:
      l.thisplevel++;
      if(l.thisplevel <= l.printlevel || l.printlevel <= 0)
      {
        file.putch('(');
        io::prinbody(l, x, file, esc);
        file.putch(')');
      }
      else
        file.putch('&');
      l.thisplevel--;
      break;
    case type::SYMBOL:
      return io::patom(l, x, file, esc);
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
        ps(x->string(), file, esc);
        file.putch('"');
      }
      else
        ps(x->string(), file, false);
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

LISPT print(lisp& l, LISPT x, file_t& file)
{
  l.thisplevel = 0;
  io::prin0(l, x, file, true);
  io::terpri(l, file);
  return x;
}

LISPT terpri(lisp&, file_t& file)
{
  file.putch('\n');
  file.flush();
  return NIL;
}

/// @brief Splice an object into a list.
///
/// @details Splices list y into x keeping cdr of x. For example:
/// @code{.lisp}
/// (let ((x '(a b c))
///       (y '(x y z)))
///  (splice x y)
///  x)
/// @endcode
/// Modifies x to hold the value (x y x b c).
///
/// Another example:
/// @code{.lisp}
/// (let ((x '(a b c))
///       (y '(x y z)))
///  (splice (cdr x) y)
///  x)
/// @endcode
/// Modifies x to hold the value (a x y z c).
///
/// If y is not a list put it in car of x and return x, otherwise return last
/// cell of y with cdr set to original (cdr x). If tailp is true, don't clobber
/// car of x.
///
LISPT splice(lisp& l, LISPT x, LISPT y, bool tailp)
{
  check(x, type::CONS);
  if(is_NIL(y))
    return x;
  LISPT t = x->cdr();
  if(type_of(y) != type::CONS)
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
  rplacd(l, x, y);
  LISPT t2 = NIL;
  for(; type_of(y) == type::CONS; y = y->cdr()) t2 = y;
  return rplacd(l, t2, t);
}

//
// Read macros.
//
LISPT rmdquote(lisp& l, file_t& file, LISPT, char)
{
  std::string buffer;
  auto c = file.getch();
  while(c != '"')
  {
    if(c == '\\')
      c = file.getch();
    buffer.push_back(c);
    c = file.getch();
  }
  return mkstring(l, buffer);
}

LISPT rmsquote(lisp& l, file_t& file, LISPT, char)
{
  int c;
  if((c = file.getch()) == ')' || is_sepr(l, c))
  {
    file.ungetch(c);
    return C_QUOTE;
  }
  file.ungetch(c);
  return cons(l, C_QUOTE, cons(l, io::lispread(l, file), NIL));
}

void set_read_table(lisp& l)
{
  l.set_read_table('"', char_class::INSERT, io::rmdquote);
  l.set_read_table('\'', char_class::INSERT, io::rmsquote);
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

} // namespace io::lisp

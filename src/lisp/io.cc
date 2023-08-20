//
// Lips, lisp shell.
// Copyright 1988, 2020-2023 Krister Joas
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

#include <array>
#include <charconv>
#include <cstdint>
#include <string>

#include "check.hh"
#include "io.hh"
#include "parser.hh"
#include "pred.hh"
#include "prim.hh"

namespace lisp
{
// clang-format off
static std::array digits{
  std::to_array<char>({
      '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
      'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j',
      'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't',
      'u', 'v', 'w', 'x', 'y', 'z'})
};
// clang-format on
} // namespace lisp

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
lisp_t ratom(ref_file_t file)
{
  lexer lexer{file};
  auto token = lexer.read();
  parser parser{lexer};
  return parser.create(token);
}

//
// LISPREAD reads a lisp expression from file FILE.
//
lisp_t lispread(ref_file_t file)
{
  lexer lexer(file);
  return parser(lexer).parse();
}

lisp_t readline(ref_file_t file)
{
  auto line = file->getline();
  if(line)
  {
    lexer lexer{*line};
    parser parser(lexer);
    auto head = parser.parse();
    if(listp(head) || head == nil)
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

lisp_t getline(lisp_t file)
{
  check(file, object::type::File);
  auto line = file->file()->getline();
  if(line)
    return mkstring(*line);
  return nil;
}

// Print the string s, on stream file
inline void ps(const std::string& s, file_t& file, bool esc)
{
  for(auto c: s)
    file.putch(c, esc);
}

inline void pi(std::int64_t i, std::int64_t base, file_t& file)
{
  static const constexpr std::size_t buffer_size = 33;
  std::array<char, buffer_size> ss{};
  if(auto [ptr, ec] = std::to_chars(ss.data(), ss.data() + ss.size(), i, static_cast<int>(base)); ec == std::errc())
  {
    *ptr = '\0';
    ps(ss.data(), file, false);
  }
}

inline void pf(double d, file_t& file)
{
  auto ss = fmt::format("{:#g}", d);
  ps(ss, file, false);
}

// Print pointer type object
inline void pp(const char* s, file_t& file, lisp_t x)
{
  ps(s, file, false);
  ps(" ", file, false);
  pi(reinterpret_cast<std::int64_t>(&*x), 16L, file); // NOLINT
  ps(">", file, false);
}

inline void psubr(const char* s, file_t& file, lisp_t x)
{
  ps(s, file, false);
  ps(" ", file, false);
  ps(x->subr().name, file, false);
  ps(">", file, false);
}

lisp_t patom(lisp_t x, file_t& file, bool esc)
{
  ps(x->symbol()->pname, file, esc);
  return x;
}

lisp_t prinbody(lisp_t x, file_t& file, bool esc)
{
  auto i = x;
  for(;;)
  {
    io::prin0(i->car(), file, esc);
    if(is_nil(i->cdr()))
      break;
    if(type_of(i->cdr()) == object::type::Cons)
    {
      file.putch(' ');
      i = i->cdr();
    }
    else
    {
      file.putch(' ');
      file.putch('.');
      file.putch(' ');
      io::prin0(i->cdr(), file, esc);
      break;
    }
  }
  return x;
}

lisp_t prin0(lisp_t x, file_t& file, bool esc)
{
  switch(type_of(x))
  {
    case object::type::Cons:
      context::current().thisplevel++;
      if(context::current().thisplevel <= context::current().printlevel || context::current().printlevel <= 0)
      {
        file.putch('(');
        io::prinbody(x, file, esc);
        file.putch(')');
      }
      else
        file.putch('&');
      context::current().thisplevel--;
      break;
    case object::type::Symbol:
      return io::patom(x, file, esc);
    case object::type::Nil:
      ps("nil", file, false);
      break;
    case object::type::Integer:
      pi(x->intval(), context::current().currentbase()->intval(), file);
      break;
    case object::type::Float:
      pf(x->floatval(), file);
      break;
    case object::type::String:
      if(esc)
      {
        file.putch('"');
        ps(x->string(), file, esc);
        file.putch('"');
      }
      else
        ps(x->string(), file, false);
      break;
    case object::type::Closure:
      pp("#<closure", file, x);
      break;
    case object::type::Lambda:
      if(x->lambda().eval)
        pp("#<lambda", file, x);
      else
        pp("#<nlambda", file, x);
      break;
    case object::type::Indirect:
      pp("#<indirect", file, x);
      break;
    case object::type::Subr:
      if(x->subr().subr == subr_t::subr::EVAL)
        psubr("#<subr", file, x);
      else
        psubr("#<fsubr", file, x);
      break;
    case object::type::Environ:
      pp("#<environ", file, x);
      break;
    case object::type::File:
      pp("#<file", file, x);
      break;
    default:
      ps("#<illegal type_of:", file, false);
      pi(to_underlying(type_of(x)), context::current().currentbase()->intval(), file);
      pp("", file, x);
  }
  return x;
}

lisp_t print(lisp_t x, file_t& file)
{
  context::current().thisplevel = 0;
  io::prin0(x, file, true);
  io::terpri(file);
  return x;
}

lisp_t terpri(file_t& file)
{
  file.putch('\n');
  file.flush();
  return nil;
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
lisp_t splice(lisp_t x, lisp_t y, bool tailp)
{
  check(x, object::type::Cons);
  if(is_nil(y))
    return x;
  lisp_t t = x->cdr();
  if(type_of(y) != object::type::Cons)
  {
    if(tailp)
      rplacd(x, cons(y, t));
    else
      rplaca(x, y);
    return x;
  }
  if(!tailp)
  {
    rplaca(x, y->car());
    y = y->cdr();
  }
  rplacd(x, y);
  lisp_t t2 = nil;
  for(; type_of(y) == object::type::Cons; y = y->cdr())
    t2 = y;
  return rplacd(t2, t);
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
    throw lisp_error(error_errc::cant_open, name);
}

file_sink::file_sink(const std::string& name, bool append)
{
  _file = std::make_unique<std::ofstream>(name, append ? std::ios_base::app : std::ios_base::out);
  if(_file->fail())
    throw lisp_error(error_errc::cant_open, name);
}

} // namespace lisp::io

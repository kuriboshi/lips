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

#include <charconv>
#include <filesystem>

#include "alloc.hh"
#include "check.hh"
#include "file.hh"
#include "io.hh"
#include "iter.hh"
#include "lexer.hh"
#include "parser.hh"
#include "pred.hh"
#include "prim.hh"
#include "vm.hh"

namespace lisp
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
  lexer lexer{std::move(file)};
  auto token = lexer.read();
  const parser parser{lexer};
  return parser::create(token);
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

//
// LISPREAD reads a lisp expression from file FILE.
//
lisp_t lispread(ref_file_t file)
{
  lexer lexer(std::move(file));
  return parser(lexer).parse();
}

lisp_t getline(lisp_t file)
{
  check(file, object::type::File);
  auto line = file->file()->getline();
  if(line)
    return mkstring(*line);
  return nil;
}

bool loadfile(const std::string& lf)
{
  try
  {
    for(auto i: *vm::loadpath())
    {
      std::filesystem::path base{i->getstr()};
      base /= lf;
      if(std::filesystem::exists(base) || std::filesystem::exists(base.replace_extension(".lisp")))
      {
        auto foo = ref_file_t::create(std::make_unique<io::file_source>(base));
        for(auto rval = lispread(foo); rval != C_EOF; rval = lispread(foo))
          rval = lisp::vm::get().eval(rval);
        return true;
      }
    }
  }
  catch(const lisp_error&)
  {
    return false;
  }
  return false;
}

// Print the string s, on stream file
inline void ps(const std::string& s, file_t& file, io::escape esc)
{
  for(auto c: s)
    file.putch(c, esc);
}

inline void pi(std::int64_t i, std::int64_t base, file_t& file)
{
  static const constexpr std::size_t buffer_size = 33;
  std::array<char, buffer_size> ss{};
  if(auto [ptr, ec] = std::to_chars(ss.begin(), ss.end(), i, static_cast<int>(base)); ec == std::errc())
  {
    *ptr = '\0';
    ps(ss.data(), file, io::escape::NO);
  }
}

inline void pf(double d, file_t& file)
{
  auto ss = fmt::format("{:#g}", d);
  ps(ss, file, io::escape::NO);
}

// Print pointer type object
inline void pp(const char* s, file_t& file, const lisp_t& x)
{
  ps(s, file, io::escape::NO);
  ps(" ", file, io::escape::NO);
  pi(reinterpret_cast<std::int64_t>(&*x), 16L, file);
  ps(">", file, io::escape::NO);
}

inline void psubr(const char* s, file_t& file, const lisp_t& x)
{
  ps(s, file, io::escape::NO);
  ps(" ", file, io::escape::NO);
  ps(x->subr().name, file, io::escape::NO);
  ps(">", file, io::escape::NO);
}

lisp_t prinbody(lisp_t x, file_t& file, io::escape esc, std::int64_t current_printlevel)
{
  auto i = x;
  for(;;)
  {
    prin0(i->car(), file, esc, current_printlevel);
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
      prin0(i->cdr(), file, esc, current_printlevel);
      break;
    }
  }
  return x;
}

lisp_t prin0(lisp_t x, file_t& file, io::escape esc, std::int64_t current_printlevel)
{
  switch(type_of(x))
  {
    case object::type::Cons:
      ++current_printlevel;
      if(current_printlevel <= vm::printlevel() || vm::printlevel() <= 0)
      {
        file.putch('(');
        prinbody(x, file, esc, current_printlevel);
        file.putch(')');
      }
      else
        file.putch('&');
      break;
    case object::type::Symbol:
      return patom(x, file, esc);
    case object::type::Nil:
      ps("nil", file, io::escape::NO);
      break;
    case object::type::Integer:
      pi(x->intval(), vm::currentbase()->intval(), file);
      break;
    case object::type::Float:
      pf(x->floatval(), file);
      break;
    case object::type::String:
      if(esc == io::escape::YES)
      {
        file.putch('"');
        ps(x->string(), file, esc);
        file.putch('"');
      }
      else
        ps(x->string(), file, io::escape::NO);
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
      ps("#<illegal type_of:", file, io::escape::NO);
      pi(to_underlying(type_of(x)), vm::currentbase()->intval(), file);
      pp("", file, x);
  }
  return x;
}

lisp_t patom(lisp_t x, file_t& file, io::escape esc)
{
  ps(x->symbol()->pname, file, esc);
  return x;
}

lisp_t print(lisp_t x, file_t& file)
{
  prin0(x, file, io::escape::YES, 0);
  terpri(file);
  return x;
}

lisp::lisp_t terpri(lisp::file_t& file)
{
  file.putch('\n');
  file.flush();
  return lisp::nil;
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
  const lisp_t t = x->cdr();
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
} // namespace lisp

namespace lisp::details::file
{
lisp_t open(lisp_t filename, lisp_t mode)
{
  check(filename, object::type::String, object::type::Symbol);
  bool readmode = true;
  bool appendmode = false;
  if(!is_nil(mode))
  {
    check(mode, object::type::Symbol);
    if(mode == C_READ)
      readmode = true;
    else if(mode == C_WRITE)
      readmode = false;
    else if(mode == C_APPEND)
    {
      readmode = false;
      appendmode = true;
    }
    else
      return error(error_errc::unknown_request, mode);
  }
  auto* f = [&]() {
    if(readmode)
      return new file_t(std::make_unique<io::file_source>(filename->getstr()));
    return new file_t(std::make_unique<io::file_sink>(filename->getstr(), appendmode));
  }();
  return getobject(ref_file_t(f));
}

lisp_t close(lisp_t fildes)
{
  check(fildes, object::type::File);
  fildes->file()->close();
  return T;
}

lisp_t ratom(lisp_t file)
{
  if(is_nil(file))
    return ratom(vm::primin());
  if(is_T(file))
    return ratom(vm::stdin());
  check(file, object::type::File);
  return ratom(file->file());
}

lisp_t readc(lisp_t file)
{
  if(is_nil(file))
    return mknumber(vm::primin()->getch());
  if(is_T(file))
    return mknumber(vm::stdin()->getch());
  check(file, object::type::File);
  return mknumber(file->file()->getch());
}

lisp_t read(lisp_t file)
{
  if(is_nil(file))
    return lispread(vm::primin());
  if(is_T(file))
    return lispread(vm::stdin());
  check(file, object::type::File);
  return lispread(file->file());
}

lisp_t print(lisp_t x, lisp_t file)
{
  if(is_nil(file))
    return print(x, *vm::primout());
  if(is_T(file))
    return print(x, *vm::primerr());
  check(file, object::type::File);
  return print(x, *file->file());
}

lisp_t load(lisp_t f)
{
  check(f, object::type::String, object::type::Symbol);
  if(!loadfile(f->getstr()))
    fatal(error_errc::cant_load, f->getstr());
  return f;
}

lisp_t terpri(lisp_t file)
{
  if(is_nil(file))
    return terpri(*vm::primout());
  if(is_T(file))
    return terpri(*vm::primerr());
  check(file, object::type::File);
  return terpri(*file->file());
}

lisp_t prin1(lisp_t x, lisp_t file)
{
  if(is_nil(file))
    return prin0(x, *vm::primout(), io::escape::NO, 0);
  if(is_T(file))
    return prin0(x, *vm::primerr(), io::escape::NO, 0);
  check(file, object::type::File);
  return prin0(x, *file->file(), io::escape::NO, 0);
}

lisp_t prin2(lisp_t x, lisp_t file)
{
  if(is_nil(file))
    return prin0(x, *vm::primout(), io::escape::YES, 0);
  if(is_T(file))
    return prin0(x, *vm::primerr(), io::escape::YES, 0);
  check(file, object::type::File);
  return prin0(x, *file->file(), io::escape::YES, 0);
}

lisp_t printlevel(lisp_t newl)
{
  auto x = vm::printlevel();
  if(!is_nil(newl))
  {
    check(newl, object::type::Integer);
    vm::printlevel(newl->intval());
  }
  return mknumber(x);
}

lisp_t spaces(lisp_t n, lisp_t file)
{
  check(n, object::type::Integer);
  ref_file_t f;
  if(is_nil(file))
    f = vm::primout();
  else if(is_T(file))
    f = vm::primerr();
  else
  {
    check(file, object::type::File);
    f = file->file();
  }
  for(auto i = n->intval(); i > 0; i--)
    f->putch(' ');
  return nil;
}

lisp_t readline(lisp_t file)
{
  if(is_nil(file))
    return readline(vm::primin());
  if(is_T(file))
    return readline(vm::stdin());
  check(file, object::type::File);
  return readline(file->file());
}

namespace pn
{
inline constexpr std::string_view OPEN = "open";             // open file
inline constexpr std::string_view CLOSE = "close";           // close file
inline constexpr std::string_view LOAD = "load";             // load file
inline constexpr std::string_view PRIN1 = "prin1";           // print without escapes
inline constexpr std::string_view PRIN2 = "prin2";           // print without new-line
inline constexpr std::string_view PRINT = "print";           // print
inline constexpr std::string_view PRINTLEVEL = "printlevel"; // how deep to print
inline constexpr std::string_view RATOM = "ratom";           // read atom
inline constexpr std::string_view READ = "read";             // read expression
inline constexpr std::string_view READC = "readc";           // read characte
inline constexpr std::string_view READLINE = "readline";     // read a line
inline constexpr std::string_view SPACES = "spaces";         // print some spaces
inline constexpr std::string_view TERPRI = "terpri";         // print new-line
} // namespace pn

void init()
{
  // clang-format off
  mkprim(pn::CLOSE,      close,      subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::OPEN,       open,       subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::LOAD,       load,       subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::PRIN1,      prin1,      subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::PRIN2,      prin2,      subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::PRINT,      print,      subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::PRINTLEVEL, printlevel, subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::RATOM,      ratom,      subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::READ,       read,       subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::READC,      readc,      subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::READLINE,   readline,   subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::SPACES,     spaces,     subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::TERPRI,     terpri,     subr_t::subr::EVAL, subr_t::spread::SPREAD);
  // clang-format on
}

} // namespace lisp::details::file

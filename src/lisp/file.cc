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
#include "predicate.hh"
#include "list.hh"
#include "vm.hh"

namespace lisp
{
namespace
{
// Print the string s, on stream file
inline void print_string(const std::string& s, file_t& file, io::escape esc)
{
  for(auto c: s)
    file.putch(c, esc);
}

inline void print_int(integer_t::value_type i, integer_t::value_type base, file_t& file)
{
  static const constexpr std::size_t buffer_size = 33;
  std::array<char, buffer_size> ss{};
  if(auto [ptr, ec] = std::to_chars(begin(ss), end(ss), i, static_cast<int>(base)); ec == std::errc())
  {
    *ptr = '\0';
    print_string(ss.data(), file, io::escape::NO);
  }
}

inline void print_float(double_t::value_type d, file_t& file)
{
  auto ss = fmt::format("{:#g}", d);
  print_string(ss, file, io::escape::NO);
}

// Print pointer type object
inline void print_pointer(const char* s, file_t& file, const lisp_t& x)
{
  print_string(s, file, io::escape::NO);
  print_string(" ", file, io::escape::NO);
  print_int(reinterpret_cast<integer_t::value_type>(&*x), 16L, file);
  print_string(">", file, io::escape::NO);
}

inline void print_subr(const char* s, file_t& file, const lisp_t& x)
{
  print_string(s, file, io::escape::NO);
  print_string(" ", file, io::escape::NO);
  print_string(x->subr().name, file, io::escape::NO);
  print_string(">", file, io::escape::NO);
}
} // namespace

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
    if(listp(head) || head == nil || head == C_EOF)
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

bool loadfile(const std::string& filename)
{
  try
  {
    for(auto i: *vm::loadpath())
    {
      std::filesystem::path base{i->getstr()};
      base /= filename;
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

lisp_t prinbody(lisp_t a, file_t& file, io::escape esc, integer_t::value_type current_printlevel)
{
  auto i = a;
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
  return a;
}

lisp_t prin0(lisp_t a, file_t& file, io::escape esc, integer_t::value_type current_printlevel)
{
  switch(type_of(a))
  {
    case object::type::Cons:
      ++current_printlevel;
      if(current_printlevel <= vm::printlevel() || vm::printlevel() <= 0)
      {
        file.putch('(');
        prinbody(a, file, esc, current_printlevel);
        file.putch(')');
      }
      else
        file.putch('&');
      break;
    case object::type::Symbol:
      return patom(a, file, esc);
    case object::type::Nil:
      print_string("nil", file, io::escape::NO);
      break;
    case object::type::Integer:
      print_int(a->as_integer(), vm::currentbase()->as_integer(), file);
      break;
    case object::type::Float:
      print_float(a->as_double(), file);
      break;
    case object::type::String:
      if(esc == io::escape::YES)
      {
        file.putch('"');
        print_string(a->as_string(), file, esc);
        file.putch('"');
      }
      else
        print_string(a->as_string(), file, io::escape::NO);
      break;
    case object::type::Closure:
      print_pointer("#<closure", file, a);
      break;
    case object::type::Lambda:
      if(a->lambda().eval)
        print_pointer("#<lambda", file, a);
      else
        print_pointer("#<nlambda", file, a);
      break;
    case object::type::Indirect:
      print_pointer("#<indirect", file, a);
      break;
    case object::type::Subr:
      if(a->subr().subr == subr_t::subr::EVAL)
        print_subr("#<subr", file, a);
      else
        print_subr("#<fsubr", file, a);
      break;
    case object::type::Environ:
      print_pointer("#<environ", file, a);
      break;
    case object::type::File:
      print_pointer("#<file", file, a);
      break;
    default:
      print_string("#<illegal type_of:", file, io::escape::NO);
      print_int(to_underlying(type_of(a)), vm::currentbase()->as_integer(), file);
      print_pointer("", file, a);
  }
  return a;
}

lisp_t patom(lisp_t a, file_t& file, io::escape esc)
{
  print_string(a->as_symbol()->pname, file, esc);
  return a;
}

lisp_t print(lisp_t a, file_t& file)
{
  prin0(a, file, io::escape::YES, 0);
  terpri(file);
  return a;
}

lisp::lisp_t terpri(lisp::file_t& file)
{
  file.putch('\n');
  file.flush();
  return lisp::nil;
}

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
using lisp::vm;

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
    vm::printlevel(newl->as_integer());
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
  for(integer_t::value_type i = n->as_integer(); i > 0; i--)
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

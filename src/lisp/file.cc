//
// Lips, lisp shell.
// Copyright 1988, 2020-2022 Krister Joas
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

#include <filesystem>
#include "lisp/lisp.hh"

namespace lisp::details::file
{
LISPT open(lisp& l, LISPT filename, LISPT mode)
{
  check(filename, type::STRING, type::SYMBOL);
  bool readmode = true;
  bool appendmode = false;
  if(!is_NIL(mode))
  {
    check(mode, type::SYMBOL);
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
      l.error(error_errc::unknown_request, mode);
  }
  auto* f = [&]() {
    if(readmode)
      return new file_t(std::make_unique<io::file_source>(filename->getstr()));
    return new file_t(std::make_unique<io::file_sink>(filename->getstr(), appendmode));
  }();
  if(f == nullptr)
    l.error(error_errc::cant_open, filename);
  auto newfile = alloc::getobject();
  newfile->set(ref_file_t(f));
  return newfile;
}

LISPT close(lisp& l, LISPT fildes)
{
  check(fildes, type::FILET);
  if(fildes->file()->has_sink())
    fildes->file()->flush();
  if(fildes->file()->close())
    return T;
  return NIL;
}

LISPT ratom(lisp& l, LISPT file)
{
  if(is_NIL(file))
    return io::ratom(l.primin());
  if(is_T(file))
    return io::ratom(l.stdin());
  check(file, type::FILET);
  return io::ratom(file->file());
}

LISPT readc(lisp& l, LISPT file)
{
  if(is_NIL(file))
    return mknumber(l.primin()->getch());
  if(is_T(file))
    return mknumber(l.stdin()->getch());
  check(file, type::FILET);
  return mknumber(file->file()->getch());
}

LISPT read(lisp& l, LISPT file)
{
  if(is_NIL(file))
    return lispread(l.primin());
  if(is_T(file))
    return lispread(l.stdin());
  check(file, type::FILET);
  return lispread(file->file());
}

LISPT print(lisp& l, LISPT x, LISPT file)
{
  if(is_NIL(file))
    return io::print(l, x, *l.primout());
  if(is_T(file))
    return io::print(l, x, *l.primerr());
  check(file, type::FILET);
  return io::print(l, x, *file->file());
}

bool loadfile(lisp& l, const std::string& lf)
{
  try
  {
    for(auto i: *l.loadpath())
    {
      std::filesystem::path base{i->getstr()};
      base /= lf;
      if(std::filesystem::exists(base) || std::filesystem::exists(base.replace_extension(".lisp")))
      {
        auto foo = ref_file_t::create(std::make_unique<io::file_source>(base));
        for(auto rval = lispread(foo); type_of(rval) != type::EMPTY; rval = lispread(foo))
          rval = l.e().eval(rval);
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

LISPT load(lisp& l, LISPT f)
{
  check(f, type::STRING, type::SYMBOL);
  if(!file::loadfile(l, f->getstr()))
    l.fatal(error_errc::cant_load);
  return f;
}

LISPT terpri(lisp& l, LISPT file)
{
  if(is_NIL(file))
    return io::terpri(*l.primout());
  if(is_T(file))
    return io::terpri(*l.primerr());
  check(file, type::FILET);
  return io::terpri(*file->file());
}

LISPT prin1(lisp& l, LISPT x, LISPT file)
{
  l.thisplevel = 0;
  if(is_NIL(file))
    return prin0(x, *l.primout(), false);
  if(is_T(file))
    return prin0(x, *l.primerr(), false);
  check(file, type::FILET);
  return prin0(x, *file->file(), false);
}

LISPT prin2(lisp& l, LISPT x, LISPT file)
{
  l.thisplevel = 0;
  if(is_NIL(file))
    return prin0(x, *l.primout(), true);
  if(is_T(file))
    return prin0(x, *l.primerr(), true);
  check(file, type::FILET);
  return prin0(x, *file->file(), true);
}

LISPT printlevel(lisp& l, LISPT newl)
{
  auto x = l.printlevel;
  if(!is_NIL(newl))
  {
    check(newl, type::INTEGER);
    l.printlevel = newl->intval();
  }
  return mknumber(x);
}

LISPT spaces(lisp& l, LISPT n, LISPT file)
{
  int i = 0;
  ref_file_t f;

  check(n, type::INTEGER);
  if(is_NIL(file))
    f = l.primout();
  else if(is_T(file))
    f = l.primerr();
  else
  {
    check(file, type::FILET);
    f = file->file();
  }
  for(i = n->intval(); i > 0; i--)
    f->putch(' ');
  return NIL;
}

LISPT readline(lisp& l, LISPT file)
{
  if(is_NIL(file))
    return io::readline(l.primin());
  if(is_T(file))
    return io::readline(l.stdin());
  check(file, type::FILET);
  return io::readline(file->file());
}

namespace pn
{
inline constexpr auto OPEN = "open";             // open file
inline constexpr auto CLOSE = "close";           // close file
inline constexpr auto LOAD = "load";             // load file
inline constexpr auto PRIN1 = "prin1";           // print without escapes
inline constexpr auto PRIN2 = "prin2";           // print without new-line
inline constexpr auto PRINT = "print";           // print
inline constexpr auto PRINTLEVEL = "printlevel"; // how deep to print
inline constexpr auto RATOM = "ratom";           // read atom
inline constexpr auto READ = "read";             // read expression
inline constexpr auto READC = "readc";           // read characte
inline constexpr auto READLINE = "readline";     // read a line
inline constexpr auto SPACES = "spaces";         // print some spaces
inline constexpr auto TERPRI = "terpri";         // print new-line
} // namespace pn

void init()
{
  C_READ = intern(pn::READ);

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

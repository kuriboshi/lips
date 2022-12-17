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

#include "alloc.hh"
#include "check.hh"
#include "eval.hh"
#include "file.hh"
#include "io.hh"
#include "iter.hh"

namespace lisp::details::file
{
LISPT open(context& ctx, LISPT filename, LISPT mode)
{
  check(filename, type::String, type::Symbol);
  bool readmode = true;
  bool appendmode = false;
  if(!is_NIL(mode))
  {
    check(mode, type::Symbol);
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
      ctx.error(error_errc::unknown_request, mode);
  }
  auto* f = [&]() {
    if(readmode)
      return new file_t(std::make_unique<io::file_source>(filename->getstr()));
    return new file_t(std::make_unique<io::file_sink>(filename->getstr(), appendmode));
  }();
  if(f == nullptr)
    ctx.error(error_errc::cant_open, filename);
  return alloc::getobject(ref_file_t(f));
}

LISPT close(context&, LISPT fildes)
{
  check(fildes, type::File);
  if(fildes->file()->has_sink())
    fildes->file()->flush();
  fildes->file()->close();
  return T;
}

LISPT ratom(context& ctx, LISPT file)
{
  if(is_NIL(file))
    return io::ratom(ctx.primin());
  if(is_T(file))
    return io::ratom(ctx.stdin());
  check(file, type::File);
  return io::ratom(file->file());
}

LISPT readc(context& ctx, LISPT file)
{
  if(is_NIL(file))
    return mknumber(ctx.primin()->getch());
  if(is_T(file))
    return mknumber(ctx.stdin()->getch());
  check(file, type::File);
  return mknumber(file->file()->getch());
}

LISPT read(context& ctx, LISPT file)
{
  if(is_NIL(file))
    return lispread(ctx.primin());
  if(is_T(file))
    return lispread(ctx.stdin());
  check(file, type::File);
  return lispread(file->file());
}

LISPT print(context& ctx, LISPT x, LISPT file)
{
  if(is_NIL(file))
    return io::print(ctx, x, *ctx.primout());
  if(is_T(file))
    return io::print(ctx, x, *ctx.primerr());
  check(file, type::File);
  return io::print(ctx, x, *file->file());
}

bool loadfile(context& ctx, const std::string& lf)
{
  try
  {
    for(auto i: *ctx.loadpath())
    {
      std::filesystem::path base{i->getstr()};
      base /= lf;
      if(std::filesystem::exists(base) || std::filesystem::exists(base.replace_extension(".lisp")))
      {
        auto foo = ref_file_t::create(std::make_unique<io::file_source>(base));
        for(auto rval = lispread(foo); type_of(rval) != type::Eof; rval = lispread(foo))
          rval = ctx.e().eval(rval);
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

LISPT load(context& ctx, LISPT f)
{
  check(f, type::String, type::Symbol);
  if(!file::loadfile(ctx, f->getstr()))
    ctx.fatal(error_errc::cant_load, f->getstr());
  return f;
}

LISPT terpri(context& ctx, LISPT file)
{
  if(is_NIL(file))
    return io::terpri(*ctx.primout());
  if(is_T(file))
    return io::terpri(*ctx.primerr());
  check(file, type::File);
  return io::terpri(*file->file());
}

LISPT prin1(context& ctx, LISPT x, LISPT file)
{
  ctx.thisplevel = 0;
  if(is_NIL(file))
    return prin0(x, *ctx.primout(), false);
  if(is_T(file))
    return prin0(x, *ctx.primerr(), false);
  check(file, type::File);
  return prin0(x, *file->file(), false);
}

LISPT prin2(context& ctx, LISPT x, LISPT file)
{
  ctx.thisplevel = 0;
  if(is_NIL(file))
    return prin0(x, *ctx.primout(), true);
  if(is_T(file))
    return prin0(x, *ctx.primerr(), true);
  check(file, type::File);
  return prin0(x, *file->file(), true);
}

LISPT printlevel(context& ctx, LISPT newl)
{
  auto x = ctx.printlevel;
  if(!is_NIL(newl))
  {
    check(newl, type::Integer);
    ctx.printlevel = newl->intval();
  }
  return mknumber(x);
}

LISPT spaces(context& ctx, LISPT n, LISPT file)
{
  int i = 0;
  ref_file_t f;

  check(n, type::Integer);
  if(is_NIL(file))
    f = ctx.primout();
  else if(is_T(file))
    f = ctx.primerr();
  else
  {
    check(file, type::File);
    f = file->file();
  }
  for(i = n->intval(); i > 0; i--)
    f->putch(' ');
  return NIL;
}

LISPT readline(context& ctx, LISPT file)
{
  if(is_NIL(file))
    return io::readline(ctx.primin());
  if(is_T(file))
    return io::readline(ctx.stdin());
  check(file, type::File);
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

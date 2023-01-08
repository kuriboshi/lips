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

#include <filesystem>

#include "alloc.hh"
#include "check.hh"
#include "file.hh"
#include "io.hh"
#include "iter.hh"
#include "vm.hh"

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
    return io::ratom(context::current().primin());
  if(is_T(file))
    return io::ratom(context::current().stdin());
  check(file, object::type::File);
  return io::ratom(file->file());
}

lisp_t readc(lisp_t file)
{
  if(is_nil(file))
    return mknumber(context::current().primin()->getch());
  if(is_T(file))
    return mknumber(context::current().stdin()->getch());
  check(file, object::type::File);
  return mknumber(file->file()->getch());
}

lisp_t read(lisp_t file)
{
  if(is_nil(file))
    return lispread(context::current().primin());
  if(is_T(file))
    return lispread(context::current().stdin());
  check(file, object::type::File);
  return lispread(file->file());
}

lisp_t print(lisp_t x, lisp_t file)
{
  if(is_nil(file))
    return io::print(x, *context::current().primout());
  if(is_T(file))
    return io::print(x, *context::current().primerr());
  check(file, object::type::File);
  return io::print(x, *file->file());
}

bool loadfile(const std::string& lf)
{
  try
  {
    for(auto i: *context::current().loadpath())
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

lisp_t load(lisp_t f)
{
  check(f, object::type::String, object::type::Symbol);
  if(!file::loadfile(f->getstr()))
    fatal(error_errc::cant_load, f->getstr());
  return f;
}

lisp_t terpri(lisp_t file)
{
  if(is_nil(file))
    return io::terpri(*context::current().primout());
  if(is_T(file))
    return io::terpri(*context::current().primerr());
  check(file, object::type::File);
  return io::terpri(*file->file());
}

lisp_t prin1(lisp_t x, lisp_t file)
{
  context::current().thisplevel = 0;
  if(is_nil(file))
    return prin0(x, *context::current().primout(), false);
  if(is_T(file))
    return prin0(x, *context::current().primerr(), false);
  check(file, object::type::File);
  return prin0(x, *file->file(), false);
}

lisp_t prin2(lisp_t x, lisp_t file)
{
  context::current().thisplevel = 0;
  if(is_nil(file))
    return prin0(x, *context::current().primout(), true);
  if(is_T(file))
    return prin0(x, *context::current().primerr(), true);
  check(file, object::type::File);
  return prin0(x, *file->file(), true);
}

lisp_t printlevel(lisp_t newl)
{
  auto x = context::current().printlevel;
  if(!is_nil(newl))
  {
    check(newl, object::type::Integer);
    context::current().printlevel = newl->intval();
  }
  return mknumber(x);
}

lisp_t spaces(lisp_t n, lisp_t file)
{
  check(n, object::type::Integer);
  ref_file_t f;
  if(is_nil(file))
    f = context::current().primout();
  else if(is_T(file))
    f = context::current().primerr();
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
    return io::readline(context::current().primin());
  if(is_T(file))
    return io::readline(context::current().stdin());
  check(file, object::type::File);
  return io::readline(file->file());
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

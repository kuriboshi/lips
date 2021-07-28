/*
 * Lips, lisp shell.
 * Copyright 1988, 2020 Krister Joas
 *
 */

#include "file.hh"
#include "alloc.hh"
#include "eval.hh"

namespace lisp
{
file::file(): base() {}
file::file(lisp& lisp): base(lisp) {}

LISPT file::open(LISPT filename, LISPT mode)
{
  l.check(filename, type::STRING, type::SYMBOL);
  bool readmode = true;
  bool appendmode = false;
  if(!is_NIL(mode))
  {
    l.check(mode, type::SYMBOL);
    if(EQ(mode, C_READ))
      readmode = true;
    else if(EQ(mode, C_WRITE))
      readmode = false;
    else if(EQ(mode, C_APPEND))
    {
      readmode = false;
      appendmode = true;
    }
    else
      return l.error(UNKNOWN_REQUEST, mode);
  }
  auto f = [&]() {
    if(readmode)
      return std::make_unique<file_t>(std::make_unique<file_source>(filename->getstr()));
    return std::make_unique<file_t>(std::make_unique<file_sink>(filename->getstr(), appendmode));
  }();
  if(!f)
    return l.error(CANT_OPEN, filename);
  LISPT newfile = getobject(l);
  newfile->set(std::move(f));
  return newfile;
}

LISPT file::close(LISPT fildes)
{
  l.check(fildes, type::FILET);
  fildes->fileval().flush();
  if(fildes->fileval().close())
    return T;
  return NIL;
}

LISPT file::ratom(LISPT file)
{
  if(is_NIL(file))
    return ::lisp::ratom(l, l.primin());
  if(is_T(file))
    return ::lisp::ratom(l, l.stdin());
  l.check(file, type::FILET);
  return ::lisp::ratom(l, file->fileval());
}

LISPT file::readc(LISPT file)
{
  if(is_NIL(file))
    return a.mknumber(l.primin().getch());
  if(is_T(file))
    return a.mknumber(l.stdin().getch());
  l.check(file, type::FILET);
  return a.mknumber(file->fileval().getch());
}

LISPT file::read(LISPT file)
{
  if(is_NIL(file))
    return lispread(l, l.primin(), false);
  if(is_T(file))
    return lispread(l, l.stdin(), false);
  l.check(file, type::FILET);
  return lispread(l, file->fileval(), false);
}

LISPT file::print(LISPT x, LISPT file)
{
  if(is_NIL(file))
    return ::lisp::print(l, x, l.primout());
  if(is_T(file))
    return ::lisp::print(l, x, l.primerr());
  l.check(file, type::FILET);
  return ::lisp::print(l, x, file->fileval());
}

bool file::loadfile(const std::string& lf)
{
  try
  {
    auto foo = std::make_unique<file_t>(std::make_unique<file_source>(lf));
    for(auto rval = lispread(l, *foo.get(), false); type_of(rval) != type::ENDOFFILE; rval = lispread(l, *foo.get(), false))
      rval = e.eval(rval);
  }
  catch(const lisp_error&)
  {
    return false;
  }
  return true;
}

LISPT file::load(LISPT f)
{
  l.check(f, type::STRING, type::SYMBOL);
  if(!loadfile(f->getstr()))
    l.fatal(CANT_LOAD);
  return f;
}

LISPT file::terpri(LISPT file)
{
  if(is_NIL(file))
    return ::lisp::terpri(l, l.primout());
  if(is_T(file))
    return ::lisp::terpri(l, l.primerr());
  l.check(file, type::FILET);
  return ::lisp::terpri(l, file->fileval());
}

LISPT file::prin1(LISPT x, LISPT file)
{
  l.thisplevel = 0;
  if(is_NIL(file))
    return prin0(l, x, l.primout(), false);
  if(is_T(file))
    return prin0(l, x, l.primerr(), false);
  l.check(file, type::FILET);
  return prin0(l, x, file->fileval(), false);
}

LISPT file::prin2(LISPT x, LISPT file)
{
  l.thisplevel = 0;
  if(is_NIL(file))
    return prin0(l, x, l.primout(), true);
  if(is_T(file))
    return prin0(l, x, l.primerr(), true);
  l.check(file, type::FILET);
  return prin0(l, x, file->fileval(), true);
}

LISPT file::plevel(LISPT newl)
{
  auto x = l.printlevel;
  if(!is_NIL(newl))
  {
    l.check(newl, type::INTEGER);
    l.printlevel = newl->intval();
  }
  return a.mknumber(x);
}

LISPT file::spaces(LISPT n, LISPT file)
{
  int i;
  file_t* f;

  l.check(n, type::INTEGER);
  if(is_NIL(file))
    f = &l.primout();
  else if(is_T(file))
    f = &l.primerr();
  else
  {
    l.check(file, type::FILET);
    f = &file->fileval();
  }
  for(i = n->intval(); i > 0; i--) f->putch(' ');
  return NIL;
}

LISPT file::readline(LISPT file)
{
  if(is_NIL(file))
    return ::lisp::readline(l, l.primin());
  else if(is_T(file))
    return ::lisp::readline(l, l.stdin());
  l.check(file, type::FILET);
  return ::lisp::readline(l, file->fileval());
}

namespace pn
{
inline constexpr auto OPEN = "open";         // open file
inline constexpr auto CLOSE = "close";       // close file
inline constexpr auto LOAD = "load";         // load file
inline constexpr auto PRIN1 = "prin1";       // print without escapes
inline constexpr auto PRIN2 = "prin2";       // print without new-line
inline constexpr auto PRINT = "print";       // print
inline constexpr auto PLEVEL = "printlevel"; // how deep to print
inline constexpr auto RATOM = "ratom";       // read atom
inline constexpr auto READ = "read";         // read expression
inline constexpr auto READC = "readc";       // read characte
inline constexpr auto READLINE = "readline"; // read a line
inline constexpr auto SPACES = "spaces";     // print some spaces
inline constexpr auto TERPRI = "terpri";     // print new-line
} // namespace pn

LISPT C_READ;

void file::init()
{
  C_READ = intern(pn::READ);

  // clang-format off
  mkprim(pn::CLOSE,    ::lisp::close,     subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::OPEN,     ::lisp::open,      subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::LOAD,     ::lisp::load,      subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::PRIN1,    ::lisp::prin1,     subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::PRIN2,    ::lisp::prin2,     subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::PRINT,    ::lisp::print,     subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::PLEVEL,   ::lisp::plevel,    subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::RATOM,    ::lisp::ratom,     subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::READ,     ::lisp::read,      subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::READC,    ::lisp::readc,     subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::READLINE, ::lisp::readline,  subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::SPACES,   ::lisp::spaces,    subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::TERPRI,   ::lisp::terpri,    subr_t::subr::EVAL, subr_t::spread::SPREAD);
  // clang-format on
}

} // namespace lisp

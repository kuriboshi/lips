//
// Lips, lisp shell.
// Copyright 1988, 2020-2022 Krister Joas
//

#include "file.hh"
#include "alloc.hh"
#include "eval.hh"

namespace lisp::file
{
LISPT open(lisp& l, LISPT filename, LISPT mode)
{
  check(filename, type::STRING, type::SYMBOL);
  bool readmode = true;
  bool appendmode = false;
  if(!is_NIL(mode))
  {
    check(mode, type::SYMBOL);
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
      return std::make_unique<file_t>(std::make_unique<io::file_source>(filename->getstr()));
    return std::make_unique<file_t>(std::make_unique<io::file_sink>(filename->getstr(), appendmode));
  }();
  if(!f)
    return l.error(CANT_OPEN, filename);
  LISPT newfile = getobject(l);
  newfile->set(std::move(f));
  return newfile;
}

LISPT close(lisp& l, LISPT fildes)
{
  check(fildes, type::FILET);
  fildes->fileval().flush();
  if(fildes->fileval().close())
    return T;
  return NIL;
}

LISPT ratom(lisp& l, LISPT file)
{
  if(is_NIL(file))
    return io::ratom(l, l.primin());
  if(is_T(file))
    return io::ratom(l, l.stdin());
  check(file, type::FILET);
  return io::ratom(l, file->fileval());
}

LISPT readc(lisp& l, LISPT file)
{
  if(is_NIL(file))
    return l.a().mknumber(l.primin().getch());
  if(is_T(file))
    return l.a().mknumber(l.stdin().getch());
  check(file, type::FILET);
  return l.a().mknumber(file->fileval().getch());
}

LISPT read(lisp& l, LISPT file)
{
  if(is_NIL(file))
    return lispread(l, l.primin());
  if(is_T(file))
    return lispread(l, l.stdin());
  check(file, type::FILET);
  return lispread(l, file->fileval());
}

LISPT print(lisp& l, LISPT x, LISPT file)
{
  if(is_NIL(file))
    return io::print(l, x, l.primout());
  if(is_T(file))
    return io::print(l, x, l.primerr());
  check(file, type::FILET);
  return io::print(l, x, file->fileval());
}

bool loadfile(lisp& l, const std::string& lf)
{
  try
  {
    auto foo = std::make_unique<file_t>(std::make_unique<io::file_source>(lf));
    for(auto rval = lispread(l, *foo.get()); type_of(rval) != type::EMPTY;
        rval = lispread(l, *foo.get()))
      rval = l.e().eval(rval);
  }
  catch(const lisp_error&)
  {
    return false;
  }
  return true;
}

LISPT load(lisp& l, LISPT f)
{
  check(f, type::STRING, type::SYMBOL);
  if(!file::loadfile(l, f->getstr()))
    l.fatal(CANT_LOAD);
  return f;
}

LISPT terpri(lisp& l, LISPT file)
{
  if(is_NIL(file))
    return io::terpri(l, l.primout());
  if(is_T(file))
    return io::terpri(l, l.primerr());
  check(file, type::FILET);
  return io::terpri(l, file->fileval());
}

LISPT prin1(lisp& l, LISPT x, LISPT file)
{
  l.thisplevel = 0;
  if(is_NIL(file))
    return prin0(l, x, l.primout(), false);
  if(is_T(file))
    return prin0(l, x, l.primerr(), false);
  check(file, type::FILET);
  return prin0(l, x, file->fileval(), false);
}

LISPT prin2(lisp& l, LISPT x, LISPT file)
{
  l.thisplevel = 0;
  if(is_NIL(file))
    return prin0(l, x, l.primout(), true);
  if(is_T(file))
    return prin0(l, x, l.primerr(), true);
  check(file, type::FILET);
  return prin0(l, x, file->fileval(), true);
}

LISPT plevel(lisp& l, LISPT newl)
{
  auto x = l.printlevel;
  if(!is_NIL(newl))
  {
    check(newl, type::INTEGER);
    l.printlevel = newl->intval();
  }
  return l.a().mknumber(x);
}

LISPT spaces(lisp& l, LISPT n, LISPT file)
{
  int i;
  file_t* f;

  check(n, type::INTEGER);
  if(is_NIL(file))
    f = &l.primout();
  else if(is_T(file))
    f = &l.primerr();
  else
  {
    check(file, type::FILET);
    f = &file->fileval();
  }
  for(i = n->intval(); i > 0; i--) f->putch(' ');
  return NIL;
}

LISPT readline(lisp& l, LISPT file)
{
  if(is_NIL(file))
    return io::readline(l, l.primin());
  else if(is_T(file))
    return io::readline(l, l.stdin());
  check(file, type::FILET);
  return io::readline(l, file->fileval());
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

void init()
{
  C_READ = intern(pn::READ);

  // clang-format off
  mkprim(pn::CLOSE,    close,     subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::OPEN,     open,      subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::LOAD,     load,      subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::PRIN1,    prin1,     subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::PRIN2,    prin2,     subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::PRINT,    print,     subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::PLEVEL,   plevel,    subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::RATOM,    ratom,     subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::READ,     read,      subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::READC,    readc,     subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::READLINE, readline,  subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::SPACES,   spaces,    subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::TERPRI,   terpri,    subr_t::subr::EVAL, subr_t::spread::SPREAD);
  // clang-format on
}

}


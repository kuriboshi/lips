/*
 * Lips, lisp shell.
 * Copyright 1988, 2020 Krister Joas
 *
 */

#include "libisp.hh"

namespace lisp
{
PRIMITIVE file::ratom(LISPT file)
{
  if(is_NIL(file))
    return ::lisp::ratom(l, l.primin());
  if(is_T(file))
    return ::lisp::ratom(l, l.stdin());
  l.check(file, FILET);
  return ::lisp::ratom(l, file->fileval());
}

PRIMITIVE file::readc(LISPT file)
{
  if(is_NIL(file))
    return a.mknumber(l.primin().getch());
  if(is_T(file))
    return a.mknumber(l.stdin().getch());
  l.check(file, FILET);
  return a.mknumber(file->fileval().getch());
}

PRIMITIVE file::read(LISPT file)
{
  if(is_NIL(file))
    return lispread(l, l.primin(), false);
  if(is_T(file))
    return lispread(l, l.stdin(), false);
  l.check(file, FILET);
  return lispread(l, file->fileval(), false);
}

PRIMITIVE file::print(LISPT x, LISPT file)
{
  if(is_NIL(file))
    return ::lisp::print(l, x, l.primout());
  if(is_T(file))
    return ::lisp::print(l, x, l.primerr());
  l.check(file, FILET);
  return ::lisp::print(l, x, file->fileval());
}

bool file::loadfile(const std::string& lf)
{
  try
  {
    auto foo = std::make_unique<file_t>(std::make_unique<file_source>(lf));
    for(auto rval = lispread(l, *foo.get(), false); type_of(rval) != ENDOFFILE; rval = lispread(l, *foo.get(), false))
      rval = e.eval(rval);
  }
  catch(const lisp_error&)
  {
    return false;
  }
  return true;
}

PRIMITIVE file::load(LISPT f)
{
  l.check(f, STRING, SYMBOL);
  if(!loadfile(f->getstr()))
    return l.error(CANT_OPEN, f);
  return f;
}

PRIMITIVE file::terpri(LISPT file)
{
  if(is_NIL(file))
    return ::lisp::terpri(l, l.primout());
  if(is_T(file))
    return ::lisp::terpri(l, l.primerr());
  l.check(file, FILET);
  return ::lisp::terpri(l, file->fileval());
}

PRIMITIVE file::prin1(LISPT x, LISPT file)
{
  l.thisplevel = 0;
  if(is_NIL(file))
    return prin0(l, x, l.primout(), false);
  if(is_T(file))
    return prin0(l, x, l.primerr(), false);
  l.check(file, FILET);
  return prin0(l, x, file->fileval(), false);
}

PRIMITIVE file::prin2(LISPT x, LISPT file)
{
  l.thisplevel = 0;
  if(is_NIL(file))
    return prin0(l, x, l.primout(), true);
  if(is_T(file))
    return prin0(l, x, l.primerr(), true);
  l.check(file, FILET);
  return prin0(l, x, file->fileval(), false);
}

PRIMITIVE file::plevel(LISPT newl)
{
  auto x = l.printlevel;
  if(!is_NIL(newl))
  {
    l.check(newl, INTEGER);
    l.printlevel = newl->intval();
  }
  return a.mknumber(x);
}

PRIMITIVE file::spaces(LISPT n, LISPT file)
{
  int i;
  file_t* f;

  l.check(n, INTEGER);
  if(is_NIL(file))
    f = &l.primout();
  else if(is_T(file))
    f = &l.primerr();
  else
  {
    l.check(file, FILET);
    f = &file->fileval();
  }
  for(i = n->intval(); i > 0; i--) f->putch(' ');
  return C_NIL;
}

PRIMITIVE file::readline(LISPT file)
{
  if(is_NIL(file))
    return ::lisp::readline(l, l.primin());
  else if(is_T(file))
    return ::lisp::readline(l, l.stdin());
  l.check(file, FILET);
  return ::lisp::readline(l, file->fileval());
}

PRIMITIVE file::cpprint(LISPT oname, LISPT file)
{
#if defined(LIPSLIB) && defined(TAGSFILE)
  FILE *f, *tagsfile, *cfile;
  char buf[120];
  const char* funn;
  char lname[20], cname[20], fname[20];
  int line, acnt;

  if(is_NIL(file))
    f = primout;
  else if(is_T(file))
    f = primerr;
  else
  {
    check(file, FILET);
    f = file->fileval();
  }
  check(oname, SYMBOL);
  check(oname->symval().value, SUBR, FSUBR);
  funn = oname->symval().pname;
  if((tagsfile = fopen(TAGSFILE, "r")) == nullptr)
    return error(CANT_OPEN, mkstring(_lisp, TAGSFILE));
  while(fgets(buf, 120, tagsfile) != nullptr)
    if(strncmp(buf, funn, strlen(funn)) == 0 && buf[strlen(funn)] == '\t')
    {
      sscanf(buf, "%s %s %[^:]:%d", lname, cname, fname, &line);
      strcpy(buf, LIPSLIB);
      strcat(buf, "/");
      strcat(buf, fname);
      fclose(tagsfile);
      if((cfile = fopen(buf, "r")) == nullptr)
        return error(CANT_OPEN, mkstring(_lisp, buf));
      for(; line > 1; line--) fgets(buf, 120, cfile);
      fgets(buf, 120, cfile);
      putch('(', f, 0);
      prin2(oname, file);
      putch(' ', f, 0);
      putch('(', f, 0);
      if(type_of(oname->symvalue()) == SUBR)
        prin1(C_SUBR, file);
      else
        prin1(C_FSUBR, file);
      putch(' ', f, 0);
      std::size_t i;
      for(i = 0; buf[i] != '(' && i < sizeof(buf); i++)
        ;
      if((acnt = oname->symval().value->subrval().argcount()) == -1)
        i++;
      for(; buf[i] != ')' && i < sizeof(buf); i++)
        if(buf[i] != ',')
          putch(buf[i], f, 0);
        else
        {
          acnt++;
          if(acnt == -1)
          {
            putch(' ', f, 0);
            putch('.', f, 0);
            acnt++;
          }
        }
      if(acnt != -1)
        putch(')', f, 0);
      putch('\n', f, 0);
      while(buf[0] != '{')
      {
        fgets(buf, 120, cfile);
        fgets(buf, 120, cfile);
      }
      while(buf[0] != '}')
      {
        fputs(buf, f);
        fgets(buf, 120, cfile);
      }
      putch('}', f, 0);
      putch(')', f, 0);
      putch(')', f, 0);
      putch('\n', f, 0);
      fclose(cfile);
      return oname;
    }
  fclose(tagsfile);
  return error(NOT_PRINTABLE, oname);
#else
  return C_NIL;
#endif
}

file::file(lisp& lisp): base(lisp) {}

void file::init()
{
  // clang-format off
  mkprim(PN_LOAD,     ::lisp::load,      subr_t::S_EVAL, subr_t::S_NOSPREAD);
  mkprim(PN_PRIN1,    ::lisp::prin1,     subr_t::S_EVAL, subr_t::S_NOSPREAD);
  mkprim(PN_PRIN2,    ::lisp::prin2,     subr_t::S_EVAL, subr_t::S_NOSPREAD);
  mkprim(PN_PRINT,    ::lisp::print,     subr_t::S_EVAL, subr_t::S_NOSPREAD);
  mkprim(PN_PLEVEL,   ::lisp::plevel,    subr_t::S_EVAL, subr_t::S_NOSPREAD);
  mkprim(PN_RATOM,    ::lisp::ratom,     subr_t::S_EVAL, subr_t::S_NOSPREAD);
  mkprim(PN_READ,     ::lisp::read,      subr_t::S_EVAL, subr_t::S_NOSPREAD);
  mkprim(PN_READC,    ::lisp::readc,     subr_t::S_EVAL, subr_t::S_NOSPREAD);
  mkprim(PN_READLINE, ::lisp::readline,  subr_t::S_EVAL, subr_t::S_NOSPREAD);
  mkprim(PN_SPACES,   ::lisp::spaces,    subr_t::S_EVAL, subr_t::S_NOSPREAD);
  mkprim(PN_TERPRI,   ::lisp::terpri,    subr_t::S_EVAL, subr_t::S_NOSPREAD);
  mkprim(PN_CPPRINT,  ::lisp::cpprint,   subr_t::S_EVAL, subr_t::S_NOSPREAD);
  // clang-format on
}

} // namespace lisp

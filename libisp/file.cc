/*
 * Lips, lisp shell.
 * Copyright 1988, 2020 Krister Joas
 *
 */

#include "libisp.hh"

namespace lisp
{
PRIMITIVE file::xratom(LISPT file)
{
  if(is_NIL(file))
    return ratom(l, l.primin());
  if(is_T(file))
    return ratom(l, l.stdin());
  l.check(file, FILET);
  return ratom(l, *file->fileval());
}

PRIMITIVE file::readc(LISPT file)
{
  if(is_NIL(file))
    return a.mknumber(l.primin().source->getch());
  if(is_T(file))
    return a.mknumber(l.stdin().source->getch());
  l.check(file, FILET);
  return a.mknumber(file->fileval()->source->getch());
}

PRIMITIVE file::xread(LISPT file)
{
  if(is_NIL(file))
    return lispread(l, l.primin(), false);
  if(is_T(file))
    return lispread(l, l.stdin(), false);
  l.check(file, FILET);
  return lispread(l, *file->fileval(), false);
}

PRIMITIVE file::xprint(LISPT x, LISPT file)
{
  if(is_NIL(file))
    return print(l, x, l.primout());
  if(is_T(file))
    return print(l, x, l.primerr());
  l.check(file, FILET);
  return print(l, x, *file->fileval());
}

bool file::loadfile(const char* lf)
{
  try
  {
    auto foo = std::make_unique<file_t>(new io::filesource(lf));
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
  l.check2(f, STRING, SYMBOL);
  if(!loadfile(f->getstr()))
    return l.error(CANT_OPEN, f);
  return f;
}

PRIMITIVE file::xterpri(LISPT file)
{
  if(is_NIL(file))
    return terpri(l, l.primout());
  if(is_T(file))
    return terpri(l, l.primerr());
  l.check(file, FILET);
  return terpri(l, *file->fileval());
}

PRIMITIVE file::prin1(LISPT x, LISPT file)
{
  l.thisplevel = 0;
  if(is_NIL(file))
    return prin0(l, x, l.primout(), false);
  if(is_T(file))
    return prin0(l, x, l.primerr(), false);
  l.check(file, FILET);
  return prin0(l, x, *file->fileval(), false);
}

PRIMITIVE file::prin2(LISPT x, LISPT file)
{
  l.thisplevel = 0;
  if(is_NIL(file))
    return prin0(l, x, l.primout(), true);
  if(is_T(file))
    return prin0(l, x, l.primerr(), true);
  l.check(file, FILET);
  return prin0(l, x, *file->fileval(), false);
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
    f = file->fileval();
  }
  for(i = n->intval(); i > 0; i--) f->sink->putch(' ');
  return C_NIL;
}

PRIMITIVE file::xreadline(LISPT file)
{
  if(is_NIL(file))
    return readline(l, l.primin());
  else if(is_T(file))
    return readline(l, l.stdin());
  l.check(file, FILET);
  return readline(l, *file->fileval());
}

PRIMITIVE file::cpprint(LISPT oname, LISPT file)
{
#if 0
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
  check2(oname->symval().value, SUBR, FSUBR);
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
      if((acnt = oname->symval().value->subrval().argcount) == -1)
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
  alloc::mkprim(PN_LOAD, ::lisp::load, 1, SUBR);
  alloc::mkprim(PN_PRIN1, ::lisp::prin1, 2, SUBR);
  alloc::mkprim(PN_PRIN2, ::lisp::prin2, 2, SUBR);
  alloc::mkprim(PN_PRINT, ::lisp::xprint, 2, SUBR);
  alloc::mkprim(PN_PLEVEL, ::lisp::plevel, 1, SUBR);
  alloc::mkprim(PN_RATOM, ::lisp::xratom, 1, SUBR);
  alloc::mkprim(PN_READ, ::lisp::xread, 1, SUBR);
  alloc::mkprim(PN_READC, ::lisp::readc, 1, SUBR);
  alloc::mkprim(PN_READLINE, ::lisp::xreadline, 1, SUBR);
  alloc::mkprim(PN_SPACES, ::lisp::spaces, 2, SUBR);
  alloc::mkprim(PN_TERPRI, ::lisp::xterpri, 1, SUBR);
  alloc::mkprim(PN_CPPRINT, ::lisp::cpprint, 2, SUBR);
}

} // namespace lisp

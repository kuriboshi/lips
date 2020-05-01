/*
 * Lips, lisp shell.
 * Copyright 1988, 2020 Krister Joas
 *
 */

#include "libisp.hh"

extern int getch(FILE*);
extern void putch(int, FILE*, int);

FILE* primin;
FILE* primout;
FILE* primerr;

namespace lisp
{
PRIMITIVE file::xratom(LISPT file)
{
#if 0
  if(is_NIL(file))
    return ratom(primin);
  if(is_T(file))
    return ratom(stdin);
  check(file, FILET);
  return ratom(file->fileval());
#else
  return C_NIL;
#endif
}

PRIMITIVE file::readc(LISPT file)
{
#if 0
  if(is_NIL(file))
    return mknumber(_lisp, getch(primin));
  if(is_T(file))
    return mknumber(_lisp, getch(stdin));
  check(file, FILET);
  return mknumber(_lisp, getch(file->fileval()));
#else
  return C_NIL;
#endif
}

PRIMITIVE file::xread(LISPT file)
{
#if 0
  if(is_NIL(file))
    return lispread(primin, 0);
  if(is_T(file))
    return lispread(stdin, 0);
  check(file, FILET);
  return lispread(file->fileval(), 0);
#else
  return C_NIL;
#endif
}

PRIMITIVE file::xprint(LISPT x, LISPT file)
{
#if 0
  if(is_NIL(file))
    return print(x, primout);
  if(is_T(file))
    return print(x, primerr);
  check(file, FILET);
  return print(x, file->fileval());
#else
  return C_NIL;
#endif
}

bool file::loadfile(const char* lf)
{
#if 0
  auto* foo = fopen(lf, "r");
  if(foo == nullptr)
    return false;
  for(auto rval = lispread(foo, 0); type_of(rval) != ENDOFFILE; rval = lispread(foo, 0))
  {
    rval = _lisp.e().eval(_lisp, rval);
  }
  fclose(foo);
  return true;
#else
  return false;
#endif
}

PRIMITIVE file::load(LISPT f)
{
#if 0
  check2(f, STRING, SYMBOL);
  if(!loadfile(f->getstr()))
    return error(CANT_OPEN, f);
  return f;
#else
  return C_NIL;
#endif
}

PRIMITIVE file::xterpri(LISPT file)
{
#if 0
  if(is_NIL(file))
    return terpri(primout);
  if(is_T(file))
    return terpri(primerr);
  check(file, FILET);
  return terpri(file->fileval());
#else
  return C_NIL;
#endif
}

PRIMITIVE file::prin1(LISPT x, LISPT file)
{
#if 0
  thisplevel = 0;
  if(is_NIL(file))
    return prin0(x, primout, 0);
  if(is_T(file))
    return prin0(x, primerr, 0);
  check(file, FILET);
  return prin0(x, file->fileval(), 0);
#else
  return C_NIL;
#endif
}

PRIMITIVE file::prin2(LISPT x, LISPT file)
{
#if 0
  thisplevel = 0;
  if(is_NIL(file))
    return prin0(x, primout, 1);
  if(is_T(file))
    return prin0(x, primerr, 1);
  check(file, FILET);
  return prin0(x, file->fileval(), 0);
#else
  return C_NIL;
#endif
}

PRIMITIVE file::plevel(LISPT newl)
{
#if 0
  auto x = printlevel;
  if(!is_NIL(newl))
  {
    check(newl, INTEGER);
    printlevel = newl->intval();
  }
  return mknumber(_lisp, x);
#else
  return C_NIL;
#endif
}

PRIMITIVE file::spaces(LISPT n, LISPT file)
{
#if 0
  int i;
  FILE* f;

  check(n, INTEGER);
  if(is_NIL(file))
    f = primout;
  else if(is_T(file))
    f = primerr;
  else
  {
    check(file, FILET);
    f = file->fileval();
  }
  for(i = n->intval(); i > 0; i--) putc(' ', f);
  return C_NIL;
#else
  return C_NIL;
#endif
}

PRIMITIVE file::xreadline(LISPT file)
{
#if 0
  FILE* f;

  if(is_NIL(file))
    f = primin;
  else if(is_T(file))
    f = stdin;
  else
  {
    check(file, FILET);
    f = file->fileval();
  }
  return readline(f);
#else
  return C_NIL;
#endif
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

file::file(lisp& lisp): base(lisp)
{
  mkprim(PN_LOAD, ::lisp::load, 1, SUBR);
  mkprim(PN_PRIN1, ::lisp::prin1, 2, SUBR);
  mkprim(PN_PRIN2, ::lisp::prin2, 2, SUBR);
  mkprim(PN_PRINT, ::lisp::xprint, 2, SUBR);
  mkprim(PN_PLEVEL, ::lisp::plevel, 1, SUBR);
  mkprim(PN_RATOM, ::lisp::xratom, 1, SUBR);
  mkprim(PN_READ, ::lisp::xread, 1, SUBR);
  mkprim(PN_READC, ::lisp::readc, 1, SUBR);
  mkprim(PN_READLINE, ::lisp::xreadline, 1, SUBR);
  mkprim(PN_SPACES, ::lisp::spaces, 2, SUBR);
  mkprim(PN_TERPRI, ::lisp::xterpri, 1, SUBR);
  mkprim(PN_CPPRINT, ::lisp::cpprint, 2, SUBR);
  primin = stdin;
  primout = stdout;
  primerr = stderr;
}

} // namespace lisp

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
PRIMITIVE xratom(LISPT file)
{
  if(is_NIL(file))
    return ratom(primin);
  if(is_T(file))
    return ratom(stdin);
  check(file, FILET);
  return ratom(file->fileval());
}

PRIMITIVE readc(LISPT file)
{
  if(is_NIL(file))
    return mknumber(getch(primin));
  if(is_T(file))
    return mknumber(getch(stdin));
  check(file, FILET);
  return mknumber(getch(file->fileval()));
}

PRIMITIVE xread(LISPT file)
{
  if(is_NIL(file))
    return lispread(primin, 0);
  if(is_T(file))
    return lispread(stdin, 0);
  check(file, FILET);
  return lispread(file->fileval(), 0);
}

PRIMITIVE xprint(LISPT x, LISPT file)
{
  if(is_NIL(file))
    return print(x, primout);
  if(is_T(file))
    return print(x, primerr);
  check(file, FILET);
  return print(x, file->fileval());
}

bool loadfile(const char* lf)
{
  auto* foo = fopen(lf, "r");
  if(foo == nullptr)
    return false;
  for(auto rval = lispread(foo, 0); type_of(rval) != ENDOFFILE; rval = lispread(foo, 0))
  {
    rval = evaluator::eval(rval);
  }
  fclose(foo);
  return true;
}

PRIMITIVE load(LISPT f)
{
  check2(f, STRING, SYMBOL);
  if(!loadfile(f->getstr()))
    return error(CANT_OPEN, f);
  return f;
}

PRIMITIVE xterpri(LISPT file)
{
  if(is_NIL(file))
    return terpri(primout);
  if(is_T(file))
    return terpri(primerr);
  check(file, FILET);
  return terpri(file->fileval());
}

PRIMITIVE prin1(LISPT x, LISPT file)
{
  thisplevel = 0;
  if(is_NIL(file))
    return prin0(x, primout, 0);
  if(is_T(file))
    return prin0(x, primerr, 0);
  check(file, FILET);
  return prin0(x, file->fileval(), 0);
}

PRIMITIVE prin2(LISPT x, LISPT file)
{
  thisplevel = 0;
  if(is_NIL(file))
    return prin0(x, primout, 1);
  if(is_T(file))
    return prin0(x, primerr, 1);
  check(file, FILET);
  return prin0(x, file->fileval(), 0);
}

PRIMITIVE plevel(LISPT newl)
{
  auto x = printlevel;
  if(!is_NIL(newl))
  {
    check(newl, INTEGER);
    printlevel = newl->intval();
  }
  return mknumber(x);
}

PRIMITIVE spaces(LISPT n, LISPT file)
{
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
}

PRIMITIVE xreadline(LISPT file)
{
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
}

PRIMITIVE cpprint(LISPT oname, LISPT file)
{
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
    return error(CANT_OPEN, mkstring(TAGSFILE));
  while(fgets(buf, 120, tagsfile) != nullptr)
    if(strncmp(buf, funn, strlen(funn)) == 0 && buf[strlen(funn)] == '\t')
    {
      sscanf(buf, "%s %s %[^:]:%d", lname, cname, fname, &line);
      strcpy(buf, LIPSLIB);
      strcat(buf, "/");
      strcat(buf, fname);
      fclose(tagsfile);
      if((cfile = fopen(buf, "r")) == nullptr)
        return error(CANT_OPEN, mkstring(buf));
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
}

file::file()
{
  mkprim(PN_LOAD, load, 1, SUBR);
  mkprim(PN_PRIN1, prin1, 2, SUBR);
  mkprim(PN_PRIN2, prin2, 2, SUBR);
  mkprim(PN_PRINT, xprint, 2, SUBR);
  mkprim(PN_PLEVEL, plevel, 1, SUBR);
  mkprim(PN_RATOM, xratom, 1, SUBR);
  mkprim(PN_READ, xread, 1, SUBR);
  mkprim(PN_READC, readc, 1, SUBR);
  mkprim(PN_READLINE, xreadline, 1, SUBR);
  mkprim(PN_SPACES, spaces, 2, SUBR);
  mkprim(PN_TERPRI, xterpri, 1, SUBR);
  mkprim(PN_CPPRINT, cpprint, 2, SUBR);
  primin = stdin;
  primout = stdout;
  primerr = stderr;
}

} // namespace lisp

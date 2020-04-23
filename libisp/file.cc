/*
 * Lips, lisp shell.
 * Copyright 1988, 2020 Krister Joas
 *
 * $Id$
 *
 */
#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>

#include "libisp.hh"

using namespace lisp;

extern int getch(FILE*);
extern void putch(int, FILE*, int);

FILE* primin;
FILE* primout;
FILE* primerr;

PRIMITIVE xratom(LISPT file)
{
  if(ISNIL(file))
    return ratom(primin);
  if(IST(file))
    return ratom(stdin);
  CHECK(file, FILET);
  return ratom(FILEVAL(file));
}

PRIMITIVE readc(LISPT file)
{
  if(ISNIL(file))
    return mknumber((long)getch(primin));
  if(IST(file))
    return mknumber((long)getch(stdin));
  CHECK(file, FILET);
  return mknumber((long)getch(FILEVAL(file)));
}

PRIMITIVE xread(LISPT file)
{
  if(ISNIL(file))
    return lispread(primin, 0);
  if(IST(file))
    return lispread(stdin, 0);
  CHECK(file, FILET);
  return lispread(FILEVAL(file), 0);
}

PRIMITIVE xprint(LISPT x, LISPT file)
{
  if(ISNIL(file))
    return print(x, primout);
  if(IST(file))
    return print(x, primerr);
  CHECK(file, FILET);
  return print(x, FILEVAL(file));
}

int loadfile(const char* lf)
{
  auto* foo = fopen(lf, "r");
  if(foo == nullptr)
    return 1;
  for(auto rval = lispread(foo, 0); TYPEOF(rval) != ENDOFFILE; rval = lispread(foo, 0))
  {
    rval = eval(rval);
  }
  fclose(foo);
  return 0;
}

PRIMITIVE load(LISPT f)
{
  CHECK2(f, STRING, SYMBOL);
  if(loadfile(GETSTR(f)))
    return error(CANT_OPEN, f);
  else
    return f;
}

PRIMITIVE xterpri(LISPT file)
{
  if(ISNIL(file))
    return terpri(primout);
  if(IST(file))
    return terpri(primerr);
  CHECK(file, FILET);
  return terpri(FILEVAL(file));
}

PRIMITIVE prin1(LISPT x, LISPT file)
{
  thisplevel = 0;
  if(ISNIL(file))
    return prin0(x, primout, 0);
  if(IST(file))
    return prin0(x, primerr, 0);
  CHECK(file, FILET);
  return prin0(x, FILEVAL(file), 0);
}

PRIMITIVE prin2(LISPT x, LISPT file)
{
  thisplevel = 0;
  if(ISNIL(file))
    return prin0(x, primout, 1);
  if(IST(file))
    return prin0(x, primerr, 1);
  CHECK(file, FILET);
  return prin0(x, FILEVAL(file), 0);
}

PRIMITIVE plevel(LISPT newl)
{
  auto x = printlevel;
  if(!ISNIL(newl))
  {
    CHECK(newl, INTEGER);
    printlevel = INTVAL(newl);
  }
  return mknumber(x);
}

PRIMITIVE spaces(LISPT n, LISPT file)
{
  int i;
  FILE* f;

  CHECK(n, INTEGER);
  if(ISNIL(file))
    f = primout;
  else if(IST(file))
    f = primerr;
  else
  {
    CHECK(file, FILET);
    f = FILEVAL(file);
  }
  for(i = INTVAL(n); i > 0; i--) putc(' ', f);
  return C_NIL;
}

PRIMITIVE xreadline(LISPT file)
{
  FILE* f;

  if(ISNIL(file))
    f = primin;
  else if(IST(file))
    f = stdin;
  else
  {
    CHECK(file, FILET);
    f = FILEVAL(file);
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

  if(ISNIL(file))
    f = primout;
  else if(IST(file))
    f = primerr;
  else
  {
    CHECK(file, FILET);
    f = FILEVAL(file);
  }
  CHECK(oname, SYMBOL);
  CHECK2(SYMVAL(oname).value, SUBR, FSUBR);
  funn = SYMVAL(oname).pname;
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
      if(TYPEOF(SYMVALUE(oname)) == SUBR)
        prin1(C_SUBR, file);
      else
        prin1(C_FSUBR, file);
      putch(' ', f, 0);
      std::size_t i;
      for(i = 0; buf[i] != '(' && i < sizeof(buf); i++)
        ;
      if((acnt = SUBRVAL(SYMVAL(oname).value).argcount) == -1)
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

void init_file()
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

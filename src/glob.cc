/*
 * Lips, lisp shell.
 * Copyright 1988, 2020 Krister Joas
 *
 * $Id$
 *
 */
#include <sys/param.h>
#include <sys/stat.h>
#include <dirent.h>
#include <pwd.h>
#include <string.h>
#include <stdlib.h>

#include <libisp.hh>
#include "main.hh"
#include "exec.hh"
#include "glob.hh"

using namespace lisp;

#define TICKS 64
#define NAMELEN 1024

static char r[MAXPATHLEN];
static char** globarr;
static char** globp;
static char** globlimit;

/*
 * If *wild is a slash then str must be a directory to match
 * wild completely. Used by match.
 */
static int dircheck(const char* str, const char* wild, const char* ss)
{
  struct stat sbuf;
  int pos;

  if(*wild == '/')
  {
    if(*str)
      return 0;
    pos = strlen(r);
    if(pos != 0)
      strcat(r, "/");
    strcat(r, ss);
    stat(r, &sbuf);
    r[pos] = '\0';
    if(sbuf.st_mode & S_IFDIR)
      return 1;
    else
      return 0;
  }
  while(*wild == '*') wild++;
  if(*str || *wild)
    return 0;
  else
    return 1;
}

/*
 * Returns 1 if s matches wildcard pattern in w, 0 otherwise. Str
 * is a simple string with no slashes.
 */
static int match(const char* str, const char* wild)
{
  int ok;
  const char* ss = str;

  while(*wild && *str)
  {
    switch(*wild)
    {
      case '*':
        wild++;
        while(*str)
          if(match(str, wild))
            return 1;
          else
            str++;
        return dircheck(str, wild, ss);
      case '?':
        break;
      case '[':
        ok = 0;
        while(*wild && *wild != ']')
        {
          if(*wild == *str)
            ok = 1;
          wild++;
        }
        if(!ok && *wild)
          return 0;
        break;
      case '\\':
        wild++;
        /* fall through */
      default:
        if(*str != *wild)
          return 0;
        break;
    }
    str++;
    wild++;
  }
  return dircheck(str, wild, ss);
}

#if 0
/*
 * Inserts element what in list where keeping alphabetic order.
 */
static LISPT orderinsert(LISPT what, LISPT where)
{
  LISPT p1, p2;

  p1 = C_NIL;
  p2 = where;
  while (!ISNIL(p2))
    {
      if (strcmp(GETSTR(CAR(p2)), GETSTR(what)) > 0)
        {
          if (!ISNIL(p1))
            {
              rplacd(p1, cons(what, C_NIL));
              rplacd(CDR(p1), p2);
            }
          else
            where = cons(what, where);
          break;
        }
      p1 = p2;
      p2 = CDR(p2);
    }
  if (ISNIL(where))
    where = cons(what, C_NIL);
  else if (ISNIL(p2))
    rplacd(p1, cons(what, C_NIL));
  return where;
}
#endif

/*
 * Expands tilde character in first position to home directory or
 * other users home directory.
 */
const char* extilde(const char* w, int rep)
{
  struct passwd* pw;
  static char s[NAMELEN];

  if(*w != '~')
    return w;
  w++;
  if(*w == '/' || !*w)
    strcpy(s, GETSTR(home));
  else
  {
    if(index(w, '/') == NULL)
    {
      pw = getpwnam(w);
      strcpy(s, w);
      w = "";
    }
    else
    {
      int i;

      for(i = 0; *w != '/'; i++) s[i] = *w++;
      s[i] = '\0';
      pw = getpwnam(s);
    }
    if(pw == NULL)
    {
      if(rep)
        error(NO_USER, mkstring(s));
      return NULL;
    }
    strncpy(s, pw->pw_dir, MAXNAMLEN);
  }
  strcat(s, w);
  return s;
}

/* 
 * walkfiles - walks through files as specified by WILD and builds an 
 *             unsorted array of character strings. Returns non-zero if
 *             any file matched the pattern, zero otherwise.
 */
static int walkfiles(const char* wild, int all, int report)
{
  int result;
  int pos;
  struct dirent* rdir;
  DIR* odir;
  const char* sw;
  const char* w;

  if(*wild == '/')
    w = wild + 1;
  else
    w = wild;
  if((odir = opendir(*r == '\0' ? "." : r)) == NULL)
  {
    if(report)
      error(NO_DIRECTORY, mkstring(r));
    return 0;
  }
  while((rdir = readdir(odir)) != NULL)
  {
    if((all || rdir->d_name[0] != '.' || *w == '.') && match(rdir->d_name, w))
    {
      result = 1;
      pos = strlen(r);
      if(pos != 0 && r[pos - 1] != '/')
        strcat(r, "/");
      strcat(r, rdir->d_name);
      for(sw = w; *sw && *sw != '/'; sw++)
        ;
      if(*sw && *(++sw))
        result = walkfiles(sw, all, 0);
      else
      {
        *globp = strsave(r);
        if(globp == globlimit)
        {
          int foo;

          foo = globp - globarr;
          globarr = (char**)realloc(globarr, (foo + TICKS) * sizeof(char*));
          globlimit = globarr + foo + TICKS;
          globp = globarr + foo;
        }
        globp++;
      }
      r[pos] = '\0';
    }
  }
  closedir(odir);
  if(!result && report)
    error(NO_MATCH, mkstring(wild));
  return result;
}

static LISPT buildlist()
{
  char** r;
  LISPT l;

  l = C_NIL;
  for(r = globarr; r < globp; r++)
  {
    l = cons(mkstring(*r), l);
    free(*r);
  }
  free(globarr);
  return l;
}

static int comp(const void* a, const void* b)
{
  /* Reverse sort. */
  return -strcmp(*(char**)a, *(char**)b);
}

LISPT expandfiles(const char* wild, int all, int report, int sort)
{
  if(*wild == '/' && *(wild + 1) == '\0')
    return cons(mkstring(wild), C_NIL);
  if(*wild == '/')
    strcpy(r, "/");
  else
    strcpy(r, "");
  globarr = (char**)malloc(TICKS * sizeof(char*));
  globp = globarr;
  globlimit = globarr + TICKS;
  if(!walkfiles(wild, all, report))
    return C_ERROR;
  if(!ISNIL(globsort) || sort)
    qsort((char*)globarr, globp - globarr, sizeof(char*), comp);
  return buildlist();
}

/*
 * Lisp function expand. Expand all files matching wild
 * in directory dir.
 */
PRIMITIVE expand(LISPT wild, LISPT rep, LISPT all)
{
  const char* wstr;
  int r = 0;

  if(ISNIL(rep))
    r = 1;
  CHECK2(wild, STRING, SYMBOL);
  wstr = extilde(GETSTR(wild), r);
  if(wstr == NULL)
    return C_NIL;
  return expandfiles(wstr, ISNIL(all) ? 0 : 1, r, 0);
}

LISPT glob(LISPT wild)
{
  return expand(wild, 0, 0);
}

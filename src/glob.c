/*
 * Lips, lisp shell.
 * Copyright 1988, 2020 Krister Joas
 *
 * $Id$
 *
 */
#include <sys/types.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <sys/dir.h>
#include <pwd.h>
#include <string.h>
#include <stdlib.h>
#include "lips.h"

#define TICKS		64
#define NAMELEN		1024

#ifndef lint
static char rcsid[] = "$Id$";
#endif

extern struct passwd *getpwnam();
extern char *getwd();
extern char *index();

static char r[MAXPATHLEN];
static char **globarr;
static char **globp;
static char **globlimit;

/*
 * If *wild is a slash then str must be a directory to match
 * wild completely. Used by match.
 */
static int
dircheck(str, wild, ss)
  char *str, *wild, *ss;
{
  struct stat sbuf;
  int pos;

  if (*wild == '/')
    {
      if (*str)  return 0;
      pos = strlen(r);
      if (pos != 0) (void) strcat(r, "/");
      (void) strcat(r, ss);
      (void) stat(r, &sbuf);
      r[pos] = '\0';
      if (sbuf.st_mode & S_IFDIR)  return 1;
      else  return 0;
    }
  while(*wild == '*') wild++;
  if (*str || *wild)
    return 0;
  else  return 1;
}

/*
 * Returns 1 if s matches wildcard pattern in w, 0 otherwise. Str
 * is a simple string with no slashes.
 */
static int
match(str, wild)
  char *str, *wild;
{
  int ok;
  char *ss = str;

  while (*wild && *str)
    {
      switch (*wild)
        {
        case '*':
          wild++;
          while (*str)
            if (match(str, wild))  return 1;
            else  str++;
          return dircheck(str, wild, ss);
        case '?':
          break;
        case '[':
          ok = 0;
          while (*wild && *wild != ']')
            {
              if (*wild == *str)  ok = 1;
              wild++;
            }
          if (!ok && *wild)  return 0;
          break;
	case '\\':
	  wild++;
	  /* fall through */
        default:
          if (*str != *wild)  return 0;
          break;
        }
      str++;
      wild++;
    }
  return dircheck(str, wild, ss);
}

/*
 * Inserts element what in list where keeping alphabetic order.
 */
static LISPT
orderinsert(what, where)
  LISPT what, where;
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
              (void) rplacd(p1, cons(what, C_NIL));
              (void) rplacd(CDR(p1), p2);
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
    (void) rplacd(p1, cons(what, C_NIL));
  return where;
}

/*
 * Expands tilde character in first position to home directory or
 * other users home directory.
 */
char *
extilde(w, rep)
  char *w;
  int rep;
{
  struct passwd *pw;
  static char s[NAMELEN];

  if (*w != '~') return w;
  w++;
  if (*w == '/' || !*w)
    (void) strcpy(s, GETSTR(home));
  else
    {
      if (index(w, '/') == NULL)
        {
          pw = getpwnam(w);
          (void) strcpy(s, w);
          w = "";
        }
      else
        {
          int i;

          for (i = 0; *w != '/'; i++)
            s[i] = *w++;
          s[i] = '\0';
          pw = getpwnam(s);
        }
      if (pw == NULL)
        {
          if (rep) (void) error(NO_USER, mkstring(s));
          return NULL;
        }
      (void) strncpy(s, pw->pw_dir, MAXNAMLEN);
    }
  (void) strcat(s, w);
  return s;
}

/* 
 * walkfiles - walks through files as specified by WILD and builds an 
 *             unsorted array of character strings. Returns non-zero if
 *             any file matched the pattern, zero otherwise.
 */
static int
walkfiles(wild, all, report)
  char *wild;
  int all, report;
{
  int result;
  int pos;
  struct direct *rdir;
  DIR *odir;
  char *sw;
  char *w;

  if (*wild == '/') w = wild + 1;
  else w = wild;
  if ((odir = opendir(r)) == NULL)
    {
      if (report) error(NO_DIRECTORY, mkstring(r));
      return 0;
    }
  while ((rdir = readdir(odir)) != NULL)
    {
      if ((all || rdir->d_name[0] != '.' || *w == '.') &&
          match(rdir->d_name, w))
        {
	  result = 1;
          pos = strlen(r);
          if (pos != 0 && r[pos-1] != '/') (void) strcat(r, "/");
          (void) strcat(r, rdir->d_name);
          for (sw = w; *sw && *sw != '/'; sw++) ;
          if (*sw && *(++sw))
	    result = walkfiles(sw, all, 0);
          else
            {
	      *globp = strsave(r);
	      if (globp == globlimit)
		{
		  int foo;

		  foo = globp - globarr;
		  globarr = (char **) realloc(globarr,
					      (foo + TICKS) * sizeof(char *));
		  globlimit = globarr + foo + TICKS;
		  globp = globarr + foo;
		}
	      globp++;
            }
          r[pos] = '\0';
        }
    }
  closedir(odir);
  if (!result && report)
    (void) error(NO_MATCH, mkstring(wild));
  return result;
}

static LISPT
buildlist()
{
  char **r;
  LISPT l;

  l = C_NIL;
  for (r = globarr; r < globp; r++)
    {
      l = cons(mkstring(*r), l);
      free(*r);
    }
  free(globarr);
  return l;
}

static int
comp(a, b)
  char **a, **b;
{
  /* Reverse sort. */
  return -strcmp(*a, *b);
}

LISPT
expandfiles(wild, all, report, sort)
  char *wild;
  int all, report, sort;
{
  if (*wild == '/' && *(wild + 1) == '\0')
    return cons(mkstring(wild), C_NIL);
  if (*wild == '/')
    (void) strcpy(r, "/");
  else
    (void) strcpy(r, "");
  globarr = (char **) malloc(TICKS * sizeof(char *));
  globp = globarr;
  globlimit = globarr + TICKS;
  if (!walkfiles(wild, all, report)) return C_ERROR;
  if (!ISNIL(globsort) || sort)
    qsort((char *) globarr, globp - globarr, sizeof(char *), comp);
  return buildlist();
}

/*
 * Lisp function expand. Expand all files matching wild
 * in directory dir.
 */
PRIMITIVE expand(wild, rep, all)
  LISPT wild, rep, all;
{
  char *wstr;
  int r = 0;

  if (ISNIL(rep)) r = 1;
  CHECK2(wild, STRING, SYMBOL);
  wstr = extilde(GETSTR(wild), r);
  if (wstr == NULL) return C_NIL;
  return expandfiles(wstr, ISNIL(all) ? 0 : 1, r, 0);
}

LISPT
glob(wild)
  LISPT wild;
{
  return expand(wild, 0, 0);
}

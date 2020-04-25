/*
 * Lips, lisp shell.
 * Copyright 1988, 2020 Krister Joas
 *
 * $Id$
 */
#include <sys/types.h>
#ifdef SELECT
#include <sys/select.h>
#endif
#include <ctype.h>
#include <signal.h>
#include <pwd.h>
#include <unistd.h>
#include <stdlib.h>
#include <errno.h>

#include <libisp.hh>
#include "main.hh"
#include "exec.hh"
#include "top.hh"

using namespace lisp;

#ifndef LIPSRC
#define LIPSRC "/usr/local/lib/lipsrc"
#endif

extern void init_term(void);
extern void end_term(void);
extern void clearlbuf(void);
extern void loadbuf(char*);

extern char* VERSION;

char* progname; /* Name of the game. */
int brkflg;     /* 1 means break at next call to peval1. */
int interrupt;
int mypgrp;             /* lips process group. */
struct options options; /* Structure for all options. */
LISPT path;             /* Search path for executables. */
LISPT home;             /* Home directory. */
LISPT globsort;         /* To sort or not during globbing. */

/* graceful death */
void finish(int stat)
{
  end_term();
  exit(stat);
}

#ifdef FANCY_SIGNALS
static int getuser(FILE* f, int def)
{
#ifdef SELECT
  int c;
  fd_set readfs;
  struct timeval timeout;

  timeout.tv_sec = 10;
  timeout.tv_usec = 0;
  FD_ZERO(&readfs);
  FD_SET(0, &readfs);
  switch(select(FD_SETSIZE, &readfs, nullptr, nullptr, &timeout))
  {
    case -1:
      fprintf(primerr, "(error in select %d) ", errno);
      c = 'n';
      break;
    case 0:
      c = def;
      break;
    default:
      c = getc(f);
      break;
  }
  return c;
#else
  return getc(f);
#endif
}

/*
 * This routine handles signals that produces core dumps.
 * It gives the user an option to continue or to halt and
 * dump a core. If continued there is no garantee anything will
 * work correctly.
 */
void core(int sig)
{
  int c;

  init_term();
  if(insidefork)
  {
    fprintf(primerr, " -- (in fork) core dumped\n");
    killpg(getpgrp(), sig);
  }
  fprintf(primerr, " -- Continue? ");
  fflush(primerr);
  c = getuser(stdin, 'y');
  while('y' != (islower(c) ? c : tolower(c)) && 'n' != (islower(c) ? c : tolower(c))) c = getuser(stdin, 'y');
  if((islower(c) ? c : tolower(c)) == 'n')
  {
    fprintf(primerr, "No\n");
    fprintf(primerr, "Core dump? ");
    fflush(primerr);
    c = getuser(stdin, 'y');
    while('y' != (islower(c) ? c : tolower(c)) && 'n' != (islower(c) ? c : tolower(c))) c = getuser(stdin, 'y');
    if((islower(c) ? c : tolower(c)) == 'n')
    {
      fprintf(primerr, "No\n");
      finish(0);
    }
    else
    {
      signal(sig, SIG_DFL);
      printf("Yes\n");
      end_term();
      killpg(mypgrp, sig);
    }
  }
  else
  {
    fprintf(primerr, "Yes\n");
    fprintf(primerr, "Warning: continued after signal %d.\n", sig);
    fprintf(primerr, "Save your work and exit.\n");
    end_term();
    throw lisp_error("continue after signal");
  }
}
#endif

void onintr(int)
{
  if(insidefork)
    exit(0);
  fprintf(primerr, "^C\n");
  unwind();
  clearlbuf();
  throw lisp_error("onintr");
}

#ifdef FANCY_SIGNALS
void onquit(int)
{
  fprintf(primerr, "Quit!");
  core(SIGQUIT);
}

void onbus(int)
{
  fprintf(primerr, "%s: Bus error!", progname);
  core(SIGBUS);
}

void onsegv(int)
{
  fprintf(primerr, "%s: Segmentation violation!", progname);
  core(SIGSEGV);
}

void onill(int)
{
  fprintf(primerr, "%s: Illegal instruction!", progname);
  core(SIGILL);
}

void onhup()
{
  exit(0);
}
#endif

/*
 * The stop key means to break inside a lisp expression. The
 * brkflg is checked on every entry to eval.
 */
void onstop(int)
{
  brkflg = 1;
}

static void fixpgrp()
{
  mypgrp = getpgrp();
  tcsetpgrp(0, mypgrp);
}

/*
 * Processes the environment variable PATH and returns a list
 * of all directories in PATH.
 */
LISPT mungepath(char* pstr)
{
  char *ps, *s;
  LISPT p;

  ps = realmalloc((unsigned)(strlen(pstr) + 1));
  if(ps == nullptr)
  {
    fprintf(stderr, "No more memory, can't munge path.\n");
    finish(1);
  }
  strcpy(ps, pstr);
  p = C_NIL;
  s = ps + strlen(ps);
  while(s >= ps)
  {
    *s = '\0';
    for(; s >= ps && *s != ':'; s--)
      ;
    p = cons(mkstring(s + 1), p);
  }
  free(ps);
  return p;
}

void onbreak()
{
  if(insidefork)
    exit(1);
}

void promptfun()
{
  tcsetpgrp(0, mypgrp); /* Get control of tty */
  insidefork = 0;
  /*
   * Check for jobs that are finished and print them.
   */
  checkfork();
  printdone();
}

static LISPT put_end(LISPT list, LISPT obj, int conc)
{
  LISPT t;

  if(ISNIL(list))
    if(conc)
      return obj;
    else
      return cons(obj, C_NIL);
  else
    for(t = list; TYPEOF(CDR(t)) == CONS; t = CDR(t))
      ;
  if(conc)
    rplacd(t, obj);
  else
    rplacd(t, cons(obj, C_NIL));
  return list;
}

static LISPT transform(LISPT list)
{
  LISPT tl;
  LISPT res;
  LISPT ll;
  int conc;

  tl = C_NIL;
  res = C_NIL;
  conc = 0;
  for(ll = list; TYPEOF(ll) == CONS; ll = CDR(ll))
  {
    if(TYPEOF(CAR(ll)) == CONS)
      tl = put_end(tl, transform(CAR(ll)), conc);
    else if(EQ(CAR(ll), C_BAR))
    {
      if(ISNIL(res))
        res = cons(C_PIPE, cons(tl, C_NIL));
      else
        res = cons(C_PIPE, cons(put_end(res, tl, conc), C_NIL));
      tl = C_NIL;
      conc = 0;
    }
    else if(EQ(CAR(ll), C_SEMI))
    {
      if(ISNIL(res))
        res = cons(C_PROGN, cons(tl, C_NIL));
      else
        res = cons(C_PROGN, cons(put_end(res, tl, conc), C_NIL));
      tl = C_NIL;
      conc = 0;
    }
    else if(EQ(CAR(ll), C_GT))
    {
      if(ISNIL(res))
        res = cons(C_TO, cons(tl, C_NIL));
      else
        res = cons(C_TO, cons(put_end(res, tl, conc), C_NIL));
      tl = C_NIL;
      conc = 1;
    }
    else if(EQ(CAR(ll), C_GGT))
    {
      if(ISNIL(res))
        res = cons(C_TOTO, cons(tl, C_NIL));
      else
        res = cons(C_TOTO, cons(put_end(res, tl, conc), C_NIL));
      tl = C_NIL;
      conc = 1;
    }
    else if(EQ(CAR(ll), C_LT))
    {
      if(ISNIL(res))
        res = cons(C_FROM, cons(tl, C_NIL));
      else
        res = cons(C_FROM, cons(put_end(res, tl, conc), C_NIL));
      tl = C_NIL;
      conc = 1;
    }
    else if(EQ(CAR(ll), C_AMPER))
    {
      if(ISNIL(res))
        res = cons(C_BACK, cons(tl, C_NIL));
      else
        res = cons(C_BACK, cons(put_end(res, tl, conc), C_NIL));
      tl = C_NIL;
      conc = 1;
    }
    else
      tl = put_end(tl, CAR(ll), 0);
  }
  if(ISNIL(res))
    return tl;
  else if(!ISNIL(tl))
    res = put_end(res, tl, conc);
  return res;
}

static void init()
{
  signal(SIGTTIN, SIG_IGN);
  signal(SIGTTOU, SIG_IGN); /* otherwise can't get ctrl tty back */

  fixpgrp();

  init_lisp();
  init_hist();

  initcvar(&path, "path", mungepath(getenv("PATH")));
  initcvar(&home, "home", mkstring(getenv("HOME")));

  initcvar(&globsort, "globsort", C_T);
  transformhook = transform;
  beforeprompt = promptfun;
  evaluator::breakhook = onbreak;

  init_exec();
}

/*
 * Loads the file INITFILE.
 */
static void loadinit(const char* initfile)
{
  if(loadfile(initfile))
    printf("Can't open file %s\n", initfile); /* System init file. */
}

/*
 * Greet user who, or if who is nil, $USER. This means loading
 * the user's init file, .lipsrc.
 */
LISPT greet(LISPT who)
{
  struct passwd* pws;
  char loadf[256];
  char* s;

  if(ISNIL(who))
    s = getenv("USER");
  else
    s = STRINGVAL(who);
  if(s == nullptr)
    return C_NIL;
  pws = getpwnam(s);
  if(pws == nullptr)
    return C_NIL;
  strcpy(loadf, pws->pw_dir);
  strcat(loadf, "/.lipsrc");
  loadfile(loadf);
  return C_T;
}

/*ARGSUSED*/
int main(int argc, char* const* argv)
{
  int option;

  options.debug = 0;
  options.version = 0;
  options.fast = 0;
  options.interactive = 0;
  options.command = 0;
  while((option = getopt(argc, argv, "c:fvid")) != EOF)
  {
    switch(option)
    {
      case 'c':
        options.command = 1;
        loadbuf(optarg);
        break;
      case 'f':
        options.fast = 1;
        break;
      case 'v':
        options.version = 1;
        break;
      case 'i':
        options.interactive = 1;
        break;
      case 'd':
        options.debug = 1;
        break;
      default:
        fprintf(primerr, "usage: -fvic [arguments]\n");
        exit(1);
        break;
    }
  }
  if(!options.interactive && !options.command)
    options.interactive = isatty(0) ? 1 : 0;
  if(options.version)
    printf("%s\n", VERSION);
  progname = argv[0];

  /*
   * Init shell and lisp interpreter.
   */
  init();
  interactive = options.interactive ? C_T : C_NIL;
  if(!options.debug && options.interactive)
  {
    signal(SIGINT, onintr);
    signal(SIGHUP, SIG_DFL);
    signal(SIGTSTP, onstop);
#ifdef FANCY_SIGNALS
    signal(SIGQUIT, onquit);
    signal(SIGILL, onill);
#ifdef SIGEMT
    signal(SIGEMT, onill);
#endif
    signal(SIGBUS, onbus);
    signal(SIGSEGV, onsegv);
#endif
  }
  if(!options.fast)
  {
    try
    {
      loadinit(LIPSRC);
      greet(C_NIL);
    }
    catch(const lisp_error& error)
    {}
  }
  while(true)
  {
    try
    {
      evaluator::toctrl = 0;
      dzero();
      evaluator::fun = C_NIL;
      evaluator::args = C_NIL;
      evaluator::env = nullptr;
      if(toploop(&topprompt, nullptr))
        break;
    }
    catch(const lisp_error& error)
    {
      printf("error: %s\n", error.what());
    }
  }
  finish(0);
}

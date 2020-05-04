/*
 * Lips, lisp shell.
 * Copyright 1988, 2020 Krister Joas
 *
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
#include <except.hh>
#include "main.hh"
#include "exec.hh"
#include "top.hh"

#ifndef LIPSRC
#define LIPSRC "/usr/local/lib/lipsrc"
#endif

extern void init_term();
extern void end_term();
extern void clearlbuf();

lisp::lisp* L;

using namespace lisp;

char* progname;      /* Name of the game. */
int mypgrp;             /* lips process group. */
options_t options; /* Structure for all options. */
LISPT path;             /* Search path for executables. */
LISPT home;             /* Home directory. */
LISPT globsort;         /* To sort or not during globbing. */

LISPT C_ALIAS;
LISPT C_AMPER;
LISPT C_BACK;
LISPT C_BAR;
LISPT C_EXCL;
LISPT C_EXEC;
LISPT C_FROM;
LISPT C_GGT;
LISPT C_GT;
LISPT C_LT;
LISPT C_OLDVAL;
LISPT C_PIPE;
LISPT C_PROGN;
LISPT C_SEMI;
LISPT C_TO;
LISPT C_TOTO;

/* graceful death */
void finish(int stat)
{
  end_term();
  exit(stat);
}

#ifdef FANCY_SIGNALS
static int getuser(int def)
{
#ifdef SELECT
  fd_set readfs;
  struct timeval timeout;

  timeout.tv_sec = 10;
  timeout.tv_usec = 0;
  FD_ZERO(&readfs);
  FD_SET(0, &readfs);
  switch(select(FD_SETSIZE, &readfs, nullptr, nullptr, &timeout))
  {
    case -1:
      L->primerr().printf("(error in select %d) ", errno);
      return 'n';
      break;
    case 0:
      return def;
      break;
    default:
      return L->primin().getch();
      break;
  }
  return def;
#else
  return L->primin().getch();
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
  init_term();
  if(insidefork)
  {
    L->primerr().printf(" -- (in fork) core dumped\n");
    killpg(getpgrp(), sig);
  }
  L->primerr().printf(" -- Continue? ");
  L->primerr().flush();
  int c = getuser('y');
  while('y' != (islower(c) ? c : tolower(c)) && 'n' != (islower(c) ? c : tolower(c))) c = getuser('y');
  if((islower(c) ? c : tolower(c)) == 'n')
  {
    L->primerr().printf("No\n");
    L->primerr().printf("Core dump? ");
    L->primerr().flush();
    c = getuser('y');
    while('y' != (islower(c) ? c : tolower(c)) && 'n' != (islower(c) ? c : tolower(c))) c = getuser('y');
    if((islower(c) ? c : tolower(c)) == 'n')
    {
      L->primerr().printf("No\n");
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
    L->primerr().printf("Yes\n");
    L->primerr().printf("Warning: continued after signal %d.\n", sig);
    L->primerr().printf("Save your work and exit.\n");
    end_term();
    throw lisp_error("continue after signal");
  }
}
#endif

void onintr(int)
{
  if(insidefork)
    exit(0);
  L->primerr().puts("^C\n");
  L->e().unwind();
  clearlbuf();
  throw lisp_error("onintr");
}

#ifdef FANCY_SIGNALS
void onquit(int)
{
  L->primerr().puts("Quit!");
  core(SIGQUIT);
}

void onbus(int)
{
  L->primerr().printf("%s: Bus error!", progname);
  core(SIGBUS);
}

void onsegv(int)
{
  L->primerr().printf("%s: Segmentation violation!", progname);
  core(SIGSEGV);
}

void onill(int)
{
  L->primerr().printf("%s: Illegal instruction!", progname);
  core(SIGILL);
}

void onhup() { exit(0); }
#endif

/*
 * The stop key means to break inside a lisp expression. The
 * brkflg is checked on every entry to eval.
 */
void onstop(int) { L->brkflg = true; }

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

  ps = L->a().realmalloc((unsigned)(strlen(pstr) + 1));
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
    p = cons(*L, mkstring(*L, s + 1), p);
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
  insidefork = false;
  /*
   * Check for jobs that are finished and print them.
   */
  checkfork();
  printdone();
}

static LISPT put_end(LISPT list, LISPT obj, bool conc)
{
  if(is_NIL(list))
  {
    if(conc)
      return obj;
    return cons(*L, obj, C_NIL);
  }
  LISPT t;
  for(t = list; type_of(t->cdr()) == CONS; t = t->cdr())
    ;
  if(conc)
    rplacd(*L, t, obj);
  else
    rplacd(*L, t, cons(*L, obj, C_NIL));
  return list;
}

static LISPT transform(LISPT list)
{
  LISPT tl = C_NIL;
  LISPT res = C_NIL;
  bool conc = false;
  for(LISPT ll = list; type_of(ll) == CONS; ll = ll->cdr())
  {
    if(type_of(ll->car()) == CONS)
      tl = put_end(tl, transform(ll->car()), conc);
    else if(EQ(ll->car(), C_BAR))
    {
      if(is_NIL(res))
        res = cons(*L, C_PIPE, cons(*L, tl, C_NIL));
      else
        res = cons(*L, C_PIPE, cons(*L, put_end(res, tl, conc), C_NIL));
      tl = C_NIL;
      conc = false;
    }
    else if(EQ(ll->car(), C_SEMI))
    {
      if(is_NIL(res))
        res = cons(*L, C_PROGN, cons(*L, tl, C_NIL));
      else
        res = cons(*L, C_PROGN, cons(*L, put_end(res, tl, conc), C_NIL));
      tl = C_NIL;
      conc = false;
    }
    else if(EQ(ll->car(), C_GT))
    {
      if(is_NIL(res))
        res = cons(*L, C_TO, cons(*L, tl, C_NIL));
      else
        res = cons(*L, C_TO, cons(*L, put_end(res, tl, conc), C_NIL));
      tl = C_NIL;
      conc = true;
    }
    else if(EQ(ll->car(), C_GGT))
    {
      if(is_NIL(res))
        res = cons(*L, C_TOTO, cons(*L, tl, C_NIL));
      else
        res = cons(*L, C_TOTO, cons(*L, put_end(res, tl, conc), C_NIL));
      tl = C_NIL;
      conc = true;
    }
    else if(EQ(ll->car(), C_LT))
    {
      if(is_NIL(res))
        res = cons(*L, C_FROM, cons(*L, tl, C_NIL));
      else
        res = cons(*L, C_FROM, cons(*L, put_end(res, tl, conc), C_NIL));
      tl = C_NIL;
      conc = true;
    }
    else if(EQ(ll->car(), C_AMPER))
    {
      if(is_NIL(res))
        res = cons(*L, C_BACK, cons(*L, tl, C_NIL));
      else
        res = cons(*L, C_BACK, cons(*L, put_end(res, tl, conc), C_NIL));
      tl = C_NIL;
      conc = true;
    }
    else
      tl = put_end(tl, ll->car(), false);
  }
  if(is_NIL(res))
    return tl;
  if(!is_NIL(tl))
    res = put_end(res, tl, conc);
  return res;
}

static void init()
{
  signal(SIGTTIN, SIG_IGN);
  signal(SIGTTOU, SIG_IGN); /* otherwise can't get ctrl tty back */

  fixpgrp();

  L = new ::lisp::lisp();

  C_ALIAS = alloc::intern("alias");
  C_AMPER = alloc::intern("&");
  C_BACK = alloc::intern(PN_BACK);
  C_BAR = alloc::intern("|");
  C_EXCL = alloc::intern("!");
  C_EXEC = alloc::intern(PN_EXEC);
  C_FROM = alloc::intern(PN_FROM);
  C_GGT = alloc::intern(">>");
  C_GT = alloc::intern(">");
  C_LT = alloc::intern("<");
  C_OLDVAL = alloc::intern("oldval");
  C_PIPE = alloc::intern(PN_PIPECMD);
  C_PROGN = alloc::intern(PN_PROGN);
  C_SEMI = alloc::intern(";");
  C_TO = alloc::intern(PN_TO);
  C_TOTO = alloc::intern(PN_TOTO);

  top::init();

  initcvar(&path, "path", mungepath(getenv("PATH")));
  initcvar(&home, "home", mkstring(*L, getenv("HOME")));
  L->a().add_mark_object(&path);
  L->a().add_mark_object(&home);

  initcvar(&globsort, "globsort", C_T);
  transformhook = transform;
  beforeprompt = promptfun;
  L->e().breakhook = onbreak;

  exec::init();
}

/*
 * Loads the file INITFILE.
 */
static void loadinit(const char* initfile)
{
  if(!loadfile(*L, initfile))
    printf("Can't open file %s\n", initfile); /* System init file. */
}

/*
 * Greet user who, or if who is nil, $USER. This means loading
 * the user's init file, .lipsrc.
 */
LISPT greet(LISPT who)
{
  const char* s;
  if(is_NIL(who))
    s = getenv("USER");
  else
    s = who->stringval();
  if(s == nullptr)
    return C_NIL;
  struct passwd* pws = getpwnam(s);
  if(pws == nullptr)
    return C_NIL;
  char loadf[256];
  strcpy(loadf, pws->pw_dir);
  strcat(loadf, "/.lipsrc");
  loadfile(*L, loadf);
  return C_T;
}

int main(int argc, char* const* argv)
{
  options.debug = false;
  options.version = false;
  options.fast = false;
  options.interactive = false;
  options.command = false;
  int option;
  while((option = getopt(argc, argv, "c:fvid")) != EOF)
  {
    switch(option)
    {
      case 'c':
        options.command = true;
        // TODO: Read expression from command line and evaluate
        // loadbuf(optarg);
        break;
      case 'f':
        options.fast = true;
        break;
      case 'v':
        options.version = true;
        break;
      case 'i':
        options.interactive = true;
        break;
      case 'd':
        options.debug = true;
        break;
      default:
        L->primerr().puts("usage: -fvic [arguments]\n");
        exit(1);
        break;
    }
  }
  if(!options.interactive && !options.command)
    options.interactive = isatty(0);
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
      L->e().reset();
      if(toploop(&topprompt, nullptr))
        break;
    }
    catch(const lisp_reset&)
    {}
    catch(const lisp_error& error)
    {
      printf("error: %s\n", error.what());
    }
    catch(const lisp_finish& fin)
    {
      L->stderr().printf("finish: %s", fin.what());
      finish(fin.exit_code);
    }
  }
  finish(0);
}

/*
 * Lips, lisp shell.
 * Copyright 1988, 2020 Krister Joas
 *
 */

#define DOCTEST_CONFIG_IMPLEMENT
#include <doctest/doctest.h>

#include <sys/types.h>
#include <sys/select.h>
#include <pwd.h>
#include <unistd.h>

#include <csetjmp>
#include <csignal>
#include <cstdlib>
#include <cerrno>
#include <cctype>

#include <lisp/libisp.hh>
#include <lisp/except.hh>
#include "main.hh"
#include "env.hh"
#include "exec.hh"
#include "top.hh"
#include "term.hh"

std::jmp_buf jumper;
int mypgrp;     /* lips process group. */
char* progname; /* Name of the game. */

static void fixpgrp()
{
  mypgrp = getpgrp();
  tcsetpgrp(0, mypgrp);
}

static int getuser(int def)
{
  fd_set readfs;
  struct timeval timeout;

  timeout.tv_sec = 10;
  timeout.tv_usec = 0;
  FD_ZERO(&readfs);
  FD_SET(0, &readfs);
  switch(select(FD_SETSIZE, &readfs, nullptr, nullptr, &timeout))
  {
    case -1:
      primerr().format("(error in select {}) ", errno);
      return 'n';
      break;
    case 0:
      return def;
      break;
    default:
      return primin().getch();
      break;
  }
  return def;
}

/*
 * This routine handles signals that produces core dumps.
 * It gives the user an option to continue or to halt and
 * dump a core. If continued there is no garantee anything will
 * work correctly.
 */
void core(int sig)
{
  // init_term();
  if(insidefork)
  {
    primerr().format(" -- (in fork) core dumped\n");
    killpg(getpgrp(), sig);
  }
  primerr().format(" -- Continue? ");
  primerr().flush();
  int c = getuser('y');
  while('y' != (islower(c) ? c : tolower(c)) && 'n' != (islower(c) ? c : tolower(c))) c = getuser('y');
  if((islower(c) ? c : tolower(c)) == 'n')
  {
    primerr().format("No\n");
    primerr().format("Core dump? ");
    primerr().flush();
    c = getuser('y');
    while('y' != (islower(c) ? c : tolower(c)) && 'n' != (islower(c) ? c : tolower(c))) c = getuser('y');
    if((islower(c) ? c : tolower(c)) == 'n')
    {
      primerr().format("No\n");
      throw lisp::lisp_finish("core", 0);
    }
    else
    {
      signal(sig, SIG_DFL);
      std::cout << "Yes\n";
      term_source::end_term();
      killpg(mypgrp, sig);
    }
  }
  else
  {
    primerr().format("Yes\n");
    primerr().format("Warning: continued after signal {}.\n", sig);
    primerr().format("Save your work and exit.\n");
    term_source::end_term();
    throw lisp::lisp_error("continue after signal");
  }
}

void onsignal(int sig)
{
  std::longjmp(jumper, sig);
}

void init_all_signals()
{
  signal(SIGINT, onsignal);
  signal(SIGHUP, SIG_DFL);
  signal(SIGTSTP, onsignal);
  signal(SIGQUIT, onsignal);
  signal(SIGILL, onsignal);
#ifdef SIGEMT
  signal(SIGEMT, onsignal);
#endif
  signal(SIGBUS, onsignal);
  //signal(SIGSEGV, onsignal);
  int sig = setjmp(jumper);
  if(sig == 0)
    return;
  switch(sig)
  {
    case SIGINT:
      if(insidefork)
        exit(0);
      throw lisp::lisp_reset();
    case SIGHUP:
      exit(0);
    case SIGQUIT:
      primerr().format("{}: Quit", progname);
      break;
    case SIGILL:
    case SIGEMT:
      primerr().format("{}: Illegal instruction", progname);
      break;
    case SIGBUS:
      primerr().format("{}: Bus error", progname);
      break;
    case SIGSEGV:
      primerr().format("{}: Segmentation fault", progname);
      break;
    case SIGTSTP:
      primerr().format("{}: Stop", progname);
      lisp::break_flag(true);
      return;
    default:
      break;
  }
  core(sig);
}

using namespace lisp;

LISPT C_ALIAS;
LISPT C_AMPER;
LISPT C_BACK;
LISPT C_BAR;
LISPT C_EXCL;
LISPT C_EXEC;
LISPT C_GGT;
LISPT C_GT;
LISPT C_LT;
LISPT C_OLDVAL;
LISPT C_PIPE;
LISPT C_PROGN;
LISPT C_REDIR_APPEND;
LISPT C_REDIR_FROM;
LISPT C_REDIR_TO;
LISPT C_SEMI;

std::unique_ptr<environment> env;

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
    return cons(obj, NIL);
  }
  LISPT t;
  for(t = list; type_of(t->cdr()) == type::CONS; t = t->cdr())
    ;
  if(conc)
    rplacd(t, obj);
  else
    rplacd(t, cons(obj, NIL));
  return list;
}

static LISPT transform(LISPT list)
{
  LISPT tl = NIL;
  LISPT res = NIL;
  bool conc = false;
  for(LISPT ll = list; type_of(ll) == type::CONS; ll = ll->cdr())
  {
    if(type_of(ll->car()) == type::CONS)
      tl = put_end(tl, transform(ll->car()), conc);
    else if(EQ(ll->car(), C_BAR))
    {
      if(is_NIL(res))
        res = cons(C_PIPE, cons(tl, NIL));
      else
        res = cons(C_PIPE, cons(put_end(res, tl, conc), NIL));
      tl = NIL;
      conc = false;
    }
    else if(EQ(ll->car(), C_SEMI))
    {
      if(is_NIL(res))
        res = cons(C_PROGN, cons(tl, NIL));
      else
        res = cons(C_PROGN, cons(put_end(res, tl, conc), NIL));
      tl = NIL;
      conc = false;
    }
    else if(EQ(ll->car(), C_GT))
    {
      if(is_NIL(res))
        res = cons(C_REDIR_TO, cons(tl, NIL));
      else
        res = cons(C_REDIR_TO, cons(put_end(res, tl, conc), NIL));
      tl = NIL;
      conc = true;
    }
    else if(EQ(ll->car(), C_GGT))
    {
      if(is_NIL(res))
        res = cons(C_REDIR_APPEND, cons(tl, NIL));
      else
        res = cons(C_REDIR_APPEND, cons(put_end(res, tl, conc), NIL));
      tl = NIL;
      conc = true;
    }
    else if(EQ(ll->car(), C_LT))
    {
      if(is_NIL(res))
        res = cons(C_REDIR_FROM, cons(tl, NIL));
      else
        res = cons(C_REDIR_FROM, cons(put_end(res, tl, conc), NIL));
      tl = NIL;
      conc = true;
    }
    else if(EQ(ll->car(), C_AMPER))
    {
      if(is_NIL(res))
        res = cons(C_BACK, cons(tl, NIL));
      else
        res = cons(C_BACK, cons(put_end(res, tl, conc), NIL));
      tl = NIL;
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

inline std::unique_ptr<::lisp::lisp> init()
{
  signal(SIGTTIN, SIG_IGN);
  signal(SIGTTOU, SIG_IGN); /* otherwise can't get ctrl tty back */

  fixpgrp();

  auto l = std::make_unique<::lisp::lisp>();

  C_ALIAS = alloc::intern("alias");
  C_AMPER = alloc::intern("&");
  C_BACK = alloc::intern(pn::BACK);
  C_BAR = alloc::intern("|");
  C_EXCL = alloc::intern("!");
  C_EXEC = alloc::intern(pn::EXEC);
  C_GGT = alloc::intern(">>");
  C_GT = alloc::intern(">");
  C_LT = alloc::intern("<");
  C_OLDVAL = alloc::intern("oldval");
  C_PIPE = alloc::intern(pn::PIPECMD);
  C_PROGN = alloc::intern("progn");
  C_REDIR_APPEND = alloc::intern(pn::REDIR_APPEND);
  C_REDIR_FROM = alloc::intern(pn::REDIR_FROM);
  C_REDIR_TO = alloc::intern(pn::REDIR_TO);
  C_SEMI = alloc::intern(";");

  top::init(l->a());

  env = std::make_unique<environment>();

  top::transform_hook = transform;
  top::prompt_hook = promptfun;
  breakhook(onbreak);

  exec::init();

  return l;
}

/*
 * Loads the file INITFILE.
 */
static void loadinit(const char* initfile)
{
  loadfile(initfile);
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
    s = who->stringval().c_str();
  if(s == nullptr)
    return NIL;
  struct passwd* pws = getpwnam(s);
  if(pws == nullptr)
    return NIL;
  char loadf[256];
  strcpy(loadf, pws->pw_dir);
  strcat(loadf, "/.lipsrc");
  loadfile(loadf);
  return T;
}

int main(int argc, char* const* argv)
{
  doctest::Context context;

  int option;
  options_t options;
  while((option = getopt(argc, argv, "c:fvidT")) != EOF)
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
      case 'T':
        options.test = true;
        break;
      default:
        std::cout << "usage: -fvicT [arguments]\n";
        exit(1);
        break;
    }
  }
  if(!options.interactive && !options.command)
    options.interactive = isatty(0);
  if(options.version)
    std::cout << VERSION << '\n';
  progname = argv[0];

  /*
   * Init shell and lisp interpreter.
   */
  auto lisp = init();
  if(options.test)
  {
    context.applyCommandLine(argc, argv);
    auto result = context.run();
    return result;
  }
  if(!options.fast)
  {
    try
    {
      loadinit(LIPSRC);
      greet(NIL);
    }
    catch(const lisp_error& error)
    {}
  }
  file_t terminal(std::make_unique<term_source>(options));
  top toploop(*lisp, options, terminal);
  while(true)
  {
    try
    {
      if(!options.debug && options.interactive)
        init_all_signals();
      lisp->e().reset();
      lisp->repl = [&toploop](LISPT exp) -> LISPT { return toploop(exp); };
      lisp->repl(NIL);
      return 0;
    }
    catch(const lisp_reset&)
    {
      std::cout << "^C\n";
    }
    catch(const lisp_error& error)
    {
      static_cast<term_source&>(terminal.source()).clearlbuf();
      std::cout << "error: " << error.what() << '\n';
    }
    catch(const lisp_finish& fin)
    {
      lisp->primerr().format("finish: {}", fin.what());
      return fin.exit_code;
    }
  }
  return 0;
}

//
// Lips, lisp shell.
// Copyright 1988, 2020-2022 Krister Joas
//

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

#include <sys/types.h>
#include <sys/select.h>
#include <pwd.h>
#include <unistd.h>

#include <csignal>
#include <cstdlib>
#include <cerrno>
#include <cctype>
#include <string>

#include <lisp/lisp.hh>
#include "main.hh"
#include "env.hh"
#include "exec.hh"
#include "top.hh"
#include "term.hh"

using namespace lisp;

namespace
{
volatile sig_atomic_t signal_flag;
int mypgrp;     // lips process group

void onsignal(int sig)
{
  signal_flag = sig;
}

void fixpgrp()
{
  mypgrp = getpgrp();
  tcsetpgrp(0, mypgrp);
}

void init_all_signals()
{
  signal(SIGINT, onsignal);
  signal(SIGHUP, SIG_DFL);
  signal(SIGTSTP, onsignal);
}

LISPT put_end(LISPT list, LISPT obj, bool conc)
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

// NOLINTNEXTLINE(readability-function-cognitive-complexity)
LISPT transform(::lisp::lisp& l, LISPT list)
{
  LISPT tl = NIL;
  LISPT res = NIL;
  bool conc = false;
  for(auto ll = list; type_of(ll) == type::CONS; ll = ll->cdr())
  {
    if(type_of(ll->car()) == type::CONS)
      tl = put_end(tl, transform(l, ll->car()), conc);
    else if(ll->car() == C_BAR)
    {
      if(is_NIL(res))
        res = cons(C_PIPE, cons(tl, NIL));
      else
        res = cons(C_PIPE, cons(put_end(res, tl, conc), NIL));
      tl = NIL;
      conc = false;
    }
    else if(ll->car() == C_SEMI)
    {
      // Semicolon is considered a comment character. If progn transformation
      // is to be effective ';' cannot be a comment character.
      if(is_NIL(res))
        res = cons(C_PROGN, cons(tl, NIL));
      else
        res = cons(C_PROGN, cons(put_end(res, tl, conc), NIL));
      tl = NIL;
      conc = false;
    }
    else if(ll->car() == C_GT)
    {
      if(is_NIL(res))
        res = cons(C_REDIR_TO, cons(tl, NIL));
      else
        res = cons(C_REDIR_TO, cons(put_end(res, tl, conc), NIL));
      tl = NIL;
      conc = true;
    }
    else if(ll->car() == C_GGT)
    {
      if(is_NIL(res))
        res = cons(C_REDIR_APPEND, cons(tl, NIL));
      else
        res = cons(C_REDIR_APPEND, cons(put_end(res, tl, conc), NIL));
      tl = NIL;
      conc = true;
    }
    else if(ll->car() == C_LT)
    {
      if(is_NIL(res))
        res = cons(C_REDIR_FROM, cons(tl, NIL));
      else
        res = cons(C_REDIR_FROM, cons(put_end(res, tl, conc), NIL));
      tl = NIL;
      conc = true;
    }
    else if(ll->car() == C_AMPER)
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

void promptfun()
{
  tcsetpgrp(0, mypgrp); // Get control of tty
  insidefork = false;
  //
  // Check for jobs that are finished and print them.
  //
  checkfork();
  printdone();
}

void onbreak()
{
  if(insidefork)
    exit(1);
}

std::unique_ptr<::lisp::lisp> init()
{
  signal(SIGTTIN, SIG_IGN); // NOLINT
  signal(SIGTTOU, SIG_IGN); // Otherwise can't get ctrl tty back NOLINT

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

  environment = std::make_unique<env>();

  top::transform_hook = transform;
  top::prompt_hook = promptfun;
  breakhook(onbreak);

  exec::init();

  l->read_table().set('!', "rmexcl"_l);

  return l;
}

//
// Loads the file INITFILE.
//
void loadinit(const std::string& initfile)
{
  loadfile(initfile);
}

//
// Greet user who, or if who is nil, $USER. This means loading
// the user's init file, .lipsrc.
//
LISPT greet(LISPT who)
{
  std::string s;
  if(is_NIL(who))
    s = getenv("USER");
  else
    s = who->string();
  if(s.empty())
    return NIL;
  struct passwd* pws = getpwnam(s.c_str());
  if(pws == nullptr)
    return NIL;
  std::string loadf;
  loadf = pws->pw_dir;
  loadf.append("/.lipsrc");
  loadfile(loadf);
  return T;
}
}

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

std::unique_ptr<env> environment;

int main(int argc, char* const* argv)
{
  Catch::Session session;

  signal_flag = 0;
  int option = 0;
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
    options.interactive = isatty(0) != 0;
  if(options.version)
    std::cout << VERSION << '\n';

  //
  // Init shell and lisp interpreter.
  //
  auto lisp = init();
  if(options.test)
  {
    auto result = session.run();
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
    {
      std::cout << "Error loading rc file: " << error.what() << '\n';
    }
  }
  ref_file_t terminal{new file_t(std::make_unique<term_source>(options))};
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
      dynamic_cast<term_source&>(terminal->source()).clearlbuf();
      std::cerr << "error: " << error.what() << '\n';
    }
    catch(const lisp_finish& fin)
    {
      lisp->primerr()->format("finish: {}\n", fin.what());
      return fin.exit_code;
    }
  }
  return 0;
}

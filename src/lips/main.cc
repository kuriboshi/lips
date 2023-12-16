//
// Lips, lisp shell.
// Copyright 1988, 2020-2023 Krister Joas
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//

#define CATCH_CONFIG_RUNNER
#include <catch2/catch_test_macros.hpp>
#include <catch2/catch_session.hpp>

#include <sys/types.h>
#include <sys/select.h>
#include <pwd.h>
#include <unistd.h>

#include <csignal>
#include <cstdlib>
#include <cerrno>
#include <cctype>
#include <string>
#include <iostream>

#include <lisp/lisp.hh>
#include <lisp/syntax.hh>
#include "main.hh"
#include "env.hh"
#include "exec.hh"
#include "job.hh"
#include "top.hh"
#include "term.hh"

using namespace lisp;

namespace
{
volatile sig_atomic_t signal_flag; // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)
// lips process group
int mypgrp; // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)

void onsignal(int sig) { signal_flag = sig; }

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

lisp_t put_end(lisp_t list, lisp_t obj, bool conc)
{
  if(is_nil(list))
  {
    if(conc)
      return obj;
    return cons(obj, nil);
  }
  lisp_t t;
  for(t = list; type_of(t->cdr()) == object::type::Cons; t = t->cdr())
    ;
  if(conc)
    rplacd(t, obj);
  else
    rplacd(t, cons(obj, nil));
  return list;
}

// NOLINTNEXTLINE(readability-function-cognitive-complexity)
lisp_t transform(lisp_t list)
{
  lisp_t tl = nil;
  lisp_t res = nil;
  bool conc = false;
  for(auto ll = list; type_of(ll) == object::type::Cons; ll = ll->cdr())
  {
    if(type_of(ll->car()) == object::type::Cons)
      tl = put_end(tl, transform(ll->car()), conc);
    else if(ll->car() == C_BAR)
    {
      if(is_nil(res))
        res = cons(C_PIPE, cons(tl, nil));
      else
        res = cons(C_PIPE, cons(put_end(res, tl, conc), nil));
      tl = nil;
      conc = false;
    }
    else if(ll->car() == C_SEMI)
    {
      // Semicolon is considered a comment character. If progn transformation
      // is to be effective ';' cannot be a comment character.
      if(is_nil(res))
        res = cons(C_PROGN, cons(tl, nil));
      else
        res = cons(C_PROGN, cons(put_end(res, tl, conc), nil));
      tl = nil;
      conc = false;
    }
    else if(ll->car() == C_GT)
    {
      if(is_nil(res))
        res = cons(C_REDIR_TO, cons(tl, nil));
      else
        res = cons(C_REDIR_TO, cons(put_end(res, tl, conc), nil));
      tl = nil;
      conc = true;
    }
    else if(ll->car() == C_GGT)
    {
      if(is_nil(res))
        res = cons(C_REDIR_APPEND, cons(tl, nil));
      else
        res = cons(C_REDIR_APPEND, cons(put_end(res, tl, conc), nil));
      tl = nil;
      conc = true;
    }
    else if(ll->car() == C_LT)
    {
      if(is_nil(res))
        res = cons(C_REDIR_FROM, cons(tl, nil));
      else
        res = cons(C_REDIR_FROM, cons(put_end(res, tl, conc), nil));
      tl = nil;
      conc = true;
    }
    else if(ll->car() == C_AMPER)
    {
      if(is_nil(res))
        res = cons(C_BACK, cons(tl, nil));
      else
        res = cons(C_BACK, cons(put_end(res, tl, conc), nil));
      tl = nil;
      conc = true;
    }
    else
      tl = put_end(tl, ll->car(), false);
  }
  if(is_nil(res))
    return tl;
  if(!is_nil(tl))
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
  job::printdone();
}

void onbreak()
{
  if(insidefork)
    ::exit(1);
}

void init()
{
  signal(SIGTTIN, SIG_IGN);
  signal(SIGTTOU, SIG_IGN); // Otherwise can't get ctrl tty back

  fixpgrp();

  top::init();

  environment = std::make_unique<env>();

  top::transform_hook = transform;
  top::prompt_hook = promptfun;
  breakhook(onbreak);

  exec::init();

  vm::read_table().set('!', "rmexcl"_l);
}

//
// Loads the file INITFILE.
//
void loadinit(const std::string& initfile) { loadfile(initfile); }

//
// Greet user who, or if who is nil, $USER. This means loading
// the user's init file, .lipsrc.
//
lisp_t greet(lisp_t who)
{
  std::string s;
  if(is_nil(who))
    s = getenv("USER");
  else
    s = who->string();
  if(s.empty())
    return nil;
  struct passwd* pws = getpwnam(s.c_str());
  if(pws == nullptr)
    return nil;
  std::string loadf;
  loadf = pws->pw_dir;
  loadf.append("/.lipsrc");
  loadfile(loadf);
  return T;
}
} // namespace

const lisp_t C_ALIAS = intern("alias");
const lisp_t C_AMPER = intern("&");
const lisp_t C_BACK = intern(pn::BACK);
const lisp_t C_BAR = intern("|");
const lisp_t C_EXCL = intern("!");
const lisp_t C_EXEC = intern(pn::EXEC);
const lisp_t C_GGT = intern(">>");
const lisp_t C_GT = intern(">");
const lisp_t C_LT = intern("<");
const lisp_t C_OLDVAL = intern("oldval");
const lisp_t C_PIPE = intern(pn::PIPECMD);
const lisp_t C_PROGN = intern("progn");
const lisp_t C_REDIR_APPEND = intern(pn::REDIR_APPEND);
const lisp_t C_REDIR_FROM = intern(pn::REDIR_FROM);
const lisp_t C_REDIR_TO = intern(pn::REDIR_TO);
const lisp_t C_SEMI = intern(";");

std::unique_ptr<env> environment; // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)

int main(int argc, char* const* argv)
try
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
        ::exit(1);
        break;
    }
  }
  if(!options.interactive && !options.command)
    options.interactive = isatty(0) != 0;
  if(options.version)
    std::cout << version() << '\n';

  //
  // Init shell and lisp interpreter.
  //
  auto context = std::make_unique<context_t>();
  lisp::vm_t vm(std::move(context));
  init();
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
      greet(nil);
    }
    catch(const lisp_error& error)
    {
      std::cout << "Error loading rc file: " << error.what() << '\n';
    }
  }
  ref_file_t terminal{new file_t(std::make_unique<term_source>(options))};
  top toploop(options, terminal);
  while(true)
  {
    try
    {
      if(!options.debug && options.interactive)
        init_all_signals();
      vm::get().unwind();
      vm::get().repl = [&toploop](lisp_t exp) -> lisp_t { return toploop(exp); };
      vm::get().repl(nil);
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
      lisp::vm::primerr()->format("finish: {}\n", fin.what());
      return fin.exit_code;
    }
  }
  return 0;
}
catch(const std::exception& ex)
{
  std::cerr << "unknown exception: " << ex.what() << std::endl;
  return 1;
}
catch(...)
{
  return 1;
}

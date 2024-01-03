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

#include "main.hh"

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
#include "env.hh"
#include "exec.hh"
#include "job.hh"
#include "term.hh"
#include "top.hh"
#include "transform.hh"

using namespace lisp;
using namespace lisp::literals;

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

  top::transform_hook = transform;
  top::prompt_hook = promptfun;
  breakhook(onbreak);

  exec::init();

  vm::read_table().macro('!', "rmexcl"_l);
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
    s = who->as_string();
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
  auto terminal = std::make_unique<term_source>(options);
  top toploop(options, std::move(terminal));
  while(true)
  {
    try
    {
      if(!options.debug && options.interactive)
        init_all_signals();
      vm.repl = [&toploop](lisp_t exp) -> lisp_t { return toploop(exp); };
      vm.repl(nil);
      return 0;
    }
    catch(const lisp_reset&)
    {
      vm.unwind();
    }
    catch(const lisp_error& error)
    {
      std::cerr << "error: " << error.what() << '\n';
      vm.unwind();
    }
    catch(const lisp_finish& fin)
    {
      vm::primerr()->format("finish: {}\n", fin.what());
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

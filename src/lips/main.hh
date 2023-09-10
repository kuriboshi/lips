//
// Lips, lisp shell.
// Copyright 2020-2022 Krister Joas
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

#ifndef LIPS_MAIN_HH
#define LIPS_MAIN_HH

#include <memory>
#include <lisp/lisp.hh>
#include "env.hh"

inline lisp::ref_file_t primout() { return lisp::vm::primout(); }
inline lisp::ref_file_t primin() { return lisp::vm::primin(); }
inline lisp::ref_file_t primerr() { return lisp::vm::primerr(); }

struct options_t
{
  bool debug = false;       // Debugging
  bool interactive = false; // Force interactive mode
  bool command = false;     // Command string
  bool version = false;     // Print version
  bool fast = false;        // Fast start, don't read init file
  bool test = false;        // Run unit tests
};

extern std::unique_ptr<lisp::env> environment; // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)

extern const lisp::lisp_t C_ALIAS;
extern const lisp::lisp_t C_AMPER;
extern const lisp::lisp_t C_BACK;
extern const lisp::lisp_t C_BAR;
extern const lisp::lisp_t C_EXCL;
extern const lisp::lisp_t C_EXEC;
extern const lisp::lisp_t C_GGT;
extern const lisp::lisp_t C_GT;
extern const lisp::lisp_t C_LT;
extern const lisp::lisp_t C_OLDVAL;
extern const lisp::lisp_t C_PIPE;
extern const lisp::lisp_t C_PROGN;
extern const lisp::lisp_t C_REDIR_APPEND;
extern const lisp::lisp_t C_REDIR_FROM;
extern const lisp::lisp_t C_REDIR_TO;
extern const lisp::lisp_t C_SEMI;

#endif

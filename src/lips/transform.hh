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

#ifndef LIPS_TRANSFORM_HH
#define LIPS_TRANSFORM_HH

#include <lisp/lisp.hh>

lisp::lisp_t transform(lisp::lisp_t list);

extern const lisp::lisp_t C_AMPER;
extern const lisp::lisp_t C_BACK;
extern const lisp::lisp_t C_BAR;
extern const lisp::lisp_t C_GGT;
extern const lisp::lisp_t C_GT;
extern const lisp::lisp_t C_LT;
extern const lisp::lisp_t C_PIPE;
extern const lisp::lisp_t C_PROGN;
extern const lisp::lisp_t C_REDIR_APPEND;
extern const lisp::lisp_t C_REDIR_FROM;
extern const lisp::lisp_t C_REDIR_TO;
extern const lisp::lisp_t C_SEMI;

#endif

//
// Lips, lisp shell.
// Copyright 2020-2024 Krister Joas
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

#pragma once

#include "alloc.hh"
#include "types.hh"

namespace lisp::atoms
{
/// @internal Symbols which are used internally.
inline const lisp_t APPEND = intern("append");
inline const lisp_t AUTOLOAD = intern("autoload");
inline const lisp_t BROKEN = intern("broken");
inline const lisp_t BT = intern("bt");
inline const lisp_t CLOSURE = intern("closure");
inline const lisp_t CONS = intern("cons");
inline const lisp_t CVARIABLE = intern("cvariable");
inline const lisp_t DOT = intern(".");
inline const lisp_t ENDOFFILE = intern("endoffile");
inline const lisp_t ENVIRON = intern("environ");
inline const lisp_t ERROR = intern("error");
inline const lisp_t FILE = intern("file");
inline const lisp_t FLOAT = intern("float");
inline const lisp_t FSUBR = intern("fsubr");
inline const lisp_t GO = intern("go");
inline const lisp_t INDIRECT = intern("indirect");
inline const lisp_t INTEGER = intern("integer");
inline const lisp_t LAMBDA = intern("lambda");
inline const lisp_t NLAMBDA = intern("nlambda");
inline const lisp_t OLDDEF = intern("olddef");
inline const lisp_t QUOTE = intern("quote");
inline const lisp_t READ = intern("read");
inline const lisp_t REDEFINED = intern("redefined");
inline const lisp_t RESET = intern("reset");
inline const lisp_t RETURN = intern("return");
inline const lisp_t STRING = intern("string");
inline const lisp_t SUBR = intern("subr");
inline const lisp_t SYMBOL = intern("symbol");
inline const lisp_t UNBOUND = intern("unbound");
inline const lisp_t WRITE = intern("write");
} // namespace lisp::atoms

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

#include <iostream>
#include <memory>
#include <system_error>

#include "lisp.hh"
#include "syntax.hh"

namespace lisp
{
closure_t::pool_t closure_t::_pool;
lisp_t::pool_t lisp_t::_pool;

std::unordered_map<std::string, subr_t::subr_index> subr_t::subr_map;
subr_t::subr_vector subr_t::subr_store;

//
// All lisp constants needed internally.
//
LISPT T;
LISPT C_AUTOLOAD;
LISPT C_BROKEN;
LISPT C_BT;
LISPT C_CLOSURE;
LISPT C_CONS;
LISPT C_DOT;
LISPT C_ENDOFFILE;
LISPT C_ENVIRON;
LISPT C_EOF;
LISPT C_FILE;
LISPT C_FLOAT;
LISPT C_FSUBR;
LISPT C_GO;
LISPT C_INDIRECT;
LISPT C_INTEGER;
LISPT C_OLDDEF;
LISPT C_REDEFINED;
LISPT C_RESET;
LISPT C_RETURN;
LISPT C_STRING;
LISPT C_SUBR;
LISPT C_SYMBOL;
LISPT C_UNBOUND;
LISPT C_READ;
LISPT C_WRITE;
LISPT C_APPEND;
LISPT C_VERSION;

} // namespace lisp

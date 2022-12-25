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
lambda_t::pool_t lambda_t::_pool;
closure_t::pool_t closure_t::_pool;
object::pool_t object::_pool;

std::unordered_map<std::string, subr_t::subr_index> subr_t::subr_map;
subr_t::subr_vector subr_t::subr_store;

//
// All lisp constants needed internally.
//
lisp_t T;
lisp_t C_AUTOLOAD;
lisp_t C_BROKEN;
lisp_t C_BT;
lisp_t C_CLOSURE;
lisp_t C_CONS;
lisp_t C_DOT;
lisp_t C_ENDOFFILE;
lisp_t C_ENVIRON;
lisp_t C_EOF;
lisp_t C_FILE;
lisp_t C_FLOAT;
lisp_t C_FSUBR;
lisp_t C_GO;
lisp_t C_INDIRECT;
lisp_t C_INTEGER;
lisp_t C_OLDDEF;
lisp_t C_REDEFINED;
lisp_t C_RESET;
lisp_t C_RETURN;
lisp_t C_STRING;
lisp_t C_SUBR;
lisp_t C_SYMBOL;
lisp_t C_UNBOUND;
lisp_t C_READ;
lisp_t C_WRITE;
lisp_t C_APPEND;
lisp_t C_VERSION;
lisp_t C_CVARIABLE;

} // namespace lisp

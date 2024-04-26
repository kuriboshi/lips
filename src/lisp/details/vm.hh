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

#ifndef LISP_DETAILS_VM_HH
#define LISP_DETAILS_VM_HH

#include <lisp/types.hh>

namespace lisp::details::vm
{
void init();

lisp_t quote(const lisp_t&);
lisp_t lambda(const lisp_t&, const lisp_t&);
lisp_t nlambda(const lisp_t&, const lisp_t&);
lisp_t closure(const lisp_t&, const lisp_t&);
lisp_t error(const lisp_t&);
lisp_t exit(const lisp_t&);
lisp_t version();
} // namespace lisp::details::vm

#endif

//
// Lips, lisp shell.
// Copyright 2020-2025 Krister Joas
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

#include <lisp/types.hh>

namespace lisp::details::file
{

void init();

lisp_t open(const lisp_t&, const lisp_t&);
lisp_t close(const lisp_t&);
lisp_t ratom(const lisp_t&);
lisp_t readc(const lisp_t&);
lisp_t read(const lisp_t&);
lisp_t print(const lisp_t&, const lisp_t&);
lisp_t load(const lisp_t&);
lisp_t terpri(const lisp_t&);
lisp_t prin1(const lisp_t&, const lisp_t&);
lisp_t prin2(const lisp_t&, const lisp_t&);
lisp_t printlevel(const lisp_t&);
lisp_t spaces(const lisp_t&, const lisp_t&);
lisp_t readline(const lisp_t&);
lisp_t splice(const lisp_t&, const lisp_t&, const lisp_t&);

} // namespace lisp::details::file

//
// Lips, lisp shell.
// Copyright 2020-2023 Krister Joas
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

#ifndef LISP_DETAILS_FILE_HH
#define LISP_DETAILS_FILE_HH

#include <lisp/types.hh>

namespace lisp::details::file
{

void init();

lisp_t open(lisp_t, lisp_t);
lisp_t close(lisp_t);
lisp_t ratom(lisp_t);
lisp_t readc(lisp_t);
lisp_t read(lisp_t);
lisp_t print(lisp_t, lisp_t);
lisp_t load(lisp_t);
lisp_t terpri(lisp_t);
lisp_t prin1(lisp_t, lisp_t);
lisp_t prin2(lisp_t, lisp_t);
lisp_t printlevel(lisp_t);
lisp_t spaces(lisp_t, lisp_t);
lisp_t readline(lisp_t);
lisp_t splice(lisp_t, lisp_t, lisp_t);

} // namespace lisp::details::file

#endif

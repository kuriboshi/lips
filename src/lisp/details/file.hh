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

#ifndef LISP_DETAILS_FILE_HH
#define LISP_DETAILS_FILE_HH

#include <lisp/types.hh>

namespace lisp::details::file
{

void init();

lisp_t open(lisp_t, lisp_t);
lisp_t close(lisp_t);
lisp_t ratom(context&, lisp_t);
lisp_t readc(context&, lisp_t);
lisp_t read(context&, lisp_t);
lisp_t print(context&, lisp_t, lisp_t);
lisp_t load(context&, lisp_t);
lisp_t terpri(context&, lisp_t);
lisp_t prin1(context&, lisp_t, lisp_t);
lisp_t prin2(context&, lisp_t, lisp_t);
lisp_t printlevel(context&, lisp_t);
lisp_t spaces(context&, lisp_t, lisp_t);
lisp_t readline(context&, lisp_t);

bool loadfile(context&, const std::string&);

} // namespace lisp::details::file

#endif

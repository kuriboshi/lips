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

#ifndef LISP_FILE_HH
#define LISP_FILE_HH

#include "types.hh"
#include "details/file.hh"

namespace lisp
{

inline lisp_t close(lisp_t a) { return details::file::close(a); }
inline lisp_t load(lisp_t a) { return details::file::load(a); }
inline lisp_t open(lisp_t a, lisp_t b) { return details::file::open(a, b); }
inline lisp_t prin1(lisp_t a, lisp_t b) { return details::file::prin1(a, b); }
inline lisp_t prin2(lisp_t a, lisp_t b) { return details::file::prin2(a, b); }
inline lisp_t print(lisp_t a, lisp_t b) { return details::file::print(a, b); }
inline lisp_t printlevel(lisp_t a) { return details::file::printlevel(a); }
inline lisp_t ratom(lisp_t a) { return details::file::ratom(a); }
inline lisp_t read(lisp_t a) { return details::file::read(a); }
inline lisp_t readc(lisp_t a) { return details::file::readc(a); }
inline lisp_t readline(lisp_t a) { return details::file::readline(a); }
inline lisp_t spaces(lisp_t a, lisp_t b) { return details::file::spaces(a, b); }
inline lisp_t terpri(lisp_t a) { return details::file::terpri(a); }

inline bool loadfile(const std::string& filename) { return details::file::loadfile(filename); }

} // namespace lisp

#endif

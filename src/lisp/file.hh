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

#ifndef LISP_FILE_HH
#define LISP_FILE_HH

#include "lisp.hh"
#include "details/file.hh"

namespace lisp
{

inline LISPT close(LISPT a) { return details::file::close(lisp::current(), a); }
inline LISPT close(lisp& l, LISPT a) { return details::file::close(l, a); }
inline LISPT load(LISPT a) { return details::file::load(lisp::current(), a); }
inline LISPT load(lisp& l, LISPT a) { return details::file::load(l, a); }
inline LISPT open(LISPT a, LISPT b) { return details::file::open(lisp::current(), a, b); }
inline LISPT open(lisp& l, LISPT a, LISPT b) { return details::file::open(l, a, b); }
inline LISPT prin1(LISPT a, LISPT b) { return details::file::prin1(lisp::current(), a, b); }
inline LISPT prin1(lisp& l, LISPT a, LISPT b) { return details::file::prin1(l, a, b); }
inline LISPT prin2(LISPT a, LISPT b) { return details::file::prin2(lisp::current(), a, b); }
inline LISPT prin2(lisp& l, LISPT a, LISPT b) { return details::file::prin2(l, a, b); }
inline LISPT print(LISPT a, LISPT b) { return details::file::print(lisp::current(), a, b); }
inline LISPT print(lisp& l, LISPT a, LISPT b) { return details::file::print(l, a, b); }
inline LISPT printlevel(LISPT a) { return details::file::printlevel(lisp::current(), a); }
inline LISPT printlevel(lisp& l, LISPT a) { return details::file::printlevel(l, a); }
inline LISPT ratom(LISPT a) { return details::file::ratom(lisp::current(), a); }
inline LISPT ratom(lisp& l, LISPT a) { return details::file::ratom(l, a); }
inline LISPT read(LISPT a) { return details::file::read(lisp::current(), a); }
inline LISPT read(lisp& l, LISPT a) { return details::file::read(l, a); }
inline LISPT readc(LISPT a) { return details::file::readc(lisp::current(), a); }
inline LISPT readc(lisp& l, LISPT a) { return details::file::readc(l, a); }
inline LISPT readline(LISPT a) { return details::file::readline(lisp::current(), a); }
inline LISPT readline(lisp& l, LISPT a) { return details::file::readline(l, a); }
inline LISPT spaces(LISPT a, LISPT b) { return details::file::spaces(lisp::current(), a, b); }
inline LISPT spaces(lisp& l, LISPT a, LISPT b) { return details::file::spaces(l, a, b); }
inline LISPT terpri(LISPT a) { return details::file::terpri(lisp::current(), a); }
inline LISPT terpri(lisp& l, LISPT a) { return details::file::terpri(l, a); }

inline bool loadfile(lisp& l, const std::string& filename) { return details::file::loadfile(l, filename); }
inline bool loadfile(const std::string& filename) { return details::file::loadfile(lisp::current(), filename); }

} // namespace lisp

#endif

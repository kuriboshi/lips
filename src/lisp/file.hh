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

namespace lisp::file
{
void init();

LISPT open(lisp&, LISPT, LISPT);
LISPT close(lisp&, LISPT);
LISPT ratom(lisp&, LISPT);
LISPT readc(lisp&, LISPT);
LISPT read(lisp&, LISPT);
LISPT print(lisp&, LISPT, LISPT);
LISPT load(lisp&, LISPT);
LISPT terpri(lisp&, LISPT);
LISPT prin1(lisp&, LISPT, LISPT);
LISPT prin2(lisp&, LISPT, LISPT);
LISPT printlevel(lisp&, LISPT);
LISPT spaces(lisp&, LISPT, LISPT);
LISPT readline(lisp&, LISPT);

bool loadfile(lisp&, const std::string&);
}

namespace lisp
{

inline LISPT open(lisp& l, LISPT a, LISPT b) { return file::open(l, a, b); }
inline LISPT open(LISPT a, LISPT b) { return file::open(lisp::current(), a, b); }
inline LISPT close(lisp& l, LISPT a) { return file::close(l, a); }
inline LISPT close(LISPT a) { return file::close(lisp::current(), a); }
inline LISPT ratom(lisp& l, LISPT a) { return file::ratom(l, a); }
inline LISPT ratom(LISPT a) { return file::ratom(lisp::current(), a); }
inline LISPT readc(lisp& l, LISPT a) { return file::readc(l, a); }
inline LISPT readc(LISPT a) { return file::readc(lisp::current(), a); }
inline LISPT read(lisp& l, LISPT a) { return file::read(l, a); }
inline LISPT read(LISPT a) { return file::read(lisp::current(), a); }
inline LISPT print(lisp& l, LISPT a, LISPT b) { return file::print(l, a, b); }
inline LISPT print(LISPT a, LISPT b) { return file::print(lisp::current(), a, b); }
inline LISPT load(lisp& l, LISPT a) { return file::load(l, a); }
inline LISPT load(LISPT a) { return file::load(lisp::current(), a); }
inline LISPT terpri(lisp& l, LISPT a) { return file::terpri(l, a); }
inline LISPT terpri(LISPT a) { return file::terpri(lisp::current(), a); }
inline LISPT prin1(lisp& l, LISPT a, LISPT b) { return file::prin1(l, a, b); }
inline LISPT prin1(LISPT a, LISPT b) { return file::prin1(lisp::current(), a, b); }
inline LISPT prin2(lisp& l, LISPT a, LISPT b) { return file::prin2(l, a, b); }
inline LISPT prin2(LISPT a, LISPT b) { return file::prin2(lisp::current(), a, b); }
inline LISPT printlevel(lisp& l, LISPT a) { return file::printlevel(l, a); }
inline LISPT printlevel(LISPT a) { return file::printlevel(lisp::current(), a); }
inline LISPT spaces(lisp& l, LISPT a, LISPT b) { return file::spaces(l, a, b); }
inline LISPT spaces(LISPT a, LISPT b) { return file::spaces(lisp::current(), a, b); }
inline LISPT readline(lisp& l, LISPT a) { return file::readline(l, a); }
inline LISPT readline(LISPT a) { return file::readline(lisp::current(), a); }

inline bool loadfile(lisp& l, const std::string& filename) { return file::loadfile(l, filename); }
inline bool loadfile(const std::string& filename) { return file::loadfile(lisp::current(), filename); }

} // namespace lisp

#endif

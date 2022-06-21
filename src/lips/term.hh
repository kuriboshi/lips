//
// Lips, lisp shell.
// Copyright 1988, 2020-2022 Krister Joas
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

#ifndef LIPS_TERM_HH
#define LIPS_TERM_HH

#include <termios.h>
#include <lisp/io.hh>

class term_source: public lisp::io::source
{
public:
  term_source(const options_t& options) : is(linebuffer), options(options) {}
  ~term_source() override;

  // io::source
  int getch() override;
  void ungetch(int) override;
  bool close() override { return true; }
  std::optional<std::string> getline() override;

  static void end_term();
  void clearlbuf();

  iterator begin() override { is.seekg(0); return iterator(is); }

private:
  bool getline(lisp::lisp&);
  void init_keymap();
  void init_term();
  void pputc(int c, FILE* file);
  void putch(int c, FILE* file, bool esc);
  bool firstnotlp();
  void delonechar();
  bool onlyblanks();
  static int outc(int c);
  void retype(int);
  char* mkexstr();
  void fillrest(const char*);
  bool checkchar(lisp::LISPT words, std::size_t pos, char* c);
  void complete(lisp::LISPT words);
  lisp::LISPT strip(lisp::LISPT files, const char* prefix, const char* suffix);
  void scan(int begin);
  void nput(const char* str, int ntim);
  void blink();
  void clearscr();

  std::array<char, BUFSIZ> word{};
  char* last = nullptr;

  static inline constexpr int NUM_KEYS = 256;
  static inline constexpr char BELL = '\007';

  //
  // Routines for paren blinking.
  //
  enum class paren_blink
  {
    NORMAL,
    INSTRING,
    EXITSTRING,
    STARTSTRING,
    LEFTPAR,
    RIGHTPAR
  };

  struct curpos
  {
    int cpos = 0;
    int line = 0;
    char* line_start = nullptr;
  };

  //
  // Terminal functions.  Each constant stands for a function provided by the
  // line editor.
  //
  enum class term_fun
  {
    T_INSERT = 0,
    T_ERASE,
    T_RETYPE,
    T_CLEAR,
    T_KILL,
    T_EOF,
    T_TAB,
    T_LEFTPAR,
    T_RIGHTPAR,
    T_NEWLINE,
    T_STRING,
    T_ESCAPE
  };

  struct curpos parpos;         // Saves position of matching par.
  struct curpos currentpos; // Current position.

  //
  // Variables for terminal characteristics, old and new.
  //
  struct termios newterm;
  static struct termios oldterm;

  char linebuffer[BUFSIZ] = {};    // Line buffer for terminal input.
  std::istringstream is;           // For input stream.
  int parcount = 0;                // Counts paranthesis.
  int linepos = 0;                 // End of line buffer.
  int position = 0;                // Current position in line buffer.
  enum term_fun key_tab[NUM_KEYS]; // Table specifying key functions.

  const options_t& options;

  char tcap[128];               // Buffer for terminal capabilties.
  const char* curup = nullptr;
  const char* curfwd = nullptr; // Various term cap strings.
  const char* cleol = nullptr;
  const char* curdn = nullptr;
  const char* clear = nullptr;
  bool nocap = false;           // true if insufficient term cap.
};

#endif

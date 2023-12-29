//
// Lips, lisp shell.
// Copyright 1988, 2020-2023 Krister Joas
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

#include <array>
#include <termios.h>
#include <lisp/io.hh>

class term_source: public lisp::io::source
{
public:
  term_source(options_t options)
    : _is(_linebuffer.data()),
      _options(options)
  {}
  ~term_source() override;

  term_source(const term_source&) = delete;
  term_source(term_source&&) = delete;
  term_source& operator=(const term_source&) = delete;
  term_source& operator=(term_source&&) = delete;

  /// @brief Read a character from the terminal.
  ///
  /// Buffer input with procedure getline, and get characters from linebuffer.
  char getch() override;
  /// @brief Unget a character.
  ///
  /// If reading from a terminal, just push it back in the buffer, if not, do
  /// an ungetc.
  void ungetch(char) override;
  /// @brief Get a line from stdin.
  ///
  /// Do line editing functions such as kill line, retype line and delete
  /// character.  Count parethesis pairs and terminate line if matching right
  /// paren.  Typing just a return puts a right paren in the buffer as well as
  /// the newline.
  ///
  /// @returns Empty optional on EOF.
  std::optional<std::string> getline() override;

  /// @brief Reads characters from stding until the next newline.
  ///
  /// The line is split according to the defined break characters and the
  /// result is returned as a string. If a blank line is read the symbol `eof`
  /// is returned.
  ///
  /// ```text
  /// hello world => (hello world)
  /// "hello world" => ("hello world")
  /// (hello world) => (hello world)
  ///
  /// ```
  ///
  /// @param prompt A prompt string to print before reading a line.
  ///
  /// @returns A lisp expression. The symbol `eof` is returned if a blank line
  /// is read.
  lisp::lisp_t readline(std::string prompt);

  iterator begin() override
  {
    _is.seekg(0);
    return {_is};
  }

private:
  struct cursor_position
  {
    int cpos{0};
    int line{0};
    char* line_start{nullptr};
  };

  /// @brief Reset terminal to the previous state.
  void end_term();
  /// @brief Initializes the keymap.
  void init_keymap();
  /// @brief Initializes the terminal to CBREAK and no ECHO.
  void init_term();
  /// @brief Put a character on stdout prefixing it with a ^ if it's
  /// a control character.
  static void pputc(char c, FILE* file);
  /// @brief Put a character _c_ on stream _file_ escaping if _esc_ is `true`.
  static void putch(char c, FILE* file, bool esc);
  /// @brief Skips separators in the beginning of the line and returns `true`
  // if the first non-separator character is a left parenthesis, `false`
  // otherwise.
  bool first_left_paren(const std::string&);
  /// @brief Delete one character the easy way by sending backspace - space -
  /// backspace.  Do it twice if it was a control character.
  void delonechar();
  /// @brief Returns `true` if the line contains only separators.
  bool onlyblanks();
  /// @brief Output a character on stdout, used only in tputs.
  static int outc(int c);
  /// @brief Retype a line.
  ///
  /// If _all_ is 0 then retype only current line.  If _all_ is 1 then retype
  /// complete line, including prompt.  If _all_ is 2 just delete all lines.
  /// Used for ctrl-u kill.
  void retype(int all);
  // Stuff for file name completion.
  char* mkexstr();
  void fillrest(const char*);
  static bool checkchar(lisp::lisp_t words, std::size_t pos, char* c);
  void complete(lisp::lisp_t words);
  static lisp::lisp_t strip(lisp::lisp_t files, const char* prefix, const char* suffix);
  /// @brief Scans backwards and try to find a matching left parenthesis
  /// skipping strings and escapes.
  ///
  /// It records its finding in parpos.  It also updates where the cursor is
  /// now in currentpos, so it can find its way back.  _begin_ is the position
  /// in linebuffer from where to start searching.
  ///
  /// @returns A pair of cursor_position objects. The first is the position of
  /// the matching parenthesis, the second is the current position.
  std::pair<cursor_position, cursor_position> scan(int begin);
  /// @brief Puts the string _str_ on stdout _ntim_ times using tputs.
  static void nput(const std::string& str, int ntim = 1);
  /// @brief Blink matching parenthesis.
  void blink();
  void clearscr();

  bool is_control(auto c) const { return std::iscntrl(c) != 0 && c != '\n' && c != '\t'; }

  static constexpr int NUM_KEYS = 256;
  static constexpr char BELL = '\007';
  static constexpr int blink_time = 1000;
  static constexpr char at_char = '@';

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

  //
  // Terminal functions.  Each constant stands for a function provided by the
  // line editor.
  //
  enum class function
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

  std::array<char, BUFSIZ> _word{};
  char* _last{nullptr};

  //
  // Variables for terminal characteristics, old and new.
  //
  struct termios _newterm
  {};
  struct termios _oldterm
  {};

  std::array<char, BUFSIZ> _linebuffer{}; // Line buffer for terminal input.
  std::istringstream _is;                 // For input stream.
  int _linepos{0};                        // End of line buffer.
  int _position{0};                       // Current position in line buffer.

  std::array<enum function, NUM_KEYS> key_tab{}; // Table specifying key functions.
  options_t _options;

  // Various term cap strings.
  struct termcap
  {
    std::string clear;
    std::string cleol;
    std::string curfwd;
    std::string curup;
    bool nocap{false}; // true if insufficient term cap.
  } _termcap;

  std::string _current_prompt;
};

#endif

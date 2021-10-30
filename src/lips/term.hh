/*
 * Lips, lisp shell.
 * Copyright 1988, 2020 Krister Joas
 *
 */

#pragma once

#include <termios.h>

#include <lisp/io.hh>

class term_source: public lisp::io_source
{
public:
  term_source(const options_t& options) : options(options) {}
  virtual ~term_source();

  // io::source
  virtual int getch(bool inside_string) override;
  virtual void ungetch(int) override;
  virtual bool eoln() override;
  virtual bool close() override { return true; }
  virtual std::optional<std::string> getline() override;

  static void end_term();
  void clearlbuf();

private:
  bool getline(lisp::lisp&);
  void init_keymap();
  void init_term();
  void pputc(int c, FILE* file);
  void putch(int c, FILE* file, int esc);
  bool firstnotlp();
  void delonechar();
  bool onlyblanks();
  static int outc(int c);
  void retype(int);
  char* mkexstr();
  void fillrest(const char*);
  bool checkchar(lisp::LISPT words, int pos, int* c);
  void complete(lisp::LISPT words);
  lisp::LISPT strip(lisp::LISPT files, const char* prefix, const char* suffix);
  void scan(int begin);
  void nput(const char* str, int ntim);
  void blink();
  void clearscr();

  static inline constexpr int NUM_KEYS = 256;
  static inline constexpr char COMMENTCHAR = '#';
  static inline constexpr char BELL = '\007';

  /*
   * Routines for paren blinking.
   */
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

  /*
   * Terminal functions.  Each constant stands for a function provided by the
   * line editor.
   */
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

  struct curpos parpos;     /* Saves position of matching par.  */
  struct curpos currentpos; /* Current position.  */

  /*
   * Variables for terminal characteristics, old and new.
   */
  struct termios newterm;
  static struct termios oldterm;

  char linebuffer[BUFSIZ];         /* Line buffer for terminal input.  */
  int parcount = 0;                /* Counts paranthesis.  */
  int linepos = 0;                 /* End of line buffer.  */
  int position = 0;                /* Current position in line buffer.  */
  enum term_fun key_tab[NUM_KEYS]; /* Table specifying key functions.  */

  const options_t& options;

#ifdef HAVE_CURSES
  char tcap[128]; /* Buffer for terminal capabilties.  */
  const char* curup = nullptr;
  const char* curfwd = nullptr; /* Various term cap strings.  */
  const char* cleol = nullptr;
  const char* curdn = nullptr;
  const char* clear = nullptr;
  bool nocap = false; /* true if insufficient term cap. */
#endif
};

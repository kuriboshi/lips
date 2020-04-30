//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#pragma once

#include <string>
#include "lisp.hh"

namespace lisp
{
class io
{
public:
  io(lisp& lisp): _lisp(lisp) {}
  ~io() = default;

  static inline constexpr char COMMENTCHAR = '#';

  class source
  {
  public:
    source() {}
    virtual ~source() = default;

    virtual int getch() = 0;
    virtual void ungetch(int) = 0;
    virtual bool eoln() = 0;
  };

  class filesource: public source
  {
  public:
    filesource(const char* filename) { _file = fopen(filename, "r"); }

    virtual int getch() override
    {
      auto c = getc(_file);
      if(c == COMMENTCHAR) /* Skip comments.  */
        while((c = getc(_file)) != '\n')
          ;
      return c;
    }
    virtual void ungetch(int c) override { ungetc(c, _file); }
    virtual bool eoln() override { return false; }

  private:
    FILE* _file;
  };

  class stringsource: public source
  {
  public:
    stringsource(const char* string): _string(string), _len(strlen(string)) {}

    virtual int getch() override
    {
      if(_pos == _len)
        return -1;
      auto c = _string[_pos++];
      if(c == COMMENTCHAR) /* Skip comments.  */
        while((c = _string[_pos++]) != '\n')
          ;
      return c;
    }
    virtual void ungetch(int c) override { --_pos; }
    virtual bool eoln() override { return false; }

  private:
    const char* _string;
    int _pos = 0;
    int _len = 0;
  };

  class sink
  {
  public:
    sink() {}
    virtual ~sink() = default;

    virtual void putch(int, int) = 0;
  };

  class filesink: public sink
  {
  public:
    filesink(FILE* file): _file(file) {}
    filesink(const char* filename) { _file = fopen(filename, "w"); }

    virtual void putch(int c, int esc) override { putch(c, _file, esc); }

  private:
    // Put a character on stdout prefixing it with a ^ if it's a control
    // character.
    void pputc(int c, FILE* file)
    {
      if(c < 0x20 && c != '\n' && c != '\t')
      {
        putc('^', file);
        putc(c + 0x40, file);
      }
      else
        putc(c, file);
    }

    /*
     * Put a character c, on stream file, escaping enabled if esc != 0.
     */
    void putch(int c, FILE* file, int esc)
    {
      if((c == '(' || c == '"' || c == ')' || c == '\\') && esc)
        pputc('\\', file);
      pputc(c, file);
    }

    FILE* _file;
  };

  class stringsink: public sink
  {
  public:
    stringsink() {}

    std::string string() const { return _string; }

    virtual void putch(int c, int) override { _string.push_back(static_cast<char>(c)); }

  private:
    std::string _string;
  };

  LISPT top = nullptr;    /* used for threading the input structure */
  LISPT rstack = nullptr; /* partially built structure read stack */
  int printlevel = 0;     /* maximum print level */
  int thisplevel = 0;     /* during print, print level */
  static bool echoline;   /* is true if ! has been used */

  void pushr(LISPT w) { rstack = cons(_lisp, w, rstack); }
  void popr(LISPT& w)
  {
    w = rstack->car();
    rstack = rstack->cdr();
  }

  static constexpr int NUL = '\0';
  static constexpr int MAXATOMSIZE = 128; /* max length of atom read can handle */

  static char buf[MAXATOMSIZE];

  bool integerp(char*, int* res);
  bool floatp(char*);
  LISPT parsebuf(char*);
  LISPT ratom(source*);
  LISPT splice(LISPT c, LISPT, int tailp);
  LISPT lispread(source*, int line);
  static LISPT rmexcl(io&, source*, LISPT, char);
  static LISPT rmdquote(io&, source*, LISPT, char);
  static LISPT rmsquote(io&, source*, LISPT, char);
  static LISPT rmpipe(io&, source*, LISPT, char);
  static LISPT rmredir(io&, source*, LISPT, char);
  static LISPT rmbg(io&, source*, LISPT, char);
  static LISPT rmuser(io&, source*, LISPT, char);

#if 0
  static LISPT userreadmacros[128];
#endif

  LISPT readline(source*);
  LISPT patom(LISPT, sink*, int esc);
  LISPT terpri(sink*);
  LISPT prinbody(LISPT, sink*, int esc);
  LISPT prin0(LISPT, sink*, int esc);
  LISPT print(LISPT, sink*);

private:
  alloc& a() { return _lisp.a(); }
  lisp& _lisp;
};

struct rtinfo
{
  unsigned char chclass[128];
  LISPT (*rmacros[128])(io&, io::source*, LISPT, char);
};

/* variables */
extern LISPT top;
extern LISPT rstack;
extern int printlevel;
extern int thisplevel;
extern bool echoline;
extern struct rtinfo currentrt;

/* functions */
enum char_class
{
  SEPR = 001,   // seperator
  BRK = 002,    // break character
  INSERT = 004, // insert read macro
  SPLICE = 010, // splice read macro
  INFIX = 014,  // infix read macro
  RMACRO = 014  // read macro mask
};

inline bool issepr(int c) { return (currentrt.chclass[c] & SEPR) == SEPR; }
inline bool isbrk(int c) { return (currentrt.chclass[c] & BRK) == BRK; }
inline bool isrm(int c) { return (currentrt.chclass[c] & RMACRO) == RMACRO; }
inline bool isinsert(int c) { return (currentrt.chclass[c] & RMACRO) == INSERT; }
inline bool issplice(int c) { return (currentrt.chclass[c] & RMACRO) == SPLICE; }
inline bool isinfix(int c) { return (currentrt.chclass[c] & RMACRO) == INFIX; }

inline LISPT ratom(lisp& l, io::source* f) { return io(l).ratom(f); }
inline LISPT lispread(lisp& l, io::source* f, int i) { return io(l).lispread(f, i); }
inline LISPT readline(lisp& l, io::source* f) { return io(l).readline(f); }
inline LISPT patom(lisp& l, LISPT a, io::sink* f, int i) { return io(l).patom(a, f, i); }
inline LISPT terpri(lisp& l, io::sink* f) { return io(l).terpri(f); }
inline LISPT prinbody(lisp& l, LISPT a, io::sink* f, int i) { return io(l).prinbody(a, f, i); }
inline LISPT prin0(lisp& l, LISPT a, io::sink* f, int i) { return io(l).prin0(a, f, i); }
inline LISPT print(lisp& l, LISPT a, io::sink* f) { return io(l).print(a, f); }

} // namespace lisp

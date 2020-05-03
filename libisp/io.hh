//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#pragma once

#include <cstdio>
#include <cstdarg>
#include <string>
#include "lisp.hh"
#include "base.hh"

namespace lisp
{
class lisp;
struct file_t;
class io : public base
{
public:
  io(lisp& lisp): base(lisp) {}
  ~io() = default;
  static void init();

  static inline constexpr char COMMENTCHAR = '#';

  class source
  {
  public:
    source() {}
    virtual ~source() = default;

    virtual int getch() = 0;
    virtual void ungetch(int) = 0;
    virtual bool eoln() = 0;
    virtual bool close() = 0;
    virtual const char* getline() = 0;
  };

  class filesource: public source
  {
  public:
    filesource(std::FILE* file) : _file(file), _owner(false) {}
    filesource(const char* filename) : _file(fopen(filename, "r")), _owner(true) {}
    ~filesource() { if(_owner) fclose(_file); }

    virtual int getch() override
    {
      auto c = getc(_file);
      if(c == COMMENTCHAR) /* Skip comments.  */
        while((c = getc(_file)) != '\n')
          ;
      return c;
    }
    virtual void ungetch(int c) override { ungetc(c, _file); }
    virtual bool eoln() override
    {
      while(true)
      {
        auto c = getc(_file);
        if(feof(_file))
          return true;
        if(c != ' ' && c != '\t' && c != '\n')
          return false;
        if (c == '\n')
          return true;
      }
      return true;
    }
    virtual bool close() override
    {
      if(std::fclose(_file) == -1)
        return false;
      return true;
    }
    virtual const char* getline() override
    {
      char* buf = nullptr;
      std::size_t size = 0;
      auto rv = ::getline(&buf, &size, _file);
      if(rv == -1)
        return nullptr;
      return buf;
    }

  private:
    std::FILE* _file;
    bool _owner = false;
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
    virtual bool close() override { return true; }
    virtual const char* getline() override { return _string; }

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

    virtual void putch(int, bool esc = false) = 0;
    virtual void puts(const char*) = 0;
    virtual bool close() = 0;
  };

  class filesink: public sink
  {
  public:
    filesink(std::FILE* file): _file(file) {}
    filesink(const char* filename, bool append = false) { _file = fopen(filename, append ? "a" : "w"); }

    virtual void putch(int c, bool esc) override { putch(c, _file, esc); }
    virtual void puts(const char* s) override { std::fputs(s, _file); }
    virtual bool close() override
    {
      if(std::fclose(_file) == -1)
        return false;
      return true;
    }

  private:
    // Put a character on stdout prefixing it with a ^ if it's a control
    // character.
    void pputc(int c, std::FILE* file)
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
    void putch(int c, std::FILE* file, bool esc)
    {
      if(esc && (c == '(' || c == '"' || c == ')' || c == '\\'))
        pputc('\\', file);
      pputc(c, file);
    }

    std::FILE* _file;
  };

  class stringsink: public sink
  {
  public:
    stringsink() {}

    std::string string() const { return _string; }

    virtual void putch(int c, bool) override { _string.push_back(static_cast<char>(c)); }
    virtual void puts(const char* s) override { _string.append(s); }
    virtual bool close() override { return true; }

  private:
    std::string _string;
  };

  void pushr(LISPT w) { l.rstack = cons(l, w, l.rstack); }
  void popr(LISPT& w)
  {
    w = l.rstack->car();
    l.rstack = l.rstack->cdr();
  }

  static constexpr int NUL = '\0';
  static constexpr int MAXATOMSIZE = 128; /* max length of atom read can handle */

  bool integerp(char*, int* res);
  bool floatp(char*);
  LISPT splice(LISPT, LISPT, bool);
  LISPT parsebuf(char*);

  LISPT ratom(file_t&);
  LISPT lispread(file_t&, bool line = false);
  LISPT readline(file_t&);

  LISPT patom(LISPT, file_t&, bool esc = false);
  LISPT terpri(file_t&);
  LISPT prinbody(LISPT, file_t&, bool esc = false);
  LISPT prin0(LISPT, file_t&, bool esc = false);
  LISPT print(LISPT, file_t&);

  static LISPT rmexcl(io&, file_t&, LISPT, char);
  static LISPT rmdquote(io&, file_t&, LISPT, char);
  static LISPT rmsquote(io&, file_t&, LISPT, char);
  static LISPT rmpipe(io&, file_t&, LISPT, char);
  static LISPT rmredir(io&, file_t&, LISPT, char);
  static LISPT rmbg(io&, file_t&, LISPT, char);
  static LISPT rmuser(io&, file_t&, LISPT, char);

#if 0
  static LISPT userreadmacros[128];
#endif
};

struct rtinfo
{
  unsigned char chclass[128];
  LISPT (*rmacros[128])(io&, file_t&, LISPT, char);
};

struct file_t
{
  file_t(io::source* source) : source(source) {}
  file_t(io::sink* sink) : sink(sink) {}
  ~file_t()
  {
    delete source;
    delete sink;
  }

  io::source* source = nullptr;
  io::sink* sink = nullptr;

  // io::source
  int getch() { return source->getch(); }
  void ungetch(int c) { source->ungetch(c); }
  bool eoln() { return source->eoln(); }
  const char* getline() { return source->getline(); }
  // io::sink
  void putch(char c, bool esc = false) { sink->putch(c, esc); }
  void puts(const char* s) { sink->puts(s); }

  void printf(const char* format, ...)
  {
    va_list ap;
    va_start(ap, format);
    char* ret;
    vasprintf(&ret, format, ap);
    va_end(ap);
    sink->puts(ret);
  }

  bool close()
  {
    if(source)
      return source->close();
    else if(sink)
      return sink->close();
    return false;
  }
};

extern struct rtinfo currentrt;

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

inline LISPT ratom(lisp& l, file_t& f) { return io(l).ratom(f); }
inline LISPT lispread(lisp& l, file_t& f, bool i = false) { return io(l).lispread(f, i); }
inline LISPT readline(lisp& l, file_t& f) { return io(l).readline(f); }
inline LISPT patom(lisp& l, LISPT a, file_t& f, int i) { return io(l).patom(a, f, i); }
inline LISPT terpri(lisp& l, file_t& f) { return io(l).terpri(f); }
inline LISPT prinbody(lisp& l, LISPT a, file_t& f, int i) { return io(l).prinbody(a, f, i); }
inline LISPT prin0(lisp& l, LISPT a, file_t& f, bool i = false) { return io(l).prin0(a, f, i); }
inline LISPT print(lisp& l, LISPT a, file_t& f) { return io(l).print(a, f); }

} // namespace lisp

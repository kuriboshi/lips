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
#include "except.hh"

namespace lisp
{
class lisp;

class io: public base
{
public:
  io(lisp& lisp): base(lisp) {}
  ~io() = default;
  static void init(lisp&);

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

  class file_source: public source
  {
  public:
    file_source(std::FILE* file): _file(file), _owner(false) {}
    file_source(const char* filename): _file(fopen(filename, "r")), _owner(true)
    {
      // TODO: Throw different exception
      if(!_file)
        throw lisp_error("Can't open file");
    }
    ~file_source()
    {
      if(_owner)
        fclose(_file);
    }

    virtual int getch() override
    {
      _curc = getc(_file);
      if(_curc == COMMENTCHAR) /* Skip comments.  */
        while((_curc = getc(_file)) != '\n')
          ;
      return _curc;
    }
    virtual void ungetch(int c) override { ungetc(c, _file); }
    virtual bool eoln() override
    {
      while(true)
      {
        if(feof(_file))
          return true;
        if(_curc != ' ' && _curc != '\t' && _curc != '\n')
          return false;
        if(_curc == '\n')
          return true;
        _curc = getc(_file);
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
    int _curc = 0;
  };

  class string_source: public source
  {
  public:
    string_source(const char* string): _string(string), _len(strlen(string)) {}

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
    virtual void terpri() = 0;
    virtual void flush() {}
    virtual bool close() = 0;
  };

  class file_sink: public sink
  {
  public:
    file_sink(std::FILE* file): _file(file) {}
    file_sink(const char* filename, bool append = false) { _file = fopen(filename, append ? "a" : "w"); }

    virtual void putch(int c, bool esc) override { putch(c, _file, esc); }
    virtual void puts(const char* s) override { std::fputs(s, _file); }
    virtual void terpri() override { putch('\n', _file, false); }
    virtual void flush() override { fflush(_file); }
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

  class string_sink: public sink
  {
  public:
    string_sink() {}

    std::string string() const { return _string; }

    virtual void putch(int c, bool) override { _string.push_back(static_cast<char>(c)); }
    virtual void puts(const char* s) override { _string.append(s); }
    virtual void terpri() override { _string.push_back('\n'); }
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

  static inline constexpr int NUL = '\0';
  static inline constexpr int MAXATOMSIZE = 128; // Max length of atom read can handle
  static inline constexpr char COMMENTCHAR = '#';

  bool integerp(char*, int* res);
  bool floatp(char*);

  LISPT splice(LISPT, LISPT, bool);
  LISPT parsebuf(char*);

  LISPT ratom(file_t&);
  LISPT lispread(file_t&, bool line = false);
  LISPT readline(file_t&);

  LISPT patom(LISPT, file_t&, bool esc = false);
  LISPT prinbody(LISPT, file_t&, bool esc = false);
  LISPT prin0(LISPT, file_t&, bool esc = false);
  LISPT print(LISPT, file_t&);
  LISPT terpri(file_t&);

  static LISPT rmexcl(lisp&, file_t&, LISPT, char);
  static LISPT rmdquote(lisp&, file_t&, LISPT, char);
  static LISPT rmsquote(lisp&, file_t&, LISPT, char);
  static LISPT rmpipe(lisp&, file_t&, LISPT, char);
  static LISPT rmredir(lisp&, file_t&, LISPT, char);
  static LISPT rmbg(lisp&, file_t&, LISPT, char);
  static LISPT rmuser(lisp&, file_t&, LISPT, char);

#if 0
  static LISPT userreadmacros[128];
#endif

private:
  bool checkeof(lisp& l, int c, bool line);
  std::pair<bool, int> getchar(lisp& l, file_t& file, bool line);
};

class file_t final
{
public:
  file_t(std::unique_ptr<io::source> source): _source(std::move(source)) {}
  file_t(std::unique_ptr<io::sink> sink): _sink(std::move(sink)) {}
  file_t(std::unique_ptr<io::source> source, std::unique_ptr<io::sink> sink): _source(std::move(source)), _sink(std::move(sink)) {}
  ~file_t() {}

  // io::source
  io::source& source() { return *_source.get(); }
  int getch() { ptrcheck(_source); return _source->getch(); }
  void ungetch(int c) { ptrcheck(_source); _source->ungetch(c); }
  bool eoln() { ptrcheck(_source); return _source->eoln(); }
  const char* getline() { ptrcheck(_source); return _source->getline(); }

  // io::sink
  io::sink& sink() { return *_sink.get(); }
  void putch(char c, bool esc = false) { ptrcheck(_sink); _sink->putch(c, esc); }
  void puts(const char* s) { ptrcheck(_sink); _sink->puts(s); }
  void terpri() { ptrcheck(_sink); _sink->terpri(); }
  void flush() { ptrcheck(_sink); _sink->flush(); }

  void printf(const char* format, ...)
  {
    va_list ap;
    va_start(ap, format);
    char* ret;
    vasprintf(&ret, format, ap);
    va_end(ap);
    _sink->puts(ret);
  }

  bool close()
  {
    _source.release();
    _sink.release();
    return true;
  }

private:
  std::unique_ptr<io::source> _source;
  std::unique_ptr<io::sink> _sink;

  void ptrcheck(const std::unique_ptr<io::source>&) const
  {
    if(!_source)
      throw lisp_error("file_t: No source");
  }
  void ptrcheck(const std::unique_ptr<io::sink>&) const
  {
    if(!_sink)
      throw lisp_error("file_t: No sink");
  }
};

inline bool issepr(lisp& l, int c) { return (l.currentrt.chclass[c] & SEPR) == SEPR; }
inline bool isbrk(lisp& l, int c) { return (l.currentrt.chclass[c] & BRK) == BRK; }
inline bool isrm(lisp& l, int c) { return (l.currentrt.chclass[c] & RMACRO) == RMACRO; }
inline bool isinsert(lisp& l, int c) { return (l.currentrt.chclass[c] & RMACRO) == INSERT; }
inline bool issplice(lisp& l, int c) { return (l.currentrt.chclass[c] & RMACRO) == SPLICE; }
inline bool isinfix(lisp& l, int c) { return (l.currentrt.chclass[c] & RMACRO) == INFIX; }

inline LISPT ratom(lisp& l, file_t& f) { return io(l).ratom(f); }
inline LISPT lispread(lisp& l, file_t& f, bool i = false) { return io(l).lispread(f, i); }
inline LISPT readline(lisp& l, file_t& f) { return io(l).readline(f); }
inline LISPT patom(lisp& l, LISPT a, file_t& f, int i) { return io(l).patom(a, f, i); }
inline LISPT terpri(lisp& l, file_t& f) { return io(l).terpri(f); }
inline LISPT prinbody(lisp& l, LISPT a, file_t& f, int i) { return io(l).prinbody(a, f, i); }
inline LISPT prin0(lisp& l, LISPT a, file_t& f, bool i = false) { return io(l).prin0(a, f, i); }
inline LISPT print(lisp& l, LISPT a, file_t& f) { return io(l).print(a, f); }

} // namespace lisp

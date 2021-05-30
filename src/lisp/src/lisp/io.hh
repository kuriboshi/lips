//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#pragma once

#include <optional>
#include <string>
#include <fmt/format.h>
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

  void pushr(LISPT w) { l.rstack = cons(l, w, l.rstack); }
  void popr(LISPT& w)
  {
    w = l.rstack->car();
    l.rstack = l.rstack->cdr();
  }

  static inline constexpr int NUL = '\0';
  static inline constexpr int MAXATOMSIZE = 128; // Max length of atom read can handle

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

class io_source
{
public:
  io_source() {}
  virtual ~io_source() = default;

  static inline constexpr char COMMENTCHAR = '#';

  virtual int getch(bool inside_string = false) = 0;
  virtual void ungetch(int) = 0;
  virtual bool eoln() = 0;
  virtual bool close() = 0;
  virtual std::optional<std::string> getline() = 0;
};

class file_source: public io_source
{
public:
  enum class mode_t
  {
    READ,
    WRITE,
    APPEND
  };

  file_source(std::FILE* file): _file(file), _owner(false) {}
  file_source(const std::string& filename, mode_t = mode_t::READ);
  ~file_source()
  {
    if(_owner)
      fclose(_file);
  }

  virtual int getch(bool inside_string) override
  {
    _curc = getc(_file);
    if(!inside_string && _curc == COMMENTCHAR) /* Skip comments.  */
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
  virtual std::optional<std::string> getline() override
  {
    char* buf = nullptr;
    std::size_t size = 0;
    auto rv = ::getline(&buf, &size, _file);
    if(rv == -1)
      return {};
    return std::string(buf);
  }

private:
  std::FILE* _file;
  bool _owner = false;
  int _curc = 0;
};

class string_source: public io_source
{
public:
  string_source(const char* string): _string(string) {}
  string_source(std::string string): _string(string) {}

  virtual int getch(bool inside_string) override
  {
    if(_pos == _string.length())
      return -1;
    auto c = _string[_pos++];
    if(!inside_string && c == COMMENTCHAR) /* Skip comments.  */
      while(_pos != _string.length() && (c = _string[_pos++]) != '\n')
        ;
    return c;
  }
  virtual void ungetch(int c) override { --_pos; }
  virtual bool eoln() override { return false; }
  virtual bool close() override { return true; }
  virtual std::optional<std::string> getline() override { return _string; }

private:
  std::string _string;
  std::size_t _pos = 0;
};

class io_sink
{
public:
  io_sink() {}
  virtual ~io_sink() = default;

  virtual void putch(int, bool esc = false) = 0;
  virtual void puts(const char*) = 0;
  virtual void terpri() = 0;
  virtual void flush() {}
  virtual bool close() = 0;
};

class file_sink: public io_sink
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
    // Need to generalize this
    if(c >= 0 && c < 0x20 && c != '\n' && c != '\r' && c != '\t' && c != '\a')
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

class string_sink: public io_sink
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

class file_t final
{
public:
  file_t(std::unique_ptr<io_source> source): _source(std::move(source)) {}
  file_t(std::unique_ptr<io_sink> sink): _sink(std::move(sink)) {}
  file_t(std::unique_ptr<io_source> source, std::unique_ptr<io_sink> sink): _source(std::move(source)), _sink(std::move(sink)) {}
  file_t(std::string source): _source(std::make_unique<string_source>(source)) {}
  ~file_t() {}

  // io_source
  io_source& source() { return *_source.get(); }
  int getch(bool inside_string = false) { ptrcheck(_source); return _source->getch(inside_string); }
  void ungetch(int c) { ptrcheck(_source); _source->ungetch(c); }
  bool eoln() { ptrcheck(_source); return _source->eoln(); }
  std::optional<std::string> getline() { ptrcheck(_source); return _source->getline(); }

  // io_sink
  io_sink& sink() { return *_sink.get(); }
  void putch(char c, bool esc = false) { ptrcheck(_sink); _sink->putch(c, esc); }
  void puts(const char* s) { ptrcheck(_sink); _sink->puts(s); }
  void terpri() { ptrcheck(_sink); _sink->terpri(); }
  void flush() { ptrcheck(_sink); _sink->flush(); }

  template<typename... Ts>
  void format(Ts&&... t)
  {
    auto ret = fmt::format(t...);
    _sink->puts(ret.c_str());
  }

  bool close()
  {
    _source.release();
    _sink.release();
    return true;
  }

private:
  std::unique_ptr<io_source> _source;
  std::unique_ptr<io_sink> _sink;

  void ptrcheck(const std::unique_ptr<io_source>&) const
  {
    if(!_source)
      throw lisp_error("file_t: No source");
  }
  void ptrcheck(const std::unique_ptr<io_sink>&) const
  {
    if(!_sink)
      throw lisp_error("file_t: No sink");
  }
};

inline bool isascii(int c) { return c >= 0 && c <= 127; }
inline bool issepr(lisp& l, int c) { return isascii(c) && l.currentrt.chclass[c] == char_class::SEPR; }
inline bool isbrk(lisp& l, int c) { return isascii(c) && l.currentrt.chclass[c] == char_class::BRK; }
inline bool isctrl(lisp& l, int c) { return isascii(c) && l.currentrt.chclass[c] == char_class::CTRL; }
inline bool isinsert(lisp& l, int c) { return isascii(c) && l.currentrt.chclass[c] == char_class::INSERT; }
inline bool issplice(lisp& l, int c) { return isascii(c) && l.currentrt.chclass[c] == char_class::SPLICE; }
inline bool isinfix(lisp& l, int c) { return isascii(c) && l.currentrt.chclass[c] == char_class::INFIX; }

inline LISPT ratom(lisp& l, file_t& f) { return io(l).ratom(f); }
inline LISPT ratom(file_t& f) { return io(lisp::current()).ratom(f); }
inline LISPT lispread(lisp& l, file_t& f, bool esc = false) { return io(l).lispread(f, esc); }
inline LISPT lispread(file_t& f, bool esc = false) { return io(lisp::current()).lispread(f, esc); }
inline LISPT readline(lisp& l, file_t& f) { return io(l).readline(f); }
inline LISPT readline(file_t& f) { return io(lisp::current()).readline(f); }

inline LISPT patom(lisp& l, LISPT a, file_t& f, bool esc = false) { return io(l).patom(a, f, esc); }
inline LISPT patom(LISPT a, file_t& f, bool esc = false) { return io(lisp::current()).patom(a, f, esc); }
inline LISPT patom(lisp& l, LISPT a, bool out = false, bool esc = false)
{
  return io(l).patom(a, out ? l.primerr() : l.primout(), esc);
}
inline LISPT patom(LISPT a, bool out = false, bool esc = false) { return patom(lisp::current(), a, out, esc); }
inline LISPT terpri(lisp& l, file_t& f) { return io(l).terpri(f); }
inline LISPT terpri(file_t& f) { return io(lisp::current()).terpri(f); }
inline LISPT terpri(lisp& l, bool out = false) { return io(l).terpri(out ? l.primerr() : l.primout()); }
inline LISPT terpri(bool out = false) { return terpri(lisp::current(), out); }
inline LISPT prinbody(lisp& l, LISPT a, file_t& f, bool esc = false) { return io(l).prinbody(a, f, esc); }
inline LISPT prinbody(LISPT a, file_t& f, bool esc = false) { return io(lisp::current()).prinbody(a, f, esc); }
inline LISPT prinbody(lisp& l, LISPT a, bool out = false, bool esc = false)
{
  return io(l).prinbody(a, out ? l.primerr() : l.primout(), esc);
}
inline LISPT prinbody(LISPT a, bool out = false, bool esc = false) { return prinbody(lisp::current(), a, out, esc); }
inline LISPT prin0(lisp& l, LISPT a, file_t& f, bool esc = false) { return io(l).prin0(a, f, esc); }
inline LISPT prin0(LISPT a, file_t& f, bool esc = false) { return io(lisp::current()).prin0(a, f, esc); }
inline LISPT prin0(lisp& l, LISPT a, bool out = false, bool esc = false)
{
  return io(l).prin0(a, out ? l.primerr() : l.primout(), esc);
}
inline LISPT prin0(LISPT a, bool out = false, bool esc = false) { return prin0(lisp::current(), a, out, esc); }
inline LISPT print(lisp& l, LISPT a, file_t& f) { return io(l).print(a, f); }
inline LISPT print(LISPT a, file_t& f) { return io(lisp::current()).print(a, f); }
inline LISPT print(lisp& l, LISPT a, bool out = false) { return io(l).print(a, out ? l.primerr() : l.primout()); }
inline LISPT print(LISPT a, bool out = false) { return print(lisp::current(), a, out); }

} // namespace lisp

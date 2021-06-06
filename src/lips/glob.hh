/*
 * Lips, lisp shell.
 * Copyright 2020 Krister Joas
 *
 */

#pragma once

#include <doctest/doctest.h>
#include <lisp/libisp.hh>
#include <optional>
#include <string>
#include <filesystem>

class glob: public lisp::base
{
public:
  glob(lisp::lisp& lisp): base(lisp) {}
  static void init();
  using LISPT = lisp::LISPT;

  void test();

  static std::optional<std::string> extilde(const std::string& w, bool report);
  static LISPT expandfiles(const std::string& wild, bool all, bool report, bool sort);

  LISPT expand(LISPT wild, LISPT rep, LISPT all);
  LISPT expand(LISPT);

private:
  static bool dircheck(const std::string&, const std::string&, const std::string&);
  static bool match(const std::string&, const std::string&);
  static LISPT orderinsert(LISPT, LISPT);
  static std::vector<std::string> walkfiles(
    const std::filesystem::path& root, const std::string& wild, bool all, bool report);
  static LISPT buildlist(const std::vector<std::string>&);

  static void _test_dircheck();
  static void _test_match();
  static void _test_orderinsert();
  static void _test_extilde();
  static void _test_walkfiles();
  static void _test_expandfiles();
  TEST_CASE_CLASS("glob.cc: dircheck") { _test_dircheck(); }
  TEST_CASE_CLASS("glob.cc: match") { _test_match(); }
  TEST_CASE_CLASS("glob.cc: orderinsert") { _test_orderinsert(); }
  TEST_CASE_CLASS("glob.cc: extilde") { _test_extilde(); }
  TEST_CASE_CLASS("glob.cc: walkfiles") { _test_walkfiles(); }
  TEST_CASE_CLASS("glob.cc: expandfiles") { _test_expandfiles(); }
};

inline lisp::LISPT expand(lisp::lisp& l, lisp::LISPT a, lisp::LISPT b, lisp::LISPT c)
{
  return glob(l).expand(a, b, c);
}
inline lisp::LISPT expand(lisp::LISPT a, lisp::LISPT b, lisp::LISPT c)
{
  return glob(lisp::lisp::current()).expand(a, b, c);
}
inline lisp::LISPT expand(lisp::lisp& l, lisp::LISPT a) { return glob(l).expand(a); }
inline lisp::LISPT expand(lisp::LISPT a) { return glob(lisp::lisp::current()).expand(a); }

# Release notes

## 2.0.0

Major refactoring and reorganization of the source code.

- Added a simple `Makefile` as a front end to CMake
- Added gitlab ci file
- Added some benchmark code
- Added support Macs with an M1 or M2 CPU
- Added support for coverage with `gcov`/`lcov` on Linux
- Added test for using lips as a library from CMake
- CMake version 3.21 is now required
- Changed type of integer from `int` to `std::int64_t`
- Function coverage is now at 100% (as measured by clang)
- Moved all type definitions to `types.hh`
- Moved enum `lisp::type` to `lisp::object::type`
- Moved `eval`/`vm` related items from `context` to `vm`
- Reduced size of `object` by putting more value types in their own objects
- Removed all object types which don't have values from `object`
- Removed the `_type` member variable from `object` (variant maps one
  to one to type)
- Removed the `lisp` (`context`) parameter from most primitive functions
- Renamed `baktrace` to `backtrace`
- Renamed `LISPT` to `lisp_t`
- Renamed `eval` to `vm`
- Renamed `lisp` object to `context`
- Renamed `lisp_t` to `object`
- Renamed some vm continuation functions to be more understandable
- Simplified symbol storage since we now use ref counted `symbol_t` objects
- The file `lisp.hh` now only includes all other header files
- Updated Catch2 to version 3.2.1
- Updated fmt to version 9.1.0
- Use a memory pool for more objects
- Use `std::error_code` for error messages
- Use `std::string_view` in more places

## 1.0.4

- Turn on CMake policy CMP0135

## 1.0.3

- Fixed how lips is used as a library
- Prefix all read macro functions with `rm`
- Replace `cmake_external` with straight `FetchContents`
- Split some header files into `details`; this makes it possible to
  change the namespace name `Map` to `map`

## 1.0.2

- Added support for FreeBSD
- Changed internal `eval::pop` function to take a parameter in order
  to select the right template
- Prefixed TeX files with a number to keep them sorted
- Renamed `absval` to `abs`
- Use `cmake_external` when introducing external library dependencies

## 1.0.1

- Added clang-tidy rules, cmake preset, and target
- Added cmake-format target
- Added copyright notices and license to some files missing them
- Applied some of the corrections suggested by clang-tidy
- Formatted files using cmake-format
- Included the complete text of the Apache 2.0 license

## 1.0.0

First release of C++ version of lips. Here are a few highlights of the
changes.

- Added .gitignore
- Added Apache 2.0 license
- Added GitHub action for CodeQL
- Added a lexer/parser that can handle super parenthesis
- Added clang-format rules
- Added coverage report using llvm-cov
- Added docker containers to test on different versions of Ubuntu
- Added simple lisp test suite
- Added test cases to verify the C++ rewrite is true to the original
- Converted to use CMake for building
- Fixed several bugs along the way
- Removed support for TOPS-20 and small, 16-bit machines
- Removed support for printing C++ source code from lisp
- Replaced the nroff/troff manual with one written in TeX
- Use `defineq` instead of `de` and `df`
# Release notes

## 3.3.0 (2024-04-28)

Minor release with the following changes.

- Drop testing on Ubuntu 20.04
- Add testing on Ubuntu 24.04
- Add testing on Fedora 40
- Change the signature of most lisp functions to take arguments as const lisp_t&
- Breaking change: Rename getenviron to getenv
- Move all constants to the `atoms` namespace and remove the `C_` prefix

## 3.2.0 (2024-01-13)

Minor release with the following changes.

- Upgrade compiler in github workflow on Ubuntu 22.04 to g++-13
- Replace the use of FILE* with std::streams in `src/lips`
- Put user define literals in the 'inline namespace literals'

## 3.1.1 (2024-01-08)

Minor bug fix release

- Improve accuracy of gcc coverage
- Fixes for gitlab CI/CD builds
- Fix missing header files for gcc-12

## 3.1.0 (2023-12-26)

Minor release mainly revising the documentation.

- Generate external documentation from in source documentation
- Add more in source documentaiton
- Add Doxygen configuration file
- Add 'splice' function
- Fix crash when `readline` is called without parameters
- Fix 'test-linux' build target
- Treat symbols evaluating to themselves as errors

## 3.0.0 (2023-12-22)

Major release due to a number of API level changes

- Make it possible to register functions with any fixed number of
  arguments
- Primitives can now accept const references as arguments
- Turn on clang-tidy for all header files (minimum version is 17)
- Turn on many more clang-tidy warnings and fix them
- Rename some accessor functions:
  - intval -> as_integer
  - floatval -> as_double
  - symbol -> as_symbol
  - string -> as_string
- Rename ltimes -> times
- The vm class now owns the Context using unique_ptr
- Move file_t class from io.hh to file.hh
- Introduced some use of C++-20 concepts
- Add more Doxygen style comments
- Switch from LaTeX to Markdown for the reference manual
- Upgrade Catch2 to 3.5.0
- Upgrade fmt to 10.1.1

## 2.0.3 (2023-08-20)

Minor bug fix release

- Turn on some more clang-tidy warnings and fix them
- Upgrade cmake-external to v1.1.0
- Build on local filesystem for the test-linux target
- Separate job related code in exec.cc into job.[cc,hh]

## 2.0.2 (2023-07-23)

Minor release to fix some build issues

- build: Fix issues on macOS using the Xcode generator
- tidy: Fix issues raised by clang-tidy
- build: Fix building on gitlab

## 2.0.1 (2023-07-16)

- build: Use cmake-external for external dependencies (downloaded from external repo)
- Upgrade Catch2 to version 3.3.2
- test: Remove the use of sshfs on macOS

## 2.0.0 (2023-02-26)

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

## 1.0.4 (2022-08-23)

- Turn on CMake policy CMP0135

## 1.0.3 (2022-07-18)

- Fixed how lips is used as a library
- Prefix all read macro functions with `rm`
- Replace `cmake_external` with straight `FetchContents`
- Split some header files into `details`; this makes it possible to
  change the namespace name `Map` to `map`

## 1.0.2 (2022-07-16)

- Added support for FreeBSD
- Changed internal `eval::pop` function to take a parameter in order
  to select the right template
- Prefixed TeX files with a number to keep them sorted
- Renamed `absval` to `abs`
- Use `cmake_external` when introducing external library dependencies

## 1.0.1 (2022-06-21)

- Added clang-tidy rules, cmake preset, and target
- Added cmake-format target
- Added copyright notices and license to some files missing them
- Applied some of the corrections suggested by clang-tidy
- Formatted files using cmake-format
- Included the complete text of the Apache 2.0 license

## 1.0.0 (2022-06-15)

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

# Lips — lisp shell

Copyright 1988-1989, 1992, 2020-2025 Krister Joas <krister@joas.jp>

![Ubuntu 24.04](https://github.com/kuriboshi/lips/actions/workflows/ubuntu-24.04.yml/badge.svg)
![CodeQL Analysis](https://github.com/kuriboshi/lips/actions/workflows/codeql-analysis.yml/badge.svg)

Version 3.4.0 (2025-04-05)

## What is it?

`lips` is a lisp interpreter with some functions to make it work as a
shell for a Unix system.  The project has been dormant since my
university days until I needed an alternative to XML or JSON for
storing and processing structured data.

The old C code has been modernised and gradually parts have been
converted to C++.  Originally the lisp shell part was meant to be a
separate extension to a more generic, embeddable lisp interpreter.
However, that goal was never fully realised.  That goal is now a
priority and I view the shell extension as an example of how the
embeddable lisp interpreter can be used.

Don't expect a solid, or even useful, program or library.  I wrote
this as a learning exercise and to scratch an itch.  The shell
functionality is minimal and serves more as a proof of concept.

## LISP Dialect

The LISP dialect implemented doesn't strictly follow any existing LISP
implementation or standard. It's closer to Interlisp[1] in spirit than
Common Lisp[2]. There are some influences from other dialects such as
Scheme[3].

The interpreter uses shallow, dynamic binding. Originally it used a
stop-and-sweep garbage collector but now uses reference counting. The
implementation was heavily influenced by the work of John Allen as
published in the book _Anatomy of LISP_[4].

For more details see [Reference Manual](docs/reference.md).

## Requirements

The code requires C++-23 and CMake 3.21 or newer.

### Platforms

The project is developed primarily on macos 15 (Sequoia) using the
Apple clang version 17 compiler.

At the time of writing `lips` has been tested on the following
platforms and compilers. Older platforms may work as long as you
install a new enough compiler. Older compilers may also work depending
on their level of support for C++-23.

| OS Version   | Compiler Version  |
|--------------|-------------------|
| macos 15.4   | AppleClang 17.0.0 |
| macos 15.4   | llvm-clang 20.1.1 |
| Ubuntu 22.04 | gcc 13.1.0        |
| Ubuntu 24.04 | gcc 13.2.0        |
| Ubuntu 24.04 | clang 19.1.6      |
| Fedora 40    | gcc 14.2.1        |
| Fedora 41    | gcc 14.2.1        |
| Fedora 41    | clang 19.1.5      |
| Alpine 3.21  | gcc 14.2.0        |

The minimum version of `clang-tidy` is 17.

### External Dependencies

`lips` depends on two external libraries which are downloaded and
built as part of the build.

| Library Name | Version |
|--------------|---------|
| Catch2       | 3.8.1   |
| fmt          | 11.1.4  |

System library dependencies.

| Library Name |
|--------------|
| ncurses      |

System tool dependencies.

| Tool name | Remark                      |
|-----------|-----------------------------|
| ninja     |                             |
| cmake     | Minimum version is 3.21.0   |

## Building

Building is done in two phases using a combination of `make` and
`cmake` to drive the build. Using `make` is optional and is only a
front end to `cmake`.

In the first phase a minimal build environment is created. This
environment is then used to build several different targets described
below.

There is a `Makefile` in the top directory which runs `cmake` and
`ctest` for some scenerios.

| Make Target      | Description                                 |
|------------------|---------------------------------------------|
| `all` or `debug` | Debug build                                 |
| `release`        | Optimized build                             |
| `clang`          | Build with `clang` (Linux)                  |
| `llvm`           | Build with `llvm` version of clang (macos)  |
| `tidy`           | Run `clang-tidy` checks                     |
| `xcode`          | Create a configuration for Xcode (no build) |
| `coverage`       | Produce a coverage report                   |
| `format`         | Run `clang-format` on the source files      |
| `benchmark`      | Run benchmarks                              |
| `package-test`   | Test using `lips` as an external library    |
| `test-linux`     | Run all Linux tests in docker               |
| `copyright`      | Update copyright notices in source files    |
| `docs`           | Update the documentation from source files  |
| `ubuntu22`       | Build in docker on Ubuntu 22.04             |
| `ubuntu24`       | Build in docker on Ubuntu 24.04             |
| `ubuntu24-clang` | Build in docker on Ubuntu 24.04 with clang  |
| `fedora40`       | Build in docker on Fedora 40                |
| `fedora41`       | Build in docker on Fedora 41                |
| `fedora41-tidy`  | Run clang-tidy in docker on Fedora 41       |
| `fedora41-clang` | Build in docker on Fedora 41 with clang     |
| `alpine`         | Build in docker on Alpine 3.21              |

Using the two phase build is not strictly necessary and the project
can be configured and built using `cmake` directly. There are several
presets defined in `CMakePresets.json`. The one called `default`
builds a debug version of the project.

```sh
cmake --preset default
cmake --build --preset default
```

There are a number of options available to customize the build.

| Option                      | Description                     | Default value |
| --------------------------- | ------------------------------- | ------------- |
| `LIPS_ENABLE_TRACE`         | Enable tracing                  | `ON`          |
| `LIPS_ENABLE_CODE_COVERAGE` | Enable code coverage            | `OFF`         |
| `LIPS_ENABLE_TESTS`         | Enable building test code       | `OFF`         |
| `LIPS_ENABLE_BENCHMARK`     | Enable building benchmark code  | `OFF`         |
| `LIPS_ENABLE_CLANG_TIDY`    | Enable checking with clang-tidy | `OFF`         |

Turn them on by adding `-D` options to the `cmake` command or choose
one of the appropriate `cmake` presets.

## Unit Testing

Native testing on the host is triggered by the `ctest` command, either
using `make` or `ctest` directly.

```sh
make test
```

or

```sh
ctest --preset default
```

### GitHub workflows

Builds and tests using GitHub actions are included in the
`.github/workflows` directory. There are workflows for Ubuntu 24.04 as
well as one for CodeQL.

### Local testing

For local testing there are some docker files in the `test` directory
which can be used for testing on various platforms.  Runing the
following command builds and tests on all the above Linux operating
systems and compiler combinations. Individual Linux versions can be
tested separately using the `make` targets listed above.

```sh
make test-linux
```

or

```sh
cmake --preset default --target test-linux
```

On macos `podman` is used instead of `docker`. Install `podman` and
start the VM. From then on the target `test-linux` should work the
same as for `docker`.

I you use `podman` on macos, make sure you allocate enough memory to
the VM. I've had success with 4 CPU's and 4GB of memory.

## Implementation Notes

Lips does not use a garbage collector. Instead it uses reference
counting implemented by an intrusive pointer. The reference counter is
in the reference counted object itself by deriving from
`lisp::ref_count`. The intrusive smart pointer controlling the
reference counted object (let's say T) derive from `lisp::ref_ptr<T>`.

The `lisp::object` class represents the type which can store a value
of any type. It's implemented using `std::variant` which is not the
most efficient solution. The overhead is more than just a tag.

Most types stored in the `lisp::object` class are implemented using
pointers to instances which are allocated using a memory
pool. Benchmarks show allocation is almost 10 times faster than not
using a memory pool for an optimized build. Here is an example of a
test run on a Mac mini (2018). Benchmarks are run using `make
benchmark.`

```text
1: benchmark name           samples       iterations    estimated
1:                          mean          low mean      high mean
1:                          std dev       low std dev   high std dev
1: ------------------------------------------------------------------
1: pool: speed(Foo, 100)              100            27     4.5117 ms
1:                             1.63464 us     1.6339 us    1.63637 us
1:                             5.39377 ns    1.87873 ns     9.6492 ns
1:
1: pool: speed(Bar, 100)              100             4     5.5724 ms
1:                              13.815 us    13.6444 us    14.4859 us
1:                             1.48375 us    324.523 ns    3.43163 us
1:
1: pool: speed(Foo, 500)              100             6      4.836 ms
1:                             8.10305 us    8.08126 us    8.16919 us
1:                             175.717 ns    66.6933 ns    382.204 ns
1:
1: pool: speed(Bar, 500)              100             1     7.2249 ms
1:                             72.5547 us     72.499 us    72.6542 us
1:                             368.012 ns    239.182 ns    676.828 ns
1:
1: pool: speed(Foo, 1000)             100             3     5.0205 ms
1:                             17.0331 us    17.0165 us    17.0757 us
1:                             121.435 ns    21.1429 ns    242.825 ns
1:
1: pool: speed(Bar, 1000)             100             1    15.2096 ms
1:                             149.443 us     147.86 us    153.676 us
1:                             11.5088 us    1.29109 us    23.4167 us
```

## References

- [1] _Interlisp Reference Manual_, Xerox, 1983.
- [2] Guy L. Steele, _Common Lisp the Language_, 2nd edition, Digital
Press, 1990.
- [3] H. Abelson, G. Sussman, and J. Sussman, _Structure and
Interpretation of Computer Programs_, MIT Press, 1985.
- [4] Allen, John, _Anatomy of LISP_, McGraw-Hill, Inc, 1978.

## License

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

[Apache License, Version 2.0](http://www.apache.org/licenses/LICENSE-2.0)

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

# Lips — lisp shell

Copyright 1988-1989, 1992, 2020-2023 Krister Joas <krister@joas.jp>

![Ubuntu 20.04](https://github.com/kuriboshi/lips/actions/workflows/ubuntu-20.04.yml/badge.svg)
![Ubuntu 22.04](https://github.com/kuriboshi/lips/actions/workflows/ubuntu-22.04.yml/badge.svg)
![CodeQL Analysis](https://github.com/kuriboshi/lips/actions/workflows/codeql-analysis.yml/badge.svg)

Version 2.0.0

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

## Requirements

The code requires C++20 and CMake 3.21 or newer.

## Platforms

The project is developed primarily on macOS 13 (Ventura) using the
Apple clang version 14.0.0 (clang-1400.0.29.202) compiler.  It may
work on older releases of macOS if you install a newer compiler.  It's
tested regulartly on Ubuntu 22.04 and 20.04. On Ubuntu testing is done
using gcc 11.x, gcc 12.x and clang 14.x.

## Building

Configure and build using `cmake`. The `default` preset uses `ninja`
to build.

```
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
| `LIPS_USE_SSHFS`            | Use sshfs on macOS podman       | `OFF`         |

Turn them on by adding `-D` options to the `cmake` command.

There is a Makefile in the top directory which runs cmake and ctest
for some common scenerios.

## External dependencies

Lips depends on two external libraries.

- Catch2 (version 3.2.1)
- fmt (version 9.1.0)

These dependencies are downloaded and included in the build
automatically.

## Testing

Native testing on the host is triggered by the `ctest` command. For example

```
ctest --preset default
```

### GitHub workflows

Builds and tests using GitHub actions are included in the
`.github/workflows` directory. There are workflows for both Ubuntu 18
and 20.

### Local testing

For local testing there are some docker files in the `test` directory
which can be used for testing on various platforms. Currently there
are files for Ubuntu 20.04 and 22.04. The latter also has a variant
for clang. For 20.04 the version of gcc is 11 and for 22.04 it's gcc
12. For 22.04 and clang the version of clang is clang 14.

Runing the following command builds and tests on all four operating
systems and compiler combinations.

```
cmake --preset default --target test-linux
```

On macOS it's currently a little more involved. Instead of `docker` it
uses `podman`. Building using this method works but there is a bug in
`qemu` which prevents running the tests
([see this `qemu` bug report](https://gitlab.com/qemu-project/qemu/-/issues/1010)).

You can work around this by using `sshfs` instead by following the
steps in [this
post](https://dalethestirling.github.io/Macos-volumes-with-Podman/).

To summarize what you need to do to use `sshfs`. Get the port where
the `podman` VM listens to `ssh`.

```
podman machine --log-level=debug ssh -- exit 2>&1 | grep Executing | awk {'print $8'}
```

Replace `<PORT>` with the port printed by the previous
command. Replace <USERNAME> with your username on the Mac.

```
# Connect to the host.
ssh -i ~/.ssh/podman-machine-default -R 10000:$(hostname):22 -p <PORT> core@localhost

# Generate an SSH key and share it back to the macOS host.
ssh-keygen -t rsa -N "" -f ~/.ssh/id_rsa
ssh-copy-id -p 10000 <USERNAME>@127.0.0.1

# Create mount point for the host filesystem. This needs to match the
# path to where the source code is kept, without the /mnt/ part.
sudo mkdir -p /mnt/Users
sudo chown core:core /mnt/Users
```

Run the `sshfs` command. Again, <USERNAME> is the username of the Mac
user.

```
sshfs -p 10000 <USERNAME>@127.0.0.1:/Users /mnt/Users
```

After this is done configure the build with

```
cmake --preset default -D LIPS_USE_SSHFS=ON
```

From then on the target `test-linux` should work automatically.

## References

[1] _Interlisp Reference Manual_, Xerox, 1983.<br>
[2] Guy L. Steele, _Common Lisp the Language_, 2nd edition, Digital Press, 1990.<br>
[3] H. Abelson, G. Sussman, and J. Sussman, _Structure and Interpretation of Computer Programs_, MIT Press, 1985.<br>
[4] Allen, John, _Anatomy of LISP_, McGraw-Hill, Inc, 1978.

# License

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

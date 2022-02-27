Lips -- lisp shell

Copyright 1988-1989, 1992, 2020-2022 Krister Joas

# What is it?

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

# Requirements

The code requires C++17 and CMake 3.18 or newer. The CMakePresets.json
file requires CMake 3.21.0 or newer.

# Platforms

The project has been successfully compiled on MacOS 10.15 (Catalina),
FreeBSD 12.1, and Ubuntu 19.10.

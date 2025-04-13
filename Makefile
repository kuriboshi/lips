# -*- makefile-gmake -*-

all: debug

TARGETS := debug release clang llvm tidy

# Homebrew on mac is located in different places depending on CPU architecture.
ARCH := $(shell uname -m)
ifeq ($(ARCH),arm64)
  export HOMEBREW_ROOT := /opt/homebrew
else
  export HOMEBREW_ROOT := /usr/local
endif

# First stage build.
.PHONY: build/build
build/build:
	cmake --preset build

# Standard build types.
.PHONY: debug release clang llvm tidy
debug release clang llvm tidy: build/build
	cmake --build --preset build --target $@

# Create a configuration suitable to use with Xcode.
.PHONY: xcode
xcode:
	cmake --preset xcode

# Produce a coverage report.
.PHONY: coverage
coverage: debug
	cmake --build --preset debug --target coverage

# Format source code using clang-format
.PHONY: format
format: build/build
	cmake --build --preset build --target format

# Run the unit tests.
.PHONY: test
test: debug
	ctest --preset debug --output-on-failure

# Update copyright notices
.PHONY: copyright
copyright:
	./docs/copyright.sh

# Update documentation
.PHONY: docs
docs:
	./docs/make_docs.py

# Run the benchmark tests.
.PHONY: benchmark
benchmark: release
	ctest --preset benchmark

# Test using lips as a package via CMake FetchContent.
.PHONY: package-test
package-test:
	(cd test/package; cmake -G Ninja -B ../../build/package-test .)
	(cd test/package; cmake --build ../../build/package-test)
	(cd build/package-test; ctest)

# Test different Linux configurations:
#   OS            Compiler
#   Ubuntu 22.04  gcc-13
#   Ubuntu 24.04  gcc-14
#   Ubuntu 24.04  clang
#   Fedora 40	  gcc-14
#   Fedora 41	  gcc-14
#   Fedora 41	  tidy
#   Fedora 41	  clang
#   Alpine 3.21   gcc
.PHONY: container
container:
	(cd test; cmake -G Ninja -B ../build/container .)

.PHONY: test-linux
test-linux: container
	cmake --build --preset container --target test-linux

.PHONY: ubuntu22 ubuntu24 ubuntu24-clang fedora40 fedora41 fedora41-tidy fedora41-clang alpine
ubuntu22 ubuntu24 ubuntu24-clang fedora40 fedora41 fedora41-tidy fedora41-clang alpine: container
	cmake --build build/container --target $@

all: debug

# Default debug build.
.PHONY: debug.config
.PHONY: debug
debug.config: build/debug/CMakeCache.txt

build/debug/CMakeCache.txt:
	cmake --preset debug

debug: debug.config
	cmake --build --preset debug

# Optimized build.
.PHONY: release.config
.PHONY: release
release.config: build/release/CMakeCache.txt

build/release/CMakeCache.txt:
	cmake --preset release

release: release.config
	cmake --build --preset release

# Build with the clang compiler.
.PHONY: clang.config
.PHONY: clang
clang.config: build/clang/CMakeCache.txt

build/clang/CMakeCache.txt:
	cmake --preset clang

clang: clang.config
	cmake --build --preset clang

# Build with the llvm clang compiler
.PHONY: llvm.config
.PHONY: llvm
llvm.config: build/llvm/CMakeCache.txt

build/llvm/CMakeCache.txt:
	cmake --preset llvm

llvm: llvm.config
	cmake --build --preset llvm

# Build using clang-tidy to analyse the code.
.PHONY: tidy.config
.PHONY: tidy
tidy.config: build/tidy/CMakeCache.txt

build/tidy/CMakeCache.txt:
	cmake --preset tidy

tidy: tidy.config
	cmake --build --preset tidy

# Create a configuration suitable to use with Xcode.
.PHONY: xcode
xcode:
	cmake --preset xcode

# Produce a coverage report.
.PHONY: coverage
coverage: debug.config
	cmake --build --preset debug --target coverage

# Format source code using clang-format
.PHONY: format
format: debug.config
	cmake --build --preset debug --target format

# Update copyright notices
.PHONY: copyright
copyright:
	./docs/copyright.sh

# Test different Linux configurations:
#   OS            Compiler
#   Ubuntu 22.04  gcc-13
#   Ubuntu 24.04  gcc-14
#   Ubuntu 24.04  clang
#   Fedora 40	  gcc-14
#   Fedora 41	  gcc-14
#   Fedora 41	  tidy
#   Fedora 41	  clang
.PHONE: test-linux
test-linux:
	cmake --build --preset debug --target test-linux

# Run the unit tests.
.PHONY: test
test: debug
	ctest --preset default --output-on-failure

.PHONY: ubuntu24-clang fedora41-tidy fedora41-clang
ubuntu24-clang fedora41-tidy fedora41-clang:
	cmake --build --preset debug --target $@

.PHONY: ubuntu22 ubuntu24
ubuntu22 ubuntu24:
	cmake --build --preset debug --target $@-docker

.PHONY: fedora40 fedora41
fedora40 fedora41:
	cmake --build --preset debug --target $@-docker

.PHONY: alpine
alpine:
	cmake --build --preset debug --target $@-docker

# Run the benchmark tests.
.PHONY: benchmark
benchmark: release
	ctest --preset benchmark

# Test using lips as a package via CMake FetchContent.
.PHONY: package-test
package-test:
	(cd test; cmake -G Ninja -B ../build/package-test .)
	(cd test; cmake --build ../build/package-test)
	(cd build/package-test; ctest)

.PHONY: docs
docs:
	./docs/make_docs.py

all: debug
everything: test release clang tidy test-linux ubuntu24-clang fedora41-tidy fedora41-clang

# Default debug build.
.PHONY: debug
debug: build/debug/configured
	cmake --build --preset $@
build/debug/configured:
	cmake --preset debug
	touch $@

# Optimized build.
.PHONY: release
release: build/release/configured
	cmake --build --preset $@
build/release/configured:
	cmake --preset release
	touch $@

# Build with the clang compiler.
.PHONY: clang
clang: build/clang/configured
	cmake --build --preset $@
build/clang/configured:
	cmake --preset clang
	touch $@

# Build with the llvm clang compiler (Apple only)
.PHONY: llvm
llvm: build/llvm/configured
	cmake --build --preset $@
build/llvm/configured:
	cmake --preset llvm
	touch $@

# Build using clang-tidy to analyse the code.
.PHONY: tidy
tidy: build/tidy/configured
	cmake --build --preset $@
build/tidy/configured:
	cmake --preset tidy
	touch $@

# Create a configuration suitable to use with Xcode.
.PHONY: xcode
xcode:
	cmake --preset xcode

# Produce a coverage report.
.PHONY: coverage
coverage: build/debug/configured
	cmake --build --preset debug --target coverage

# Format source code using clang-format
.PHONY: format
format: build/debug/configured
	cmake --build --preset debug --target format

# Run the unit tests.
.PHONY: test
test: debug
	ctest --preset debug --output-on-failure

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
#   Alpine 3.21   gcc
.PHONY: container
container:
	(cd test; cmake -G Ninja -B ../build/container .)

.PHONY: test-linux
test-linux: container
	cmake --build --preset container --target test-linux

.PHONY: ubuntu22 ubuntu24 fedora40 fedora41 alpine ubuntu24-clang fedora41-tidy fedora41-clang
ubuntu22 ubuntu24 fedora40 fedora41 alpine ubuntu24-clang fedora41-tidy fedora41-clang: container
	cmake --build build/container --target $@

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

.PHONY: docs
docs:
	./docs/make_docs.py

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
#   Ubuntu 20.04  gcc-11
#   Ubuntu 22.04  gcc-12
#   Ubuntu 22.04  clang-14
#   Ubuntu 22.04  clang-tidy
.PHONE: test-linux
test-linux:
	cmake --build --preset debug --target test-linux

# Run the unit tests.
.PHONY: test
test: debug
	ctest --preset default

ubuntu20 ubuntu22 ubuntu22-tidy ubuntu22-clang:
	cmake --build --preset debug --target $@

# Run the benchmark tests.
.PHONY: benchmark
benchmark: release
	ctest --preset benchmark

# Test using lips as a package via CMake FetchContent.
.PHONY: package_test
package_test:
	(cd test; cmake -G Ninja -B ../build/package_test .)
	(cd test; cmake --build ../build/package_test)
	(cd build/package_test; ctest)

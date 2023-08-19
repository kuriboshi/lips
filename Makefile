all: debug

.PHONY: debug.config
.PHONY: debug
.PHONY: release.config
.PHONY: release
.PHONY: clang.config
.PHONY: clang
.PHONY: tidy.config
.PHONY: tidy
.PHONY: xcode

debug.config: build/debug/CMakeCache.txt

build/debug/CMakeCache.txt:
	cmake --preset debug

debug: debug.config
	cmake --build --preset debug

release.config: build/release/CMakeCache.txt

build/release/CMakeCache.txt:
	cmake --preset release

release: release.config
	cmake --build --preset release

clang.config: build/clang/CMakeCache.txt

build/clang/CMakeCache.txt:
	cmake --preset clang

clang: clang.config
	cmake --build --preset clang

tidy.config: build/tidy/CMakeCache.txt

build/tidy/CMakeCache.txt:
	cmake --preset tidy

tidy: tidy.config
	cmake --build --preset tidy

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

# Test different Linux configurations:
#   OS            Compiler
#   Ubuntu 20.04  gcc-11
#   Ubuntu 22.04  gcc-12
#   Ubuntu 22.04  clang-14
#   Ubuntu 22.04  clang-tidy
.PHONE: test-linux
test-linux:
	cmake --build --preset debug --target test-linux

.PHONE: test
test: debug
	ctest --preset default

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

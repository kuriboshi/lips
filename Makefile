all: debug

.PHONY: debug.config
.PHONY: debug
.PHONY: release.config
.PHONY: release
.PHONY: xcode
.PHONY: tidy.config

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

xcode:
	cmake --preset xcode

tidy.config: build/clang-tidy/CMakeCache.txt

build/clang-tidy/CMakeCache.txt:
	cmake --preset clang-tidy

.PHONY: coverage
.PHONY: format
.PHONY: tidy
.PHONE: test-linux
.PHONE: test
.PHONY: benchmark

coverage: debug.config
	cmake --build --preset debug --target coverage

format: debug.config
	cmake --build --preset debug --target format

tidy: tidy.config
	cmake --build --preset clang-tidy

test-linux:
	cmake --build --preset debug --target test-linux

test: debug
	ctest --preset default

benchmark: release
	ctest --preset benchmark

.PHONY: package_test

package_test:
	(cd test; cmake -G Ninja -B ../build/package_test .)
	(cd test; cmake --build ../build/package_test)
	(cd build/package_test; ctest)

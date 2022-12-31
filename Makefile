all: debug

.PHONY: debug.config
.PHONY: debug
.PHONY: release.config
.PHONY: release
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

xcode:
	cmake --preset xcode

.PHONY: coverage
.PHONY: format
.PHONE: test-linux
.PHONE: test
.PHONY: benchmark

coverage: debug.config
	cmake --build --preset debug --target coverage

format: debug.config
	cmake --build --preset debug --target format

test-linux:
	cmake --build --preset debug --target test-linux

test: debug
	ctest --preset default

benchmark: release
	ctest --preset benchmark

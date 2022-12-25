all: debug

.PHONY: debug.config
.PHONY: debug
.PHONY: coverage
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

coverage: debug.config
	cmake --build --preset debug --target ccov

format: debug.config
	cmake --build --preset debug --target format

test: debug
	ctest --preset default

xcode:
	cmake --preset xcode

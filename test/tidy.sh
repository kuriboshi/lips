#!/bin/sh

cmake --preset clang-tidy -B "$1-tidy" &&
    cmake --build "$1-tidy"

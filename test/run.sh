#!/bin/sh

cmake --preset docker -B "$1" &&
    cmake --build "$1" &&
    (cd "$1"; ctest -V)

#!/bin/sh

cmake --preset "$1" -B /project/build -S /project/lips &&
    cmake --build /project/build &&
    (cd /project/build; ctest --verbose -R 'test_.*')

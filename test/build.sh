#!/bin/sh

case "$1" in
    -t)
        test=/bin/true
        shift
        ;;
    *)
        test=/bin/false
        ;;
esac

cmake --preset docker -B "$1" &&
    cmake --build "$1" &&
    {
        if $test
        then
            (cd "$1"; ctest -V -R 'test_.*')
        else
            true
        fi
    }

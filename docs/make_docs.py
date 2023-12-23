#!/usr/bin/env python3

from pathlib import Path
import fileinput
import glob
import re
from enum import Enum

class State(Enum):
    NONE = 0
    FILE = 1
    HEADER = 2

class Signature(object):
    def __init__(self):
        self.data = []
        self.active = True

    def add(self, line):
        if self.active:
            self.data.append(line)

    def deactivate(self):
        self.active = False

    def print(self, output, prefix):
        if self.data:
            print(f'\n{prefix}```cpp', file=output)
            [print(f'{prefix}{i}', file=output) for i in self.data]
            print(f'{prefix}```', file=output)
        self.data = []
        self.active = True

class Description(object):
    def __init__(self):
        self.data = []
        self.lisp_signature = None
        self.cpp_signature = Signature()

    def add(self, line):
        self.data.append(line)

    def lisp(self, line):
        self.lisp_signature = line

    def cpp(self, line):
        self.cpp_signature.add(line)

    def cpp_deactivate(self, line):
        self.cpp_signature.deactivate()

    def print(self, output):
        if self.lisp_signature:
            print('', file=output)
            print(f'> {self.lisp_signature}', file=output)
            self.cpp_signature.print(output, '')
        else:
            self.cpp_signature.print(output, '> ')
        if self.data:
            print('', file=output)
            # self.data[0] = prefix + self.data[0]
            [print(i, file=output) for i in self.data]
        self.data = []
        self.lisp_signature = None

def main(dir):
    p = Path(dir)
    for file in p.iterdir():
        if file.suffix == '.hh':
            state = State.NONE
            description = Description()
            output = None
            for line in fileinput.input(files=file):
                if re.search('^/// @file', line):
                    state = State.HEADER
                    output = open(Path('docs') / file.with_suffix('.md').name, 'w', encoding='utf-8')
                    continue
                if state == State.NONE:
                    continue
                # A @brief command starts a new function description.
                m = re.match('^/// @brief (.*)', line)
                if m:
                    description.print(output)
                    description.add(m.group(1))
                    state = State.FILE
                    continue
                m = re.match('^/// @lisp{(\(.*\)),(.*)}', line)
                if m:
                    description.lisp(f'`{m.group(1)}` (_{m.group(2)}_)')
                    state = State.FILE
                    continue
                m = re.match('^/// (.*)}', line)
                if m:
                    description.add(m.group(1))
                    continue
                # If we're in the header part of the file just print
                # the documentation comments.
                m = re.match('^/// ?(.*)', line)
                if m and state == State.HEADER:
                    continue
                if state != State.FILE:
                    continue
                # Process the @tparam or @param commands.
                m = re.match('^/// @t?param ([a-zA-Z0-9]*) (.*)', line)
                if m:
                    description.add(f'- _{m.group(1)}_ {m.group(2)}')
                    continue
                # If there is a @returns command we add a line.
                m = re.match('^/// @returns? (.*)', line)
                if m:
                    description.add(f'Returns: {m.group(1)}')
                    continue
                # Any other comment is added to the description.
                m = re.match('^/// ?(.*)', line)
                if m:
                    description.add(f'{m.group(1)}')
                    continue
                # Start processing the signature of a C++ function.
                m = re.match('^([^/].*){', line)
                if m:
                    description.cpp(m.group(1).strip())
                    continue
                # When we see a blank line, any curly brace or # ends the signature.
                m = re.match('^$', line)
                if m:
                    description.cpp_deactivate(line)
                    continue
                m = re.match('^[{}#].*', line)
                if m:
                    description.cpp_deactivate(line)
                    continue
                m = re.match('^([^/].*)', line)
                if m:
                    description.cpp(m.group(1).strip())
                    continue
            description.print(output)
            if output:
                output.close()
                output = None

main('src/lisp')

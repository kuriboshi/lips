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

class Description(object):
    def __init__(self):
        self.data = []

    def add(self, line):
        self.data.append(line)

    def print(self):
        if self.data:
            print('')
            [print(i) for i in self.data]
        self.data = []

class Signature(object):
    def __init__(self):
        self.data = []
        self.active = True

    def add(self, line):
        if self.active:
            self.data.append(line)

    def deactivate(self):
        self.active = False

    def print(self):
        if self.data:
            print('\n```c++')
            for i in self.data:
                print(i)
            print('```')
        self.data = []
        self.active = True

def print_doc(signature, description):
    if description:
        [print(i) for i in description]
        description = []
    if signature:
        print('\n```c++')
        [print(i) for i in signature]
        print('```')
        signature = []
    return (signature, description)

def main(dir):
    p = Path(dir)
    for file in p.iterdir():
        if file.suffix == '.hh':
            state = State.NONE
            signature = Signature()
            description = Description()
            for line in fileinput.input(files=file):
                if re.search('^/// @file', line):
                    state = State.HEADER
                    continue
                if state == State.NONE:
                    continue
                # A @brief command starts a new function description.
                m = re.match('^/// @brief (.*)', line)
                if m:
                    description.print()
                    signature.print()
                    description.add(f'> {m.group(1)}')
                    state = State.FILE
                    continue
                # If we're in the header part of the file just print
                # the documentation comments.
                m = re.match('^/// ?(.*)', line)
                if m and state == State.HEADER:
                    print(m.group(1))
                    continue
                if state != State.FILE:
                    continue
                # Process the @tparam or @param commands.
                m = re.match('^/// @t?param ([a-zA-Z]*) (.*)', line)
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
                    signature.add(m.group(1).strip())
                    continue
                # When we see a blank line, any curly brace or # ends the signature.
                m = re.match('^$', line)
                if m:
                    signature.deactivate()
                    continue
                m = re.match('^[{}#].*', line)
                if m:
                    signature.deactivate()
                    continue
                m = re.match('^([^/].*)', line)
                if m:
                    signature.add(m.group(1).strip())
                    continue
            description.print()
            signature.print()


main('src/lisp')

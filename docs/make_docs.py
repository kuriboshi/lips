#!/usr/bin/env python3

# Copyright (c) 2023, 2025 Krister Joas
#
# Licensed under the Apache License, Version 2.0 (the "License"); you
# may not use this file except in compliance with the License.  You may
# obtain a copy of the License at
#
# [http://www.apache.org/licenses/LICENSE-2.0](http://www.apache.org/licenses/LICENSE-2.0)
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
# implied.  See the License for the specific language governing
# permissions and limitations under the License.

from pathlib import Path
import fileinput
import glob
import re
from enum import Enum

class State(Enum):
    NONE = 0
    PREAMBLE = 1
    HEADER = 2
    INTERNAL = 3
    FILE = 4

class Signature(object):
    def __init__(self):
        self.data = []
        self.active = True

    def add(self, line):
        if self.active:
            self.data.append(line)

    def deactivate(self):
        self.active = False

    def print(self, prefix):
        output = []
        if self.data:
            output.append(f'\n{prefix}```cpp')
            [output.append(f'{prefix}{i}') for i in self.data]
            output.append(f'{prefix}```')
        self.data = []
        self.active = True
        return output

class Function(object):
    def __init__(self):
        self.data = []
        self.lisp_signature = []
        self.cpp_signature = Signature()

    def add(self, line):
        self.data.append(line)

    def lisp(self, line):
        self.lisp_signature.append(line)

    def cpp(self, line):
        self.cpp_signature.add(line)

    def cpp_deactivate(self, line):
        self.cpp_signature.deactivate()

    def print(self):
        output = []
        if self.lisp_signature:
            output.append('')
            [output.append(f'> {i}') for i in self.lisp_signature]
            output.extend(self.cpp_signature.print(''))
        else:
            output.extend(self.cpp_signature.print('> '))
        if self.data:
            output.append('')
            [output.append(i) for i in self.data]
        self.data = []
        self.lisp_signature = []
        return output

def process_file(filename):
    output = []
    state = State.NONE
    function = Function()
    for line in fileinput.input(files=filename):
        # Skip lines until we find a @file command.
        if re.search('^/// @file', line):
            state = State.PREAMBLE
            continue
        if state == State.NONE:
            continue

        m = re.match('^/// @internal.*', line)
        if m:
            output.extend(function.print())
            state = State.INTERNAL
            continue

        # A @brief command starts a new function description.
        m = re.match('^/// @brief (.*)', line)
        if m:
            output.extend(function.print())
            function.add(m.group(1))
            state = State.FILE
            continue
        # A @lisp command contains the lisp function signature.
        m = re.match(r'^/// @lisp{(.*),(.*)}', line)
        if m:
            function.lisp(f'`{m.group(1)}` (_{m.group(2)}_)')
            state = State.FILE
            continue

        # If we're in the header part of the file just print
        # the documentation comments.
        m = re.match('^/// ?(.*)', line)
        if m and state == State.PREAMBLE:
            state = State.HEADER
            continue
        if m and state == State.HEADER:
            output.append(m.group(1))
            continue
        if state != State.FILE:
            continue

        # Process the @tparam or @param commands.
        m = re.match('^/// @t?param ([a-zA-Z0-9]*) (.*)', line)
        if m:
            function.add(f'- _{m.group(1)}_ {m.group(2)}')
            continue

        # If there is a @returns command we add a line.
        m = re.match('^/// @returns? (.*)', line)
        if m:
            function.add(f'**Returns**: {m.group(1)}')
            continue

        # Any other comment is added to the function.
        m = re.match('^/// ?(.*)', line)
        if m:
            function.add(f'{m.group(1)}')
            continue

        # Start processing the signature of a C++ function.
        m = re.match('^([^/].*){', line)
        if m:
            function.cpp(m.group(1).strip())
            continue

        # When we see a blank line, any curly brace or a # the
        # signature ends.
        m = re.match('^$', line)
        if m:
            function.cpp_deactivate(line)
            continue
        m = re.match('^[{}#].*', line)
        if m:
            function.cpp_deactivate(line)
            continue
        m = re.match('^([^/].*)', line)
        if m:
            function.cpp(m.group(1).strip())
            continue
    output.extend(function.print())
    return output

def main(dir):
    for d in dir:
        p = Path(d)
        for filename in p.iterdir():
            if filename.suffix == '.hh':
                output = process_file(filename)
                if output:
                    with open(Path('docs/reference') / filename.with_suffix('.md').name, 'w', encoding='utf-8') as f:
                        [print(i, file=f) for i in output]

main(['src/lisp', 'src/lips'])

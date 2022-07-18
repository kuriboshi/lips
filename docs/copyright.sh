#!/bin/sh

#
# Copyright 2022 Krister Joas
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

for file in $(git grep -l Copyright)
do
    # Skip README.md since it should contain a list of all copyright years.
    [[ "$file" == "README.md" ]] && continue
    # Skip LICENSE file.
    [[ "$file" == "LICENSE" ]] && continue

    echo "processing: ${file}"

    awk -v exclude="$(dirname "$0")/exclude.txt" '
function add_year(year) {
  if(!(year in years)) {
    if(length(all_years) != 0) {
      all_years = all_years " "year
    } else {
      all_years = year
    }
  }
  years[year]
}
function output_years(first, last) {
  if(last - first == 0) {
    if(length(copyright_years) == 0) {
      copyright_years = "" first
    } else {
      copyright_years = copyright_years ", " first
    }
  } else if(last - first > 0) {
    if(length(copyright_years) == 0) {
      copyright_years = first "-" last
    } else {
      copyright_years = copyright_years ", " first "-" last
    }
  }
}
/Copyright/ {
  copyright = $0
  all_years = ""
  delete years
  split($0, b)
  ix = 0
  for(i in b) {
    ++ix
    if(b[ix] ~ /^[0-9][0-9][0-9][0-9],/) {
      add_year(substr(b[ix], 1, 4))
    } else if(b[ix] ~ /^[0-9][0-9][0-9][0-9]-/) {
      split(substr(b[ix], 1, 9), c, "-")
      for(i = +c[1]; i <= +c[2]; ++i) {
        add_year(i)
      }
    } else if(b[ix] ~ /^[0-9][0-9][0-9][0-9]/) {
      add_year(b[ix])
    }
  }
  while(("git log --format=\"%ai %H\" --follow " FILENAME " | grep -v -f " exclude " | sed '\''1!G;h;$!d'\''" | getline) > 0) {
    split($0, d, "-")
    add_year(d[1])
  }
  split(all_years, y)
  first = 0
  last = 0
  copyright_years = ""
  ix = 0
  for(i in y) {
    ++ix
    year = +y[ix]
    if(first == 0) {
      first = year
    }
    if(last == 0) {
      last = year
    } else {
      if(year - last == 1) {
        last = year
      } else {
        output_years(first, last)
        first = year
        last = year
      }
    }
  }
  output_years(first, last)
  sub(/[0-9][0-9, -]*/, copyright_years " ", copyright)
  print copyright
  next
}
{
  print
}' $file > ${file}~

    # Only update file if it changed. Copy and remove to preserve
    # original file mode.
    if ! cmp -s ${file} ${file}~
    then
        echo "updated: ${file}"
        cp ${file}~ ${file}
    fi
    rm -f ${file}~
done

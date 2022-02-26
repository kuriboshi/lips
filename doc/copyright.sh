#!/bin/sh

# Copyright 2022 Krister Joas

for file in $(git grep -l Copyright)
do
    # Skip README.md since it should contain a list of all copyright years.
    [[ "$file" == "README.md" ]] && continue

    awk '
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
  while(("git log --reverse --format=%ai --follow " FILENAME | getline) > 0) {
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

    mv ${file}~ ${file}
done

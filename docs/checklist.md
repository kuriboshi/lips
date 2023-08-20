[comment]: # (-*- markdown -*-)

Tasks to perform a release

- Run `clang-format`
- Run `docs/copyright.sh`
- Run `clang-tidy`
- Run all unit tests
- Run `test-linux`
- Update `README.md`
  - Update the version number
  - Check if any other updates are needed
- Update `CMakeLists.txt`
  - Update the version number
- Regenerate `docs/lips.pdf`
  - Update the version number in `docs/lips.tex`
  - Update the publish date in `docs/lips.tex`
- Update `NEWS.md`
  - Update version number 
  - Add summary of the changes
- Tag the release and push to GitHub
  - `git tag -a -s vm.n.p`
  - The comment should have
    - A one line summary, e.g.: "Minor release with the following changes"
    - A list of items of interest
  - `git push github --tags`
- On GitHub create the release from the tag
  - Release format: `Version m.n.p`
  - Copy the changes from `NEWS.md`

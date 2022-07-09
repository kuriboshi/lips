include(cmake-external)

cmake_external(NAME fmt
  URL https://github.com/fmtlib/fmt/archive/8.1.1.tar.gz
  SHA256 3d794d3cf67633b34b2771eb9f073bde87e846e0d395d254df7b211ef1ec7346
  DEFINE CMAKE_BUILD_TYPE=Release
         FMT_TEST=OFF)

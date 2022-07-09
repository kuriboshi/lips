include(cmake-external)

cmake_external(NAME catch2
  URL https://github.com/catchorg/Catch2/archive/v2.13.8.tar.gz
  SHA256 b9b592bd743c09f13ee4bf35fc30eeee2748963184f6bea836b146e6cc2a585a
  DEFINE CMAKE_BUILD_TYPE=Release
         CATCH_BUILD_TESTING=OFF)

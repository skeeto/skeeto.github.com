cmake_minimum_required(VERSION 3.20)
project(ssg LANGUAGES CXX)

# Unity build: each executable is a single translation unit, a platform
# layer that includes the portable core (ssg.cpp). No C++ runtime library,
# exceptions, or RTTI.

include(CheckCXXCompilerFlag)
include(CheckLinkerFlag)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)
if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
  set(CMAKE_BUILD_TYPE Release)
endif()

add_compile_options(
  -fno-exceptions -fno-rtti -fno-threadsafe-statics
  -fno-asynchronous-unwind-tables
  -Wall -Wextra -Wconversion -Wno-sign-conversion
  -Wno-unused-parameter -Wno-unused-function
)

# Debug builds trap on undefined behavior, which includes failed asserts.
set(SSG_TRAP "")
check_cxx_compiler_flag(-fsanitize-trap SSG_HAS_TRAP)
if(SSG_HAS_TRAP)
  set(SSG_TRAP -fsanitize=undefined -fsanitize-trap)
else()
  check_cxx_compiler_flag(-fsanitize-undefined-trap-on-error SSG_HAS_TRAP2)
  if(SSG_HAS_TRAP2)
    set(SSG_TRAP -fsanitize=undefined -fsanitize-undefined-trap-on-error)
  endif()
endif()
add_compile_options("$<$<CONFIG:Debug>:-g3;${SSG_TRAP}>")
add_link_options("$<$<CONFIG:Debug>:${SSG_TRAP}>")

check_linker_flag(CXX -nostdlib++ SSG_HAS_NOSTDLIBXX)
if(SSG_HAS_NOSTDLIBXX)
  add_link_options(-nostdlib++)
endif()

if(WIN32)
  # CRT-free: the only import is kernel32.dll. Static libgcc keeps its
  # helpers (e.g. 64-bit division on i686) from adding a DLL dependency.
  add_executable(ssg main_win32.cpp)
  target_link_options(ssg PRIVATE -nostartfiles -static-libgcc)
  target_link_libraries(ssg PRIVATE kernel32)
else()
  add_executable(ssg main_posix.cpp)
endif()

if(NOT CMAKE_CROSSCOMPILING)
  enable_testing()
  add_executable(ssg_test main_test.cpp)
  add_test(NAME unit COMMAND ssg_test)

  # Development tool: highlight standard input (see test/hlgallery.py)
  if(NOT WIN32)
    add_executable(hlcat test/hlcat.cpp)
  endif()
endif()

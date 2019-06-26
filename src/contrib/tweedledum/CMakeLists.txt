cmake_minimum_required(VERSION 3.1)
project(tweedledum
  DESCRIPTION "Interface between Quilc and the tweedledum library"
  LANGUAGES CXX)

set(default_build_type "Release")
if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
  message(STATUS "Setting build type to '${default_build_type}'")
  set(CMAKE_BUILD_TYPE "${default_build_type}" CACHE
    STRING "Choose the type of build." FORCE)
  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY
    STRINGS "Debug" "Release" "MinSizeRel" "RelWithDebInfo")
endif()

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

set(TWEEDLEDUM_DIR ${PROJECT_SOURCE_DIR})

add_library(tweedledum-object OBJECT ${TWEEDLEDUM_DIR}/tweedledum.cpp)
set_target_properties(tweedledum-object
  PROPERTIES
  CXX_EXTENSIONS OFF
  POSITION_INDEPENDENT_CODE True)
target_compile_definitions(tweedledum-object PRIVATE FMT_HEADER_ONLY)
target_compile_features(tweedledum-object PRIVATE cxx_std_17)
target_include_directories(tweedledum-object
  PRIVATE ${TWEEDLEDUM_DIR}/tweedledum/include
  PRIVATE ${TWEEDLEDUM_DIR}/tweedledum/libs/fmt
  PRIVATE ${TWEEDLEDUM_DIR}/tweedledum/libs/easy
  PRIVATE ${TWEEDLEDUM_DIR}/tweedledum/libs/glucose
  PRIVATE ${TWEEDLEDUM_DIR}/tweedledum/libs/kitty
  PRIVATE ${TWEEDLEDUM_DIR}/tweedledum/include)

set(TWEEDLEDUM_NAME tweedledum)

add_library(tweedledum-shared SHARED $<TARGET_OBJECTS:tweedledum-object>)
set_target_properties(tweedledum-shared PROPERTIES OUTPUT_NAME ${TWEEDLEDUM_NAME})
install(TARGETS tweedledum-shared LIBRARY DESTINATION ".")

find_library(LIBSBCL sbcl
  NAMES libsbcl
  DOC "Path to libsbcl.a"
  HINTS /usr/local/lib)

if("${LIBSBCL}" STREQUAL "LIBSBCL-NOTFOUND")
  message(WARNING "Cannot build custom SBCL binary.
    To build a new SBCL binary, run: \"cmake -DLIBSBCL=/path/to/libsbcl.a .\"")
else()
  find_package(ZLIB REQUIRED)
  find_package(Threads REQUIRED)
  find_library(MATH_LIBRARY m REQUIRED)

  add_library(tweedledum-static STATIC $<TARGET_OBJECTS:tweedledum-object>)
  set_target_properties(tweedledum-static PROPERTIES OUTPUT_NAME ${TWEEDLEDUM_NAME})
  set(LIBTWEEDLEDUM_STATIC_NAME
    "${CMAKE_STATIC_LIBRARY_PREFIX}${TWEEDLEDUM_NAME}${CMAKE_STATIC_LIBRARY_SUFFIX}")

  file(WRITE empty.cpp "// This file exists to placate CMake's need \
for a source file being associated to a target.
")
  add_executable(rigetti-sbcl empty.cpp)
  target_link_options(rigetti-sbcl
    PRIVATE
    # The following linker flags come from sbcl/src/runtime/sbcl.mk:
    # LINKER:-macosx_version_min,10.6
    # LINKER:-arch,x86_64
    # LINKER:-dynamic
    # LINKER:-twolevel_namespace
    # LINKER:-bind_at_load
    LINKER:-pagezero_size,0x100000
    # If you omit the pagezero_size option above, the resulting sbcl binary
    # will jump right into LDB after complaining that the image has been
    # corrupted.
    LINKER:-force_load,${LIBSBCL}
    LINKER:-force_load,${LIBTWEEDLEDUM_STATIC_NAME})
  target_link_libraries(rigetti-sbcl
    ${CMAKE_DL_LIBS} Threads::Threads ZLIB::ZLIB ${MATH_LIBRARY}
    # The linker will link libtweedledum only once but we have to add it
    # explicitly below to ensure CMake knows this target's dependencies.
    tweedledum-static)

  install(TARGETS rigetti-sbcl RUNTIME DESTINATION ".")
endif()

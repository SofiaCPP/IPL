cmake_minimum_required(VERSION 3.27)

project(IPL)

if (MSVC)
    add_compile_options(/W4)
else()
    add_compile_options(-Wall -Wextra -pedantic)
endif()

set(CMAKE_COMPILE_WARNING_AS_ERROR Off)

set(CMAKE_CXX_STANDARD 23)

set(CMAKE_EXPORT_COMPILE_COMMANDS On)

enable_testing()
include(GoogleTest)
add_subdirectory(extern/googletest)
add_subdirectory(JSImpl)
add_subdirectory(gctest)
add_subdirectory(spasm)

cmake_minimum_required(VERSION 3.28)

project(JSImpl)

if (MSVC)
    add_compile_options(/W4)
else()
    add_compile_options(-Wall -Wextra -pedantic)
endif()

set(CMAKE_COMPILE_WARNING_AS_ERROR Off)

set(CMAKE_CXX_STANDARD 23)

add_subdirectory(src)

enable_testing()
add_subdirectory(test)

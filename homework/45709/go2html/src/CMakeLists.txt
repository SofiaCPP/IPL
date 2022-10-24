cmake_minimum_required(VERSION 3.22)
project(go2html)

set(CMAKE_CXX_STANDARD 14)

add_executable(go2html main.cpp Lexer.cpp Lexer.hpp Token.hpp HTMLBuilder.cpp HTMLBuilder.hpp)

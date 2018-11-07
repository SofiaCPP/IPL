cmake_minimum_required(VERSION 3.9)
project(js2html)

set(CMAKE_CXX_STANDARD 11)

add_executable(lexer
        Common.h
        Token.h
        Token.h
        Highlighter.h
        js2html.cpp
        Token.cpp
        Highlighter.cpp)

add_custom_command(TARGET lexer
        PRE_BUILD
        COMMAND flex -o ${PROJECT_SOURCE_DIR}/js2html.cpp ${PROJECT_SOURCE_DIR}/js2html.flex)
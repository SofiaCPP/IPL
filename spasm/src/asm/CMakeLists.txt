cmake_minimum_required(VERSION 3.27)

set(SPASM_HEADER_FILES
    assembler.hpp
    bytecode.hpp
    lexer.hpp
    symbol.hpp
    token.hpp
    tokenizer.hpp
)

set(SPASM_SOURCE_FILES
    assembler.cpp
    bytecode.cpp
    lexer.cpp
    symbol.cpp
    token.cpp
    tokenizer.cpp
    ${SPASM_HEADER_FILES}
)


add_library(spasm_lib STATIC ${SPASM_SOURCE_FILES})

add_executable(spasm main.cpp)
target_link_libraries(spasm spasm_lib)

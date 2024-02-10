cmake_minimum_required(VERSION 3.28)

set(SPRT_HEADER_FILES
    spasm.hpp
    spasm_impl.hpp
    string.hpp
    types.hpp
    value.hpp
)

set(SPRT_SOURCE_FILES
    spasm.cpp
    ${SPRT_HEADER_FILES}
)


add_library(sprt STATIC ${SPRT_SOURCE_FILES})

add_executable(sprun main.cpp)
target_link_libraries(sprun sprt)

add_subdirectory(asm)
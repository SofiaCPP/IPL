cmake_minimum_required(VERSION 3.27)

set(BENCH_HEADER_FILES
    cxxopts.hpp
    gc.h
    weighted_action.h
)

set(BENCH_SOURCE_FILES
    main.cpp
    minitrace.cpp
    ${BENCH_HEADER_FILES}
)


add_library(test_bench STATIC ${BENCH_SOURCE_FILES})
target_compile_definitions(test_bench PRIVATE -D_CRT_SECURE_NO_WARNINGS)

add_executable(gc_dummy gc_dummy.cpp)
target_link_libraries(gc_dummy test_bench)

add_executable(gc_leak gc_leak.cpp)
target_link_libraries(gc_leak test_bench)

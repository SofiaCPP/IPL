enable_testing()
include(CTest)
include(GoogleTest)

set(TEST_HEADERS
    precompiled.h
)

set(TEST_SOURCES
    sprtTests.cpp
)

add_executable(sprt_test ${TEST_SOURCES} ${TEST_HEADERS})
target_include_directories(sprt_test
    SYSTEM PRIVATE ../src
    SYSTEM PRIVATE ../src/asm
    SYSTEM PRIVATE googletest/googletest/include
    SYSTEM PRIVATE googletest/googletmock/include
)
target_precompile_headers(sprt_test PRIVATE ${TEST_HEADERS})
target_link_libraries(sprt_test sprt spasm_lib gtest gmock gmock_main)

gtest_discover_tests(sprt_test)

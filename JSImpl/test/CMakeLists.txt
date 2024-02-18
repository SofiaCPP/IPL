enable_testing()
include(CTest)
include(GoogleTest)

set(HEADER_FILES
    precompiled.h
)

set(TEST_SOURCES
    ASTInterpreterTests.cpp
    CodeGenerationTests.cpp
    LexerTests.cpp
    ParserTests.cpp
    ${HEADER_FILES}
)

add_executable(Test ${TEST_SOURCES})
target_include_directories(Test
    SYSTEM PRIVATE ..
    SYSTEM PRIVATE googletest/googletest/include
    SYSTEM PRIVATE googletest/googletmock/include
)
target_precompile_headers(Test PRIVATE ${HEADER_FILES})
target_link_libraries(Test JS gtest gmock gmock_main)

gtest_discover_tests(Test)

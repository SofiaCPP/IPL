enable_testing()
include(CTest)
include(GoogleTest)

set(TEST_SOURCES
    ASTInterpreterTests.cpp
    CodeGenerationTests.cpp
    LexerTests.cpp
    ParserTests.cpp
)

add_executable(Test ${TEST_SOURCES})
target_include_directories(Test
    SYSTEM PRIVATE ..
    SYSTEM PRIVATE googletest/googletest/include
    SYSTEM PRIVATE googletest/googletmock/include
)
target_link_libraries(Test JS gtest gmock gmock_main)

gtest_discover_tests(Test)

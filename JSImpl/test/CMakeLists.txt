enable_testing()
include(CTest)
include(GoogleTest)

# add_test(NAME solution
#     COMMAND solution --minimal=1
#     WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})

add_subdirectory(googletest)

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

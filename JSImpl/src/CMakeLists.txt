set(JSLIB_HEADER_FILES
    ASTInterpreter.h
    ASTPrinter.h
    ByteCodeGenerator.h
    CommonTypes.h
    Expression.h
    ExpressionDefinitions.h
    ExpressionVisitor.h
    ExpressionsFwd.h
    JSONParser.h
    Lexer.h
    Parser.h
)

set(JSLIB_SOURCE_FILES
    ASTInterpreter.cpp
    ASTPrinter.cpp
    ByteCodeGenerator.cpp
    Expression.cpp
    JSONParser.cpp
    Lexer.cpp
    Parser.cpp
    ${JSLIB_HEADER_FILES}
)


add_library(JS STATIC ${JSLIB_SOURCE_FILES})

add_executable(JSImpl main.cpp)
target_link_libraries(JSImpl JS)
# target_precompile_headers(solution PRIVATE ${HEADER_FILES})



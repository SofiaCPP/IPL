PROJ_DIR = path.getabsolute(".")

solution 'CodeFormatting'
    configurations  { 'Debug' }
    configuration 'Windows'
        flags {
            'FatalWarnings',
            'ExtraWarnings',
            'Symbols',
            'FullSymbols'
        }
    startproject 'CodeFormatting'

configuration 'Debug'
    targetdir(path.join(PROJ_DIR, 'bin', 'debug'))
    objdir(path.join(PROJ_DIR, 'bin', 'debug'))

project 'CodeFormatting'
    kind 'ConsoleApp'
    language 'C++'
    files {
        path.join(PROJ_DIR, "main.cpp"),
        path.join(PROJ_DIR, "..", "..", "..", "JSImpl", "src", "Lexer.h"),
        path.join(PROJ_DIR, "..", "..", "..", "JSImpl", "src", "Lexer.cpp")
    }
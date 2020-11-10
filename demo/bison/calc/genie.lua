solution 'BisonDemos'
    configurations {'debug', 'release'}
    flags {
        'Cpp14',
        'Symbols',
    }
    configuration '*'

    local root = 'build/'


    configuration 'debug'
        targetdir(root .. 'bin/debug')
        objdir(root .. 'obj/debug')

    configuration 'release'
        targetdir(root .. 'bin/release')
        objdir(root .. 'obj/release')
        flags 'OptimizeSpeed'
    configuration '*'

    project 'Calc'
        kind 'ConsoleApp'
        language 'C'
        uuid(os.uuid('Calc'))
        files {
            'lex.yy.c',
            'parser.tab.c',
        }
    project 'DebugCalc'
        kind 'ConsoleApp'
        language 'C'
        uuid(os.uuid('DebugCalc'))
        defines 'YYDEBUG=1'
        files {
            'lex.yy.c',
            'parser.tab.c',
        }

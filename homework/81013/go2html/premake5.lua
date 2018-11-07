solution 'HL'
    configurations {'debug', 'release'}
    flags {
        'FatalWarnings'
    }

    local root = 'build/'

    configuration 'debug'
        targetdir(root .. 'bin/debug')
        objdir(root .. 'obj/debug')

    configuration 'release'
        targetdir(root .. 'bin/release')
        objdir(root .. 'obj/release')
    configuration '*'

    project 'HL'
        kind 'ConsoleApp'
        language 'C++'
        files '*.cpp'
        files '*.h'

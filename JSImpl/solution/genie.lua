solution 'IPL'
    configurations {'debug', 'release'}
    flags {
        'FatalWarnings',
        'ExtraWarnings',
        'Cpp14',
    }

    local root = '../build/'

    configuration 'debug'
        targetdir(root .. 'bin/debug')
        objdir(root .. 'obj/debug')

    configuration 'release'
        targetdir(root .. 'bin/release')
        objdir(root .. 'obj/release')
    configuration '*'

    project 'JSImpl'
        kind 'ConsoleApp'
        language 'C++'
        uuid(os.uuid('JSImpl'))
        files '../src/*.cpp'
        files '../src/*.h'

    include '../test'

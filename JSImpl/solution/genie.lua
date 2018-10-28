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

    project 'JSLib'
        kind 'StaticLib'
        language 'C++'
        uuid(os.uuid('JSLib'))
        files '../src/*.cpp'
        removefiles '../src/main.cpp'
        files '../src/*.h'

    project 'JSImpl'
        kind 'ConsoleApp'
        language 'C++'
        uuid(os.uuid('JSImpl'))
        files '../src/main.cpp'
        links 'JSLib'

    include '../test'

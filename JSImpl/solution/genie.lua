solution 'JSImpl'
    configurations {'Debug', 'Release'}
    platforms { 'x64' }
    flags {
        'FatalWarnings',
        'ExtraWarnings',
        'Cpp14',
        'Symbols',
    }


    local root = '../build/'

    configuration 'Debug'
        targetdir(root .. 'bin/Debug')
        objdir(root .. 'obj/Debug')

    configuration 'Release'
        flags 'OptimizeSpeed'
        targetdir(root .. 'bin/Release')
        objdir(root .. 'obj/Release')
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
    startproject 'Test'

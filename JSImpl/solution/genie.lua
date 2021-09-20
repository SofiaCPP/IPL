solution 'JSImpl'
    if _ACTION ~= 'jcdb' then
        platforms { 'x32', 'x64' }
        configurations {'Debug', 'Release'}
    else
        configurations {'Debug'}
    end
    flags {
        'FatalWarnings',
        'ExtraWarnings',
        'Cpp17',
        'Symbols',
    }
    defines {
        '_SCL_SECURE_NO_WARNINGS',
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

    include '../test'
    startproject 'Test'

    group 'JSImpl'
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

    group 'Spasm'
        include '../../spasm/solution/'

    group 'gctest'
        include '../../gctest/solution/'

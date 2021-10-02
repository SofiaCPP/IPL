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
    if _ACTION ~= 'ninja' then
        location '.'
    else
        location './ninja'
    end

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
            location(solution().location)
            files '../src/*.cpp'
            removefiles '../src/main.cpp'
            files '../src/*.h'

        project 'JSImpl'
            kind 'ConsoleApp'
            language 'C++'
            uuid(os.uuid('JSImpl'))
            location(solution().location)
            files '../src/main.cpp'
            links 'JSLib'

    group 'Spasm'
        include '../../spasm/solution/'

    group 'gctest'
        include '../../gctest/solution/'

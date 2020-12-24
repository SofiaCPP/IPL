solution 'gctest'
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
        'MTR_ENABLED',
    }


    local root = './build/'

    configuration 'Debug'
        targetdir(root .. 'bin/Debug')
        objdir(root .. 'obj/Debug')

    configuration 'Release'
        flags 'OptimizeSpeed'
        targetdir(root .. 'bin/Release')
        objdir(root .. 'obj/Release')
    configuration '*'

    project 'test_bench'
        kind 'StaticLib'
        language 'C++'
        files {
            'main.cpp',
        }

    project 'leak_gc'
        kind 'ConsoleApp'
        language 'C++'
        files 'gc_leak.cpp'
        links 'test_bench'


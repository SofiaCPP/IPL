if not solution() then
    solution 'gctest'
    if _ACTION ~= 'jcdb' then
        platforms { 'x32', 'x64' }
        configurations {'Debug', 'Release'}
    else
        configurations {'Debug'}
        platforms { 'x64' }
    end
    flags {
        'FatalWarnings',
        'ExtraWarnings',
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
end

function setup()
    flags 'Cpp17'
    defines {
        '_SCL_SECURE_NO_WARNINGS',
        '_CRT_SECURE_NO_WARNINGS',
        'MTR_ENABLED',
    }

    -- GCC generates a warning for uninitialized data in
    -- std::normal_distribution
    configuration 'gcc'
        removeflags 'ExtraWarnings'
    configuration '*'
end

    project 'test_bench'
        kind 'StaticLib'
        language 'C++'
        files {
            '../src/main.cpp',
            '../src/minitrace.cpp',
        }
        removeflags 'ExtraWarnings'
        setup()

    project 'leak_gc'
        kind 'ConsoleApp'
        language 'C++'
        files '../src/gc_leak.cpp'
        links 'test_bench'
        setup()

    project 'dummy_gc'
        kind 'ConsoleApp'
        language 'C++'
        files '../src/gc_dummy.cpp'
        links 'test_bench'
        setup()


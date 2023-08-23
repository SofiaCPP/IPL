if not solution() then
    solution 'spasm'
    configurations {'Debug', 'Release'}
    platforms { 'x64' }
    flags {
        'FatalWarnings',
        'ExtraWarnings',
        'Cpp14',
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
end

    project 'sprt'
        kind 'StaticLib'
        language 'C++'
        uuid(os.uuid('sprt'))
        files '../src/*.cpp'
        removefiles '../src/main.cpp'
        files '../src/*.hpp'

    project 'spasm_lib'
        kind 'StaticLib'
        language 'C++'
        uuid(os.uuid('spasm_lib'))
        location(solution().location)
        files '../src/asm/*.cpp'
        removefiles {
            '../src/asm/main.cpp',
            '../src/asm/lexdump.cpp',
            '../src/asm/lexdump3.cpp',
        }
        files '../src/asm/*.hpp'

    project 'spasm'
        kind 'ConsoleApp'
        language 'C++'
        uuid(os.uuid('spasm'))
        location(solution().location)
		links 'spasm_lib'
        files '../src/asm/main.cpp'
        removefiles {
            '../src/asm/lexdump.cpp',
            '../src/asm/lexdump3.cpp',
        }
        files '../src/asm/*.hpp'

    project 'sprun'
        kind 'ConsoleApp'
        language 'C++'
        uuid(os.uuid('sprun'))
        location(solution().location)
        files '../src/main.cpp'
        links 'sprt'

    -- include '../test'
    startproject 'sprun'

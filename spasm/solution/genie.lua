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
        files '../src/*.h'

    project 'spasm_lib'
        kind 'StaticLib'
        language 'C++'
        uuid(os.uuid('spasm_lib'))
        files '../src/asm/*.cpp'
        removefiles {
            '../src/asm/main.cpp',
            '../src/asm/lexdump.cpp',
            '../src/asm/lexdump3.cpp',
        }
        files '../src/asm/*.h'

    project 'spasm'
        kind 'ConsoleApp'
        language 'C++'
        uuid(os.uuid('spasm'))
		links 'spasm_lib'
        files '../src/asm/main.cpp'
        removefiles {
            '../src/asm/lexdump.cpp',
            '../src/asm/lexdump3.cpp',
        }
        files '../src/asm/*.h'

    project 'sprun'
        kind 'ConsoleApp'
        language 'C++'
        uuid(os.uuid('sprun'))
        files '../src/main.cpp'
        links 'sprt'

    -- include '../test'
    startproject 'sprun'

solution 'js2html'
    configurations {'debug', 'release'}
    configuration 'Linux'
        flags {
            'FatalWarnings',
            'ExtraWarnings',
            'Cpp14',
        }
    configuration '*'

    local root = 'build/'


    configuration 'debug'
        targetdir(root .. 'bin/debug')
        objdir(root .. 'obj/debug')

    configuration 'release'
        targetdir(root .. 'bin/release')
        objdir(root .. 'obj/release')
    configuration '*'

    project 'js2html'
        kind 'ConsoleApp'
        language 'C'
        uuid(os.uuid('js2html'))
        files {
            'js2html.c',
            'js2html.flex',
        }
        debugargs 'hello.js'

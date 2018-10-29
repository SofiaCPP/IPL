project 'Test'
    kind 'ConsoleApp'
    language 'C++'
    uuid(os.uuid('Test'))

    files '*.cpp'

    includedirs {
        'googletest/googletest/include',
        'googletest/googlemock/include',
        '..',
    }
    links {
        'JSLib',
        'gtest',
        'gmock',
        'gtest_main',
    }
    configuration 'Linux'
        links 'pthread'
    configuration '*'

project 'gtest'
    kind 'StaticLib'
    language 'C++'
    uuid(os.uuid('gtest'))
    files 'googletest/googletest/src/gtest-all.cc'
    files 'googletest/googletest/include/gtest/*.h'
    includedirs {
        'googletest/googletest',
        'googletest/googletest/include',
    }

project 'gmock'
    kind 'StaticLib'
    language 'C++'
    uuid(os.uuid('gmock'))
    files 'googletest/googlemock/src/gmock-all.cc'
    files 'googletest/googlemock/include/gmock/*.h'
    includedirs {
        'googletest/googlemock',
        'googletest/googlemock/include',
        'googletest/googletest/include',
    }

project 'gtest_main'
    kind 'StaticLib'
    language 'C++'
    uuid(os.uuid('gtest_main'))
    files 'googletest/googletest/src/gtest_main.cc'
    includedirs {
        'googletest/googletest',
        'googletest/googletest/include',
    }

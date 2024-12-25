cmake_minimum_required(VERSION 3.0)
project(ms-2
    VERSION 3.0
    DESCRIPTION "ms-2"
    LANGUAGES CXX
)

if (MSVC)
    # ...
else()
    add_compile_options(-Wcomment)
endif()

add_executable(ms
    src/compiler.cpp
    src/context.cpp
    src/debug.cpp
    src/env.cpp
    src/lang.cpp
    src/memory.cpp
    src/misc.cpp
    src/opcode.cpp
    src/source.cpp
    src/ast.cpp
    main.cpp
)

target_compile_features(ms PUBLIC cxx_std_20)
target_include_directories(ms PUBLIC include)
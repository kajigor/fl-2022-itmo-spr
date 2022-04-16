cmake_minimum_required(VERSION 3.17)
project(ff8 C)

set(CMAKE_C_STANDARD 99)

add_executable(ff8 main.c src/str.c src/grammar.c src/stack.c)

cmake_minimum_required(VERSION 3.0)
project(99Problems)


set(CMAKE_CXX_STANDARD 17)

include_directories(
    src
    third_party
    test/spec
)


add_executable(${PROJECT_NAME}_test test/main.cpp)

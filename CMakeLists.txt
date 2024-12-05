cmake_minimum_required(VERSION 3.30)

project(RealTimeDeltaQSD
        VERSION 0.1
        LANGUAGES CXX
        )

option(BUILD_SHARED_LIBS "Build libraries as shared as opposed to static" ON)


set(CXX_STANDARD 17)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
set(PREFIX DQ)

set(CMAKE_AUTOMOC ON)
set(CMAKE_AUTORCC ON)
set(CMAKE_AUTOUIC ON)

set(CMAKE_INCLUDE_CURRENT_DIR ON)

add_subdirectory(src)
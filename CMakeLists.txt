cmake_minimum_required(VERSION 3.30)

set(CMAKE_CXX_COMPILER "g++")
project(RealTimeDeltaQSD
        VERSION 0.1
        LANGUAGES CXX
        )

        option(BUILD_SHARED_LIBS "Build libraries as shared as opposed to static" OFF)

set(CXX_STANDARD 17)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
set(PREFIX DQ)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_AUTOMOC ON)
set(CMAKE_AUTORCC ON)
set(CMAKE_AUTOUIC ON)

set(CMAKE_INCLUDE_CURRENT_DIR ON)
include(cmake/CPM.cmake)
include(cmake/AddJson.cmake)
add_subdirectory(src)



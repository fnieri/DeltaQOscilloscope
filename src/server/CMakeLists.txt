add_library(${PREFIX}_server
    Server.cpp
    Server.h
)

target_include_directories(${PREFIX}_server
    PUBLIC
    ${CMAKE_SOURCE_DIR}
        ${PREFIX}_diagram
)

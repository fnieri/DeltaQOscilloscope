add_library(${PREFIX}_server
    Server.cpp
    Server.h
)

target_link_libraries(${PREFIX}_server
    PUBLIC
        ${PREFIX}_diagram
    )

target_include_directories(${PREFIX}_server
    PUBLIC
    ${CMAKE_SOURCE_DIR}
)

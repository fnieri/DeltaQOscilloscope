add_library(${PREFIX}_server
    Server.cpp
    Server.h
    SpansReader.h
    SpansReader.cpp
)

target_link_libraries(${PREFIX}_server
    PUBLIC
        ${PREFIX}_diagram
    )

target_include_directories(${PREFIX}_server
    PUBLIC
    ${CMAKE_SOURCE_DIR}
)


add_executable(server
        main.cpp
)

target_link_libraries(server
    PRIVATE
    ${PREFIX}_server
)

target_compile_features(server PRIVATE cxx_std_17)

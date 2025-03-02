
find_package(Python3 COMPONENTS Interpreter Development REQUIRED)

add_library(${PREFIX}_parser 
    ParserWrapper.h
    ParserWrapper.cpp
)

target_include_directories(${PREFIX}_parser
    PUBLIC
    ${CMAKE_SOURCE_DIR}
    ${Python3_INCLUDE_DIRS}
)

target_link_libraries(${PREFIX}_parser
    PRIVATE Python3::Python
)

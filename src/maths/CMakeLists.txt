add_library(${PREFIX}_maths
        DeltaQ.h
        DeltaQ.cpp
        DeltaQOperations.h
        DeltaQOperations.cpp
)

target_include_directories(${PREFIX}_maths
        PUBLIC
        ${CMAKE_SOURCE_DIR}
)

add_executable(maths
        main.cpp
)


target_link_libraries(maths
        PRIVATE
        ${PREFIX}_maths
)

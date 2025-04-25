add_library(${PREFIX}_maths
    ConfidenceInterval.h
    ConfidenceInterval.cpp
    DeltaQ.h
    DeltaQ.cpp
    DeltaQOperations.h
    DeltaQOperations.cpp
)

target_include_directories(${PREFIX}_maths
        PUBLIC
        ${CMAKE_SOURCE_DIR}
)

target_link_libraries(${PREFIX}_maths
    PRIVATE
    fftw3
)

add_executable(maths
        main.cpp
)


target_link_libraries(maths
        PRIVATE
        ${PREFIX}_maths
)

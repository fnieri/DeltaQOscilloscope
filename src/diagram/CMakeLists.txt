add_library(${PREFIX}_diagram
    Outcome.h
    Outcome.cpp
    Operator.h
    Operator.cpp
    Probe.h
    Probe.cpp
    System.h
    System.cpp
    SystemUtils.h
    SystemUtils.cpp
    Sample.h
    Observable.h
    Observable.cpp
)

target_link_libraries(${PREFIX}_diagram
    PUBLIC
        ${PREFIX}_maths
        ${PREFIX}_application
    PRIVATE
        nlohmann_json
)

target_include_directories(${PREFIX}_diagram
        PUBLIC
        ${CMAKE_SOURCE_DIR}
)

add_executable(diagram
        main.cpp
)

target_link_libraries(diagram
        PRIVATE
        ${PREFIX}_diagram
)

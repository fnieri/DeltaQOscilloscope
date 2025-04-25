add_library(${PREFIX}_diagram
    AllToFinish.h
    AllToFinish.cpp
    DiagramComponent.h
    DiagramComponent.cpp
    FirstToFinish.h
    FirstToFinish.cpp
    Outcome.h
    Outcome.cpp
    Operator.h
    Operator.cpp
    ProbabilisticOperator.h
    ProbabilisticOperator.cpp
    Probe.h
    Probe.cpp
    System.h
    System.cpp
    Sample.h
    Observable.h
    Observable.cpp
)

target_link_libraries(${PREFIX}_diagram
    PUBLIC
        ${PREFIX}_maths
        ${PREFIX}_parser
        ${PREFIX}_application
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

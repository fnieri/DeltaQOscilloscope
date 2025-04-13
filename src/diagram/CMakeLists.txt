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
    SystemParser.cpp
    SystemParser.h
    Sample.h
    Observable.h
    Observable.cpp
)



find_package(Python3 REQUIRED COMPONENTS Interpreter)
target_link_libraries(${PREFIX}_diagram
    PUBLIC
        ${PREFIX}_maths
        nlohmann_json
        ${PREFIX}_parser
)


target_include_directories(${PREFIX}_diagram
        PUBLIC
        ${CMAKE_SOURCE_DIR}
PRIVATE ${Python3_INCLUDE_DIRS}
)

add_executable(diagram
        main.cpp
)


target_link_libraries(diagram
        PRIVATE
        ${PREFIX}_diagram
${Python3_LIBRARIES}
)

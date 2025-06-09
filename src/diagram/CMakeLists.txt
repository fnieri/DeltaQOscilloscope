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


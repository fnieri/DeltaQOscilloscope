add_library(${PREFIX}_maths
    ConfidenceInterval.h
    ConfidenceInterval.cpp
    DeltaQ.h
    DeltaQ.cpp
    DeltaQRepr.h
    DeltaQOperations.h
    DeltaQOperations.cpp
    QTA.h
    Triggers.h
    Triggers.cpp
    TriggerTypes.h
    TriggerManager.cpp
    TriggerManager.h
    Snapshot.h
    Snapshot.cpp
)

target_include_directories(${PREFIX}_maths
        PUBLIC
        ${PREFIX}_diagram
)

target_link_libraries(${PREFIX}_maths
    PRIVATE
    fftw3
)


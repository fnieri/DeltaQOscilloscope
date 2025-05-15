add_library(${PREFIX}_dashboard
    DelaySettingsWidget.h
    DelaySettingsWidget.cpp
    DQPlotController.h
    DQPlotController.cpp
    DQPlotList.h
    DQPlotList.cpp
    DeltaQPlot.h
    DeltaQPlot.cpp
    MainWindow.h
    MainWindow.cpp
    NewPlotList.h
    NewPlotList.cpp
    Sidebar.h
    Sidebar.cpp
    SnapshotViewerWindow.h
    SnapshotViewerWindow.cpp
    QTAInputWidget.cpp
    QTAInputWidget.h
    TriggersTab.cpp
    TriggersTab.h
)


find_package(QT NAMES Qt6 Qt5 REQUIRED COMPONENTS Core Gyu Widgets Charts Graphs)
find_package(Qt6 REQUIRED COMPONENTS Core Gui Charts Widgets Graphs)

target_link_libraries(${PREFIX}_dashboard
        PUBLIC Qt6::Core Qt6::Gui Qt6::Widgets Qt6::Charts Qt6::Graphs
        ${PREFIX}_maths
        ${PREFIX}_server
        ${PREFIX}_parser
    )

target_include_directories(${PREFIX}_dashboard
        PUBLIC
        ${CMAKE_SOURCE_DIR}         
        ${PREFIX}_parser
)


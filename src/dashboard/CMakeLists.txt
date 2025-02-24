add_library(${PREFIX}_dashboard
       JsonComponent.h
        JsonComponent.cpp
        DQPlotController.h
        DQPlotController.cpp
        DeltaQPlot.h
        DeltaQPlot.cpp
        MainWindow.h
        MainWindow.cpp
        AddPlotDialog.h
        AddPlotDialog.cpp
    )

find_package(QT NAMES Qt6 Qt5 REQUIRED COMPONENTS Core Gyu Widgets Charts Graphs)
find_package(Qt6 REQUIRED COMPONENTS Core Gui Charts Widgets Graphs)

target_link_libraries(${PREFIX}_dashboard
        PUBLIC Qt6::Core Qt6::Gui Qt6::Widgets Qt6::Charts Qt6::Graphs
        ${PREFIX}_maths
        nlohmann_json
        ${PREFIX}_server
    )

target_include_directories(${PREFIX}_dashboard
        PUBLIC
        ${CMAKE_SOURCE_DIR}
         
    )


add_executable(dashboard
        main.cpp
)

target_link_libraries(dashboard
        PRIVATE
        ${PREFIX}_dashboard
)

target_compile_features(dashboard PRIVATE cxx_std_17)

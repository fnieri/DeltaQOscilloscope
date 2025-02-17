add_subdirectory(dashboard)
add_subdirectory(diagram)
add_subdirectory(maths)
add_subdirectory(server)

add_executable(RealTimeDeltaQSD main.cpp)

target_link_libraries(RealTimeDeltaQSD ${PREFIX}_server ${PREFIX}_diagram ${PREFIX}_dashboard)

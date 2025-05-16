add_library(${PREFIX}_application 
    Application.cpp
    Application.h
)


target_link_libraries(${PREFIX}_application
    PRIVATE
    ${PREFIX}_server 
)

# Make sure other libraries can find Application headers
target_include_directories(${PREFIX}_application
    PUBLIC ${CMAKE_SOURCE_DIR}
)

add_subdirectory(dashboard)
add_subdirectory(diagram)
add_subdirectory(maths)
add_subdirectory(server)
add_subdirectory(parser)

add_executable(RealTimeDeltaQSD main.cpp)

target_include_directories(RealTimeDeltaQSD PUBLIC ${CMAKE_SOURCE_DIR})

target_link_libraries(RealTimeDeltaQSD ${PREFIX}_server ${PREFIX}_diagram ${PREFIX}_dashboard ${PREFIX}_parser)

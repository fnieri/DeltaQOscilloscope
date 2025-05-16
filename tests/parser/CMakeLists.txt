set(test_suite ${PREFIX}_parser_test)

# Names of the test files without the extention
set(${test_suite}
        TestSystemBuilder.cpp
)

add_executable(${test_suite} ${${test_suite}})

add_test(NAME ${test_suite} COMMAND ${test_suite} WORKING_DIRECTORY ${CMAKE_SOURCE_DIR})
target_link_libraries(${test_suite} PRIVATE  gtest GTest::gtest_main ${PREFIX}_parser ${PREFIX}_diagram)
CPMAddPackage(
    NAME pybind11
    GIT_REPOSITORY https://github.com/pybind/pybind11.git
    VERSION 2.13.6
    DOWNLOAD_ONLY YES
)

if(pybind11_ADDED)
    find_package(Python3 REQUIRED COMPONENTS Interpreter Development)

    target_include_directories(<project_name> PRIVATE ${Python3_INCLUDE_DIRS})
    target_link_libraries(<project_name> PRIVATE ${Python3_LIBRARIES})

    target_include_directories(<project_name> PRIVATE ${pybind11_SOURCE_DIR}/include)
endif()

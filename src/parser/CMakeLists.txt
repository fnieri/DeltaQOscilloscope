set(ANTLR4_INCLUDE_DIR "/usr/local/include/antlr4-runtime")
set(ANTLR4_LIB_DIR "/usr/local/lib")


find_library(ANTLR4_RUNTIME_LIB_STATIC NAMES antlr4-runtime PATHS ${ANTLR4_LIB_DIR})


if(NOT ANTLR4_RUNTIME_LIB_STATIC)
    message(FATAL_ERROR "Static ANTLR4 runtime library not found in ${ANTLR4_LIB_DIR}")
endif()

add_library(antlr4-runtime STATIC IMPORTED)
set_target_properties(antlr4-runtime PROPERTIES
    IMPORTED_LOCATION ${ANTLR4_RUNTIME_LIB_STATIC}
    INTERFACE_INCLUDE_DIRECTORIES ${ANTLR4_INCLUDE_DIR}
)

# -----------------------------------------------------------
# ⚙️ Generate ANTLR Files
# -----------------------------------------------------------
set(ANTLR_GENERATED_DIR ${CMAKE_CURRENT_BINARY_DIR}/generated)
file(MAKE_DIRECTORY ${ANTLR_GENERATED_DIR})

find_program(ANTLR_EXECUTABLE antlr4)
if(NOT ANTLR_EXECUTABLE)
    message(FATAL_ERROR "ANTLR tool (antlr4) not found in PATH")
endif()

execute_process(
    COMMAND ${ANTLR_EXECUTABLE}
        -Dlanguage=Cpp
        -visitor -no-listener
        -o ${ANTLR_GENERATED_DIR}
        -package parser
        ${CMAKE_CURRENT_SOURCE_DIR}/DQGrammar.g4
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    OUTPUT_VARIABLE ANTLR_OUTPUT
    ERROR_VARIABLE ANTLR_ERROR
    RESULT_VARIABLE ANTLR_RESULT
)

if(NOT ANTLR_RESULT EQUAL 0)
    message(FATAL_ERROR "ANTLR generation failed:\n${ANTLR_ERROR}")
else()
    message(STATUS "ANTLR generated successfully:\n${ANTLR_OUTPUT}")
endif()

# -----------------------------------------------------------
# 🧱 Parser Library
# -----------------------------------------------------------
add_library(${PREFIX}_parser
    ${ANTLR_GENERATED_DIR}/DQGrammarLexer.cpp
    ${ANTLR_GENERATED_DIR}/DQGrammarParser.cpp
    ${ANTLR_GENERATED_DIR}/DQGrammarBaseVisitor.cpp
    ${ANTLR_GENERATED_DIR}/DQGrammarVisitor.cpp

    ${ANTLR_GENERATED_DIR}/DQGrammarLexer.h
    ${ANTLR_GENERATED_DIR}/DQGrammarParser.h
    ${ANTLR_GENERATED_DIR}/DQGrammarBaseVisitor.h
    ${ANTLR_GENERATED_DIR}/DQGrammarVisitor.h

    SystemBuilder.h
    SystemBuilder.cpp
    SystemErrorListener.h
    SystemParserInterface.h
    SystemParserInterface.cpp
)

target_include_directories(${PREFIX}_parser
    PUBLIC
        ${CMAKE_CURRENT_SOURCE_DIR}
        ${ANTLR_GENERATED_DIR}
        ${ANTLR4_INCLUDE_DIR}
)

target_link_libraries(${PREFIX}_parser
    PRIVATE antlr4-runtime
)

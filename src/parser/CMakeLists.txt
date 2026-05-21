# Locate ANTLR4 C++ runtime — check common install paths so this works on
# Linux (/usr/local), Intel Mac (/usr/local/homebrew) and Apple Silicon (/opt/homebrew).
find_path(ANTLR4_INCLUDE_DIR antlr4-runtime.h
    PATHS
        /usr/local/include/antlr4-runtime
        /opt/homebrew/include/antlr4-runtime
        /usr/include/antlr4-runtime
    REQUIRED
)
find_library(ANTLR4_RUNTIME_LIB NAMES antlr4-runtime
    PATHS /usr/local/lib /opt/homebrew/lib /usr/lib
    REQUIRED
)

add_library(antlr4-runtime STATIC IMPORTED)
set_target_properties(antlr4-runtime PROPERTIES
    IMPORTED_LOCATION      ${ANTLR4_RUNTIME_LIB}
    INTERFACE_INCLUDE_DIRECTORIES ${ANTLR4_INCLUDE_DIR}
)

# -----------------------------------------------------------
# Generated files live in the source tree (version-controlled).
# add_custom_command re-runs ANTLR *only* when DQGrammar.g4 changes.
# Contributors without the antlr4 tool can still build from the
# committed files; they just can't modify the grammar.
# -----------------------------------------------------------
set(ANTLR_GENERATED_DIR ${CMAKE_CURRENT_SOURCE_DIR}/generated)
set(GRAMMAR_FILE        ${CMAKE_CURRENT_SOURCE_DIR}/DQGrammar.g4)

set(GENERATED_SOURCES
    ${ANTLR_GENERATED_DIR}/DQGrammarLexer.cpp
    ${ANTLR_GENERATED_DIR}/DQGrammarParser.cpp
    ${ANTLR_GENERATED_DIR}/DQGrammarBaseVisitor.cpp
    ${ANTLR_GENERATED_DIR}/DQGrammarVisitor.cpp
)
set(GENERATED_HEADERS
    ${ANTLR_GENERATED_DIR}/DQGrammarLexer.h
    ${ANTLR_GENERATED_DIR}/DQGrammarParser.h
    ${ANTLR_GENERATED_DIR}/DQGrammarBaseVisitor.h
    ${ANTLR_GENERATED_DIR}/DQGrammarVisitor.h
)

find_program(ANTLR_EXECUTABLE antlr4)
if(ANTLR_EXECUTABLE)
    add_custom_command(
        OUTPUT  ${GENERATED_SOURCES} ${GENERATED_HEADERS}
        COMMAND ${ANTLR_EXECUTABLE}
                -Dlanguage=Cpp
                -visitor -no-listener
                -o ${ANTLR_GENERATED_DIR}
                -package parser
                ${GRAMMAR_FILE}
        DEPENDS ${GRAMMAR_FILE}
        COMMENT "Regenerating ANTLR parser from DQGrammar.g4"
        VERBATIM
    )
else()
    message(STATUS
        "antlr4 not found in PATH — building from committed generated files. "
        "Re-run cmake after installing antlr4 if you need to modify the grammar.")
endif()

# -----------------------------------------------------------
# Parser library
# -----------------------------------------------------------
add_library(${PREFIX}_parser
    ${GENERATED_SOURCES}
    ${GENERATED_HEADERS}
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

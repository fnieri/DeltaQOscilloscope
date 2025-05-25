#include "SystemParserInterface.h"
#include "SystemErrorListener.h"
#include <exception>
#include <fstream>
#include <iostream>
#include <sstream>
#include <stdexcept>

/**
 * @brief Parses a system definition from a file.
 * @param filename Path to the file containing system definition.
 * @return Optional containing the parsed System if successful, nullopt on error.
 * @throws std::invalid_argument if parsing fails.
 */
std::optional<System> SystemParserInterface::parseFile(const std::string &filename)
{
    std::ifstream file(filename);
    if (!file) {
        std::cerr << "Error: Could not open file: " << filename << std::endl;
        return std::nullopt;
    }

    // Read entire file content
    std::stringstream buffer;
    buffer << file.rdbuf();
    std::string content = buffer.str();

    antlr4::ANTLRInputStream input(content);
    try {
        return parseInternal(input);
    } catch (std::exception &e) {
        throw std::invalid_argument(e.what());
    }
}

/**
 * @brief Parses a system definition from a string.
 * @param inputStr String containing system definition.
 * @return Optional containing the parsed System if successful, nullopt on error.
 * @throws std::invalid_argument if parsing fails.
 */
std::optional<System> SystemParserInterface::parseString(const std::string &inputStr)
{
    antlr4::ANTLRInputStream input(inputStr);
    try {
        return parseInternal(input);
    } catch (std::exception &e) {
        throw std::invalid_argument(e.what());
    }
}

/**
 * @brief Internal parsing implementation using ANTLR.
 * @param input ANTLR input stream containing system definition.
 * @return Optional containing the parsed System if successful, nullopt on error.
 * @throws std::invalid_argument if parsing fails.
 */
std::optional<System> SystemParserInterface::parseInternal(antlr4::ANTLRInputStream &input)
{
    // Initialize lexer and parser
    parser::DQGrammarLexer lexer(&input);
    antlr4::CommonTokenStream tokens(&lexer);
    parser::DQGrammarParser parser(&tokens);

    // Configure error handling
    SystemErrorListener errorListener;
    lexer.removeErrorListeners();
    parser.removeErrorListeners();
    lexer.addErrorListener(&errorListener);
    parser.addErrorListener(&errorListener);

    try {
        // Parse and build system
        auto tree = parser.start();
        SystemBuilderVisitor visitor;
        visitor.visitStart(tree);
        return visitor.getSystem();
    } catch (std::exception &e) {
        throw std::invalid_argument(e.what());
    }
}

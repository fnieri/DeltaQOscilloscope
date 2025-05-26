#pragma once

#include "../diagram/System.h"
#include "DQGrammarLexer.h"
#include "DQGrammarParser.h"
#include "SystemBuilder.h"
#include "antlr4-runtime.h"
#include <memory>
#include <optional>
#include <string>

/**
 * @class SystemParserInterface
 * @brief Provides an interface for parsing system definitions from files or strings.
 */
class SystemParserInterface
{
public:
    /**
     * @brief Parses a system definition from a file.
     * @param filename Path to the file containing system definition.
     * @return Optional containing the parsed System if successful, nullopt otherwise.
     */
    static std::optional<System> parseFile(const std::string &filename);

    /**
     * @brief Parses a system definition from a string.
     * @param input String containing system definition.
     * @return Optional containing the parsed System if successful, nullopt otherwise.
     */
    static std::optional<System> parseString(const std::string &input);

private:
    /**
     * @brief Internal parsing method using ANTLR input stream.
     * @param input ANTLR input stream containing system definition.
     * @return Optional containing the parsed System if successful, nullopt otherwise.
     */
    static std::optional<System> parseInternal(antlr4::ANTLRInputStream &input);
};

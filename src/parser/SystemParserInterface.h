
#pragma once

#include "../diagram/System.h"
#include "DQGrammarLexer.h"
#include "DQGrammarParser.h"
#include "SystemBuilder.h"
#include "antlr4-runtime.h"
#include <memory>
#include <optional>
#include <string>

class SystemParserInterface
{
public:
    static std::optional<System> parseFile(const std::string &filename);

    static std::optional<System> parseString(const std::string &input);

private:
    static std::optional<System> parseInternal(antlr4::ANTLRInputStream &input);
};

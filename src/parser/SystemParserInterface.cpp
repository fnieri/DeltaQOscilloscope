
#include "SystemParserInterface.h"
#include "SystemErrorListener.h"
#include <exception>
#include <fstream>
#include <iostream>
#include <sstream>
#include <stdexcept>
std::optional<System> SystemParserInterface::parseFile(const std::string &filename)
{
    std::ifstream file(filename);
    if (!file) {
        std::cerr << "Error: Could not open file: " << filename << std::endl;
        return std::nullopt;
    }

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

std::optional<System> SystemParserInterface::parseString(const std::string &inputStr)
{
    antlr4::ANTLRInputStream input(inputStr);
    try {
        return parseInternal(input);
    } catch (std::exception &e) {
        throw std::invalid_argument(e.what());
    }
}

std::optional<System> SystemParserInterface::parseInternal(antlr4::ANTLRInputStream &input)
{
    parser::DQGrammarLexer lexer(&input);
    antlr4::CommonTokenStream tokens(&lexer);
    parser::DQGrammarParser parser(&tokens);

    SystemErrorListener errorListener;
    lexer.removeErrorListeners();
    parser.removeErrorListeners();
    lexer.addErrorListener(&errorListener);
    parser.addErrorListener(&errorListener);

    try {
        auto tree = parser.start();
        SystemBuilderVisitor visitor;
        visitor.visitStart(tree);
        return visitor.getSystem();
    } catch (std::exception &e) {
        throw std::invalid_argument(e.what());
    }
}

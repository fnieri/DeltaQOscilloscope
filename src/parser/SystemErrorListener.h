#include "antlr4-runtime.h"
#include <iostream>
#include <stdexcept>

class SystemErrorListener : public antlr4::BaseErrorListener
{
public:
    void syntaxError(antlr4::Recognizer *recognizer, antlr4::Token *offendingSymbol, size_t line, size_t charPositionInLine, const std::string &msg,
        std::exception_ptr e) override
    {
        throw std::runtime_error("Syntax error at line " + std::to_string(line) + ":" + std::to_string(charPositionInLine) + " - " + msg);
    }
};

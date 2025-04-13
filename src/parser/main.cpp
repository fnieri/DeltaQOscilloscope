
#include "DQGrammarLexer.h"
#include "DQGrammarParser.h"
#include <fstream>
#include <iostream>

using namespace antlr4;

int main(int argc, const char *argv[])
{
    std::ifstream stream("./src/parser/example.dq");
    // std::cout << stream.rdbuf() << "\n";
    ANTLRInputStream input(stream);

    parser::DQGrammarLexer lexer(&input);
    CommonTokenStream tokens(&lexer);
    parser::DQGrammarParser parser(&tokens);

    tree::ParseTree *tree = parser.start(); // entry rule
    std::cout << tree->toStringTree(&parser) << std::endl;
    std::cout << "Parsed successfully!" << std::endl;

    return 0;
}

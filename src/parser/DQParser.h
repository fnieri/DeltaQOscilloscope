#pragma once
#include "../diagram/System.h"
#include <stdexcept>
#include <string>
#include <vector>

// Hand-rolled recursive descent parser for DQGrammar.g4.
// Replaces the ANTLR runtime + generated files + SystemBuilderVisitor entirely.
// Same public surface as SystemParserInterface — just swap the include.
class DQParser
{
public:
    explicit DQParser(std::string input);
    System parse();

private:
    // ----- Lexer -----

    enum class TT {
        Ident, Number,
        Eq, Semi, Arrow, Colon, Comma,
        LBrack, RBrack, LParen, RParen,
        Eof
    };

    struct Token {
        TT          type;
        std::string val;
        int         line = 1;
    };

    std::vector<Token> tokens_;
    std::size_t        pos_ = 0;

    void tokenize(const std::string& src);

    // ----- Parser helpers -----

    const Token& peek() const;
    const Token& consume();
    const Token& expect(TT type, const char* what);
    bool          check(TT type) const;
    bool          checkVal(const char* val) const;

    // ----- Grammar rules -----

    void                                              parseDefinition(
        std::unordered_map<std::string, std::shared_ptr<Probe>>&    probes,
        std::unordered_map<std::string, std::shared_ptr<Operator>>& operators,
        std::unordered_map<std::string, std::shared_ptr<Outcome>>&  outcomes,
        std::unordered_set<std::string>&                            allNames,
        std::map<std::string, std::vector<std::string>>&            deps,
        std::string&                                                currentProbe);

    void                                              parseSystemDecl(
        std::unordered_map<std::string, std::shared_ptr<Probe>>&    probes,
        std::unordered_map<std::string, std::shared_ptr<Operator>>& operators,
        std::unordered_map<std::string, std::shared_ptr<Outcome>>&  outcomes);

    std::vector<std::shared_ptr<Observable>>          parseChain(
        std::unordered_map<std::string, std::shared_ptr<Probe>>&    probes,
        std::unordered_map<std::string, std::shared_ptr<Operator>>& operators,
        std::unordered_map<std::string, std::shared_ptr<Outcome>>&  outcomes,
        std::unordered_set<std::string>&                            allNames,
        std::map<std::string, std::vector<std::string>>&            deps,
        const std::string&                                          currentProbe);

    std::shared_ptr<Observable>                       parseComponent(
        std::unordered_map<std::string, std::shared_ptr<Probe>>&    probes,
        std::unordered_map<std::string, std::shared_ptr<Operator>>& operators,
        std::unordered_map<std::string, std::shared_ptr<Outcome>>&  outcomes,
        std::unordered_set<std::string>&                            allNames,
        std::map<std::string, std::vector<std::string>>&            deps,
        const std::string&                                          currentProbe);

    std::vector<double>                               parseProbList();

    std::vector<std::vector<std::shared_ptr<Observable>>> parseComponentList(
        std::unordered_map<std::string, std::shared_ptr<Probe>>&    probes,
        std::unordered_map<std::string, std::shared_ptr<Operator>>& operators,
        std::unordered_map<std::string, std::shared_ptr<Outcome>>&  outcomes,
        std::unordered_set<std::string>&                            allNames,
        std::map<std::string, std::vector<std::string>>&            deps,
        const std::string&                                          currentProbe);

    // ----- Cycle detection (same DFS as SystemBuilderVisitor) -----

    void checkCycles(const std::map<std::string, std::vector<std::string>>& deps) const;
    bool hasCycle(const std::string& node,
                  const std::map<std::string, std::vector<std::string>>& deps,
                  std::set<std::string>& visited,
                  std::set<std::string>& stack) const;

    [[noreturn]] void fail(const std::string& msg) const;

    std::string src_;
};

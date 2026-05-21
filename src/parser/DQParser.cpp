#include "DQParser.h"
#include <cctype>
#include <locale>
#include <stdexcept>

// ============================================================
// Lexer
// ============================================================

DQParser::DQParser(std::string input) : src_(std::move(input))
{
    tokenize(src_);
}

void DQParser::tokenize(const std::string& src)
{
    int line = 1;
    std::size_t i = 0;
    while (i < src.size()) {
        char c = src[i];

        if (c == '\n') { ++line; ++i; continue; }
        if (std::isspace(static_cast<unsigned char>(c))) { ++i; continue; }

        // Arrow before '=' so "->" is matched before a lone '-' would confuse things
        if (c == '-' && i + 1 < src.size() && src[i + 1] == '>') {
            tokens_.push_back({TT::Arrow, "->", line});
            i += 2; continue;
        }

        switch (c) {
        case '=': tokens_.push_back({TT::Eq,     "=", line}); ++i; continue;
        case ';': tokens_.push_back({TT::Semi,    ";", line}); ++i; continue;
        case ':': tokens_.push_back({TT::Colon,   ":", line}); ++i; continue;
        case ',': tokens_.push_back({TT::Comma,   ",", line}); ++i; continue;
        case '[': tokens_.push_back({TT::LBrack,  "[", line}); ++i; continue;
        case ']': tokens_.push_back({TT::RBrack,  "]", line}); ++i; continue;
        case '(': tokens_.push_back({TT::LParen,  "(", line}); ++i; continue;
        case ')': tokens_.push_back({TT::RParen,  ")", line}); ++i; continue;
        default: break;
        }

        if (std::isdigit(static_cast<unsigned char>(c))) {
            std::size_t start = i;
            while (i < src.size() && std::isdigit(static_cast<unsigned char>(src[i]))) ++i;
            if (i < src.size() && src[i] == '.') {
                ++i;
                while (i < src.size() && std::isdigit(static_cast<unsigned char>(src[i]))) ++i;
            }
            tokens_.push_back({TT::Number, src.substr(start, i - start), line});
            continue;
        }

        if (std::isalpha(static_cast<unsigned char>(c)) || c == '_') {
            std::size_t start = i;
            while (i < src.size() &&
                   (std::isalnum(static_cast<unsigned char>(src[i])) || src[i] == '_'))
                ++i;
            tokens_.push_back({TT::Ident, src.substr(start, i - start), line});
            continue;
        }

        fail("unexpected character '" + std::string(1, c) + "'");
    }
    tokens_.push_back({TT::Eof, "", line});
}

// ============================================================
// Parser helpers
// ============================================================

const DQParser::Token& DQParser::peek() const
{
    return tokens_[pos_];
}

const DQParser::Token& DQParser::consume()
{
    return tokens_[pos_++];
}

const DQParser::Token& DQParser::expect(TT type, const char* what)
{
    if (peek().type != type)
        fail(std::string("expected ") + what + ", got '" + peek().val + "'");
    return consume();
}

bool DQParser::check(TT type) const
{
    return peek().type == type;
}

bool DQParser::checkVal(const char* val) const
{
    return peek().type == TT::Ident && peek().val == val;
}

[[noreturn]] void DQParser::fail(const std::string& msg) const
{
    int line = pos_ < tokens_.size() ? tokens_[pos_].line : -1;
    throw std::invalid_argument("Parse error (line " + std::to_string(line) + "): " + msg);
}

// ============================================================
// Top-level parse
// ============================================================

System DQParser::parse()
{
    std::unordered_map<std::string, std::shared_ptr<Probe>>    probes;
    std::unordered_map<std::string, std::shared_ptr<Operator>> operators;
    std::unordered_map<std::string, std::shared_ptr<Outcome>>  outcomes;
    std::unordered_set<std::string>                            allNames;
    std::map<std::string, std::vector<std::string>>            deps;
    std::string                                                currentProbe;

    // definition* system? EOF
    while (!check(TT::Eof) && !checkVal("system")) {
        parseDefinition(probes, operators, outcomes, allNames, deps, currentProbe);
    }

    System system;

    if (checkVal("system")) {
        parseSystemDecl(probes, operators, outcomes);
    }

    expect(TT::Eof, "end of input");

    checkCycles(deps);

    system.setOutcomes(outcomes);
    system.setProbes(probes);
    system.setOperators(operators);
    return system;
}

// ============================================================
// Grammar rules
// ============================================================

// definition: IDENTIFIER '=' component_chain ';'
void DQParser::parseDefinition(
    std::unordered_map<std::string, std::shared_ptr<Probe>>&    probes,
    std::unordered_map<std::string, std::shared_ptr<Operator>>& operators,
    std::unordered_map<std::string, std::shared_ptr<Outcome>>&  outcomes,
    std::unordered_set<std::string>&                            allNames,
    std::map<std::string, std::vector<std::string>>&            deps,
    std::string&                                                currentProbe)
{
    const std::string name = expect(TT::Ident, "probe name").val;
    if (probes.count(name))
        throw std::invalid_argument("Probe has already been defined");
    if (allNames.count(name))
        throw std::invalid_argument("Duplicate name detected: " + name);

    allNames.insert(name);
    currentProbe = name;

    expect(TT::Eq, "'='");
    auto chain = parseChain(probes, operators, outcomes, allNames, deps, currentProbe);
    expect(TT::Semi, "';'");

    probes[name] = std::make_shared<Probe>(name, chain);
    currentProbe.clear();
}

// system: 'system' '=' component_chain ';'?
void DQParser::parseSystemDecl(
    std::unordered_map<std::string, std::shared_ptr<Probe>>&    probes,
    std::unordered_map<std::string, std::shared_ptr<Operator>>& operators,
    std::unordered_map<std::string, std::shared_ptr<Outcome>>&  outcomes)
{
    consume(); // 'system'
    expect(TT::Eq, "'='");

    std::unordered_set<std::string>                 allNames;
    std::map<std::string, std::vector<std::string>> deps;
    std::string                                     noProbe;
    parseChain(probes, operators, outcomes, allNames, deps, noProbe);

    if (check(TT::Semi)) consume();
}

// component_chain: component ('->' component)*
std::vector<std::shared_ptr<Observable>> DQParser::parseChain(
    std::unordered_map<std::string, std::shared_ptr<Probe>>&    probes,
    std::unordered_map<std::string, std::shared_ptr<Operator>>& operators,
    std::unordered_map<std::string, std::shared_ptr<Outcome>>&  outcomes,
    std::unordered_set<std::string>&                            allNames,
    std::map<std::string, std::vector<std::string>>&            deps,
    const std::string&                                          currentProbe)
{
    std::vector<std::shared_ptr<Observable>> chain;
    chain.push_back(parseComponent(probes, operators, outcomes, allNames, deps, currentProbe));

    while (check(TT::Arrow)) {
        consume();
        chain.push_back(parseComponent(probes, operators, outcomes, allNames, deps, currentProbe));
    }
    return chain;
}

// component: behaviorComponent | probeComponent | outcome
// Disambiguate with 1-token lookahead: if next is 's'|'f'|'a'|'p' and peek+1 is ':', it's a typed component.
std::shared_ptr<Observable> DQParser::parseComponent(
    std::unordered_map<std::string, std::shared_ptr<Probe>>&    probes,
    std::unordered_map<std::string, std::shared_ptr<Operator>>& operators,
    std::unordered_map<std::string, std::shared_ptr<Outcome>>&  outcomes,
    std::unordered_set<std::string>&                            allNames,
    std::map<std::string, std::vector<std::string>>&            deps,
    const std::string&                                          currentProbe)
{
    if (!check(TT::Ident))
        fail("expected component");

    const std::string& head = peek().val;
    bool nextIsColon = (pos_ + 1 < tokens_.size() && tokens_[pos_ + 1].type == TT::Colon);

    // probeComponent: 's' ':' IDENTIFIER
    if (head == "s" && nextIsColon) {
        consume(); // 's'
        consume(); // ':'
        const std::string name = expect(TT::Ident, "probe name").val;

        if (!currentProbe.empty())
            deps[currentProbe].push_back(name);

        if (probes.count(name))
            return std::dynamic_pointer_cast<Observable>(probes[name]);

        auto p = std::make_shared<Probe>(name);
        probes[name] = p;
        return std::dynamic_pointer_cast<Observable>(p);
    }

    // behaviorComponent: ('f'|'a'|'p') ':' IDENTIFIER ...
    if ((head == "f" || head == "a" || head == "p") && nextIsColon) {
        const std::string typeStr = consume().val; // 'f'|'a'|'p'
        consume();                                  // ':'
        const std::string name = expect(TT::Ident, "operator name").val;

        if (operators.count(name))
            return std::dynamic_pointer_cast<Observable>(operators[name]);
        if (allNames.count(name))
            throw std::invalid_argument("Duplicate name detected: " + name);
        allNames.insert(name);

        OperatorType type;
        if      (typeStr == "a") type = OperatorType::ATF;
        else if (typeStr == "f") type = OperatorType::FTF;
        else                     type = OperatorType::PRB;

        auto op = std::make_shared<Operator>(name, type);

        std::vector<double> probs;
        if (check(TT::LBrack)) {
            consume();
            probs = parseProbList();
            expect(TT::RBrack, "']'");
        }

        if (type == OperatorType::PRB && probs.empty())
            throw std::invalid_argument("A probabilistic operator must have probabilities");
        if (type != OperatorType::PRB && !probs.empty())
            throw std::invalid_argument("A non-probabilistic operator cannot have probabilities");
        if (!probs.empty())
            op->setProbabilities(probs);

        expect(TT::LParen, "'('");
        auto chains = parseComponentList(probes, operators, outcomes, allNames, deps, currentProbe);
        expect(TT::RParen, "')'");

        std::vector<std::vector<std::shared_ptr<Observable>>> links;
        for (auto& c : chains)
            if (!c.empty()) links.push_back(c);
        op->setCausalLinks(links);

        operators[name] = op;
        return std::dynamic_pointer_cast<Observable>(op);
    }

    // outcome: IDENTIFIER
    const std::string name = consume().val;
    if (outcomes.count(name))
        return std::dynamic_pointer_cast<Observable>(outcomes[name]);
    if (allNames.count(name))
        throw std::invalid_argument("Duplicate name detected: " + name);
    allNames.insert(name);

    auto outcome = std::make_shared<Outcome>(name);
    outcomes[name] = outcome;
    return std::dynamic_pointer_cast<Observable>(outcome);
}

// probability_list: NUMBER (',' NUMBER)+
std::vector<double> DQParser::parseProbList()
{
    std::locale::global(std::locale("C"));
    std::vector<double> probs;
    probs.push_back(std::stod(expect(TT::Number, "number").val));
    while (check(TT::Comma)) {
        consume();
        probs.push_back(std::stod(expect(TT::Number, "number").val));
    }
    return probs;
}

// component_list: component_chain (',' component_chain)+
std::vector<std::vector<std::shared_ptr<Observable>>> DQParser::parseComponentList(
    std::unordered_map<std::string, std::shared_ptr<Probe>>&    probes,
    std::unordered_map<std::string, std::shared_ptr<Operator>>& operators,
    std::unordered_map<std::string, std::shared_ptr<Outcome>>&  outcomes,
    std::unordered_set<std::string>&                            allNames,
    std::map<std::string, std::vector<std::string>>&            deps,
    const std::string&                                          currentProbe)
{
    std::vector<std::vector<std::shared_ptr<Observable>>> chains;
    chains.push_back(parseChain(probes, operators, outcomes, allNames, deps, currentProbe));
    while (check(TT::Comma)) {
        consume();
        chains.push_back(parseChain(probes, operators, outcomes, allNames, deps, currentProbe));
    }
    return chains;
}

// ============================================================
// Cycle detection — identical DFS logic as SystemBuilderVisitor
// ============================================================

void DQParser::checkCycles(const std::map<std::string, std::vector<std::string>>& deps) const
{
    std::set<std::string> visited, stack;
    for (const auto& [node, _] : deps)
        if (hasCycle(node, deps, visited, stack))
            throw std::invalid_argument("Cycle detected in system definition involving: " + node);
}

bool DQParser::hasCycle(const std::string& node,
                        const std::map<std::string, std::vector<std::string>>& deps,
                        std::set<std::string>& visited,
                        std::set<std::string>& stack) const
{
    if (stack.count(node))  return true;
    if (visited.count(node)) return false;
    visited.insert(node);
    stack.insert(node);
    auto it = deps.find(node);
    if (it != deps.end())
        for (const auto& nb : it->second)
            if (hasCycle(nb, deps, visited, stack)) return true;
    stack.erase(node);
    return false;
}

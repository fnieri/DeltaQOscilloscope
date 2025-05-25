#ifndef SYSTEMBUILDER_H
#define SYSTEMBUILDER_H

#include "../diagram/System.h"
#include "DQGrammarVisitor.h"
#include <memory>
#include <unordered_map>

/**
 * @brief Visitor class that builds a System from a parsed grammar tree.
 */
class SystemBuilderVisitor : public parser::DQGrammarVisitor
{
private:
    std::unordered_map<std::string, std::shared_ptr<Outcome>> outcomes;   ///< Map of outcome names to Outcome objects.
    std::unordered_map<std::string, std::shared_ptr<Operator>> operators; ///< Map of operator names to Operator objects.
    std::unordered_map<std::string, std::shared_ptr<Probe>> probes;       ///< Map of probe names to Probe objects.

    std::vector<std::string> definedProbes; ///< List of defined probe names.
    System system; ///< The final system constructed by the visitor.

    std::string currentlyBuildingProbe; ///< Tracks the probe currently being built for dependency management.
    std::map<std::string, std::vector<std::string>> dependencies; ///< Graph of probe dependencies.

    std::unordered_map<std::string, std::vector<std::string>> definitionLinks; ///< For debugging: links between definitions.
    std::unordered_map<std::string, std::vector<std::vector<std::string>>> operatorLinks; ///< For debugging: operator chains.
    std::vector<std::string> systemLinks; ///< Top-level system observable links.
    std::unordered_set<std::string> allNames; ///< Tracks all used names to detect duplicates.

    /**
     * @brief Checks the dependency graph for cycles.
     * @throws std::invalid_argument if a cycle is detected.
     */
    void checkForCycles() const;

    /**
     * @brief Recursive utility to detect cycles in a graph.
     * @param node Current node.
     * @param visited Set of visited nodes.
     * @param recursionStack Stack of nodes in the current DFS path.
     * @return True if a cycle is found.
     */
    bool hasCycle(const std::string& node,
                  std::set<std::string>& visited,
                  std::set<std::string>& recursionStack) const;

public:
    /**
     * @brief Returns the constructed System.
     * @return The system object.
     */
    System getSystem() const;

    // Visitor overrides from DQGrammarVisitor:
    std::any visitStart(parser::DQGrammarParser::StartContext *context) override;
    std::any visitDefinition(parser::DQGrammarParser::DefinitionContext *context) override;
    std::any visitSystem(parser::DQGrammarParser::SystemContext *context) override;
    std::any visitComponent(parser::DQGrammarParser::ComponentContext *context) override;
    std::any visitBehaviorComponent(parser::DQGrammarParser::BehaviorComponentContext *context) override;
    std::any visitProbeComponent(parser::DQGrammarParser::ProbeComponentContext *context) override;
    std::any visitProbability_list(parser::DQGrammarParser::Probability_listContext *context) override;
    std::any visitComponent_list(parser::DQGrammarParser::Component_listContext *context) override;
    std::any visitOutcome(parser::DQGrammarParser::OutcomeContext *context) override;
    std::any visitComponent_chain(parser::DQGrammarParser::Component_chainContext *context) override;
};

#endif // SYSTEMBUILDER_H

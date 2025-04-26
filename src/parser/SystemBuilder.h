#ifndef SYSTEMBUILDER_H
#define SYSTEMBUILDER_H

#include "../diagram/System.h"
#include "DQGrammarVisitor.h"
#include <memory>
#include <unordered_map>

class SystemBuilderVisitor : public parser::DQGrammarVisitor
{
private:
    std::unordered_map<std::string, std::shared_ptr<Outcome>> outcomes;
    std::unordered_map<std::string, std::shared_ptr<Operator>> operators;
    std::unordered_map<std::string, std::shared_ptr<Probe>> probes;
    std::vector<std::string> definedProbes;
    System system;
    std::string currentProbe; // Track current probe context for setting next components
    std::unordered_map<std::string, std::vector<std::string>> definitionLinks;
    std::unordered_map<std::string, std::vector<std::vector<std::string>>> operatorLinks;
    std::vector<std::string> systemLinks;

public:
    System getSystem() const;

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

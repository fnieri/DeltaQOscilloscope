
// Generated from DQGrammar.g4 by ANTLR 4.13.2

#pragma once


#include "antlr4-runtime.h"
#include "DQGrammarParser.h"



/**
 * This class defines an abstract visitor for a parse tree
 * produced by DQGrammarParser.
 */
class  DQGrammarVisitor : public antlr4::tree::AbstractParseTreeVisitor {
public:

  /**
   * Visit parse trees produced by DQGrammarParser.
   */
    virtual std::any visitStart(DQGrammarParser::StartContext *context) = 0;

    virtual std::any visitDefinition(DQGrammarParser::DefinitionContext *context) = 0;

    virtual std::any visitSystem(DQGrammarParser::SystemContext *context) = 0;

    virtual std::any visitComponent(DQGrammarParser::ComponentContext *context) = 0;

    virtual std::any visitBehaviorComponent(DQGrammarParser::BehaviorComponentContext *context) = 0;

    virtual std::any visitProbeComponent(DQGrammarParser::ProbeComponentContext *context) = 0;

    virtual std::any visitProbability_list(DQGrammarParser::Probability_listContext *context) = 0;

    virtual std::any visitComponent_list(DQGrammarParser::Component_listContext *context) = 0;

    virtual std::any visitOutcome(DQGrammarParser::OutcomeContext *context) = 0;


};


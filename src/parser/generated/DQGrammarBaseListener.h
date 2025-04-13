
// Generated from DQGrammar.g4 by ANTLR 4.13.2

#pragma once


#include "antlr4-runtime.h"
#include "DQGrammarListener.h"


/**
 * This class provides an empty implementation of DQGrammarListener,
 * which can be extended to create a listener which only needs to handle a subset
 * of the available methods.
 */
class  DQGrammarBaseListener : public DQGrammarListener {
public:

  virtual void enterStart(DQGrammarParser::StartContext * /*ctx*/) override { }
  virtual void exitStart(DQGrammarParser::StartContext * /*ctx*/) override { }

  virtual void enterDefinition(DQGrammarParser::DefinitionContext * /*ctx*/) override { }
  virtual void exitDefinition(DQGrammarParser::DefinitionContext * /*ctx*/) override { }

  virtual void enterSystem(DQGrammarParser::SystemContext * /*ctx*/) override { }
  virtual void exitSystem(DQGrammarParser::SystemContext * /*ctx*/) override { }

  virtual void enterOutcomeComponent(DQGrammarParser::OutcomeComponentContext * /*ctx*/) override { }
  virtual void exitOutcomeComponent(DQGrammarParser::OutcomeComponentContext * /*ctx*/) override { }

  virtual void enterBehaviorComponent(DQGrammarParser::BehaviorComponentContext * /*ctx*/) override { }
  virtual void exitBehaviorComponent(DQGrammarParser::BehaviorComponentContext * /*ctx*/) override { }

  virtual void enterProbeComponent(DQGrammarParser::ProbeComponentContext * /*ctx*/) override { }
  virtual void exitProbeComponent(DQGrammarParser::ProbeComponentContext * /*ctx*/) override { }

  virtual void enterProbability_list(DQGrammarParser::Probability_listContext * /*ctx*/) override { }
  virtual void exitProbability_list(DQGrammarParser::Probability_listContext * /*ctx*/) override { }

  virtual void enterComponent_list(DQGrammarParser::Component_listContext * /*ctx*/) override { }
  virtual void exitComponent_list(DQGrammarParser::Component_listContext * /*ctx*/) override { }

  virtual void enterOutcome(DQGrammarParser::OutcomeContext * /*ctx*/) override { }
  virtual void exitOutcome(DQGrammarParser::OutcomeContext * /*ctx*/) override { }


  virtual void enterEveryRule(antlr4::ParserRuleContext * /*ctx*/) override { }
  virtual void exitEveryRule(antlr4::ParserRuleContext * /*ctx*/) override { }
  virtual void visitTerminal(antlr4::tree::TerminalNode * /*node*/) override { }
  virtual void visitErrorNode(antlr4::tree::ErrorNode * /*node*/) override { }

};


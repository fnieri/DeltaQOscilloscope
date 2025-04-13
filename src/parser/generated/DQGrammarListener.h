
// Generated from DQGrammar.g4 by ANTLR 4.13.2

#pragma once


#include "antlr4-runtime.h"
#include "DQGrammarParser.h"


/**
 * This interface defines an abstract listener for a parse tree produced by DQGrammarParser.
 */
class  DQGrammarListener : public antlr4::tree::ParseTreeListener {
public:

  virtual void enterStart(DQGrammarParser::StartContext *ctx) = 0;
  virtual void exitStart(DQGrammarParser::StartContext *ctx) = 0;

  virtual void enterDefinition(DQGrammarParser::DefinitionContext *ctx) = 0;
  virtual void exitDefinition(DQGrammarParser::DefinitionContext *ctx) = 0;

  virtual void enterSystem(DQGrammarParser::SystemContext *ctx) = 0;
  virtual void exitSystem(DQGrammarParser::SystemContext *ctx) = 0;

  virtual void enterOutcomeComponent(DQGrammarParser::OutcomeComponentContext *ctx) = 0;
  virtual void exitOutcomeComponent(DQGrammarParser::OutcomeComponentContext *ctx) = 0;

  virtual void enterBehaviorComponent(DQGrammarParser::BehaviorComponentContext *ctx) = 0;
  virtual void exitBehaviorComponent(DQGrammarParser::BehaviorComponentContext *ctx) = 0;

  virtual void enterProbeComponent(DQGrammarParser::ProbeComponentContext *ctx) = 0;
  virtual void exitProbeComponent(DQGrammarParser::ProbeComponentContext *ctx) = 0;

  virtual void enterProbability_list(DQGrammarParser::Probability_listContext *ctx) = 0;
  virtual void exitProbability_list(DQGrammarParser::Probability_listContext *ctx) = 0;

  virtual void enterComponent_list(DQGrammarParser::Component_listContext *ctx) = 0;
  virtual void exitComponent_list(DQGrammarParser::Component_listContext *ctx) = 0;

  virtual void enterOutcome(DQGrammarParser::OutcomeContext *ctx) = 0;
  virtual void exitOutcome(DQGrammarParser::OutcomeContext *ctx) = 0;


};


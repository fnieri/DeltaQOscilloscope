
// Generated from /home/francy/Desktop/RealTimeDeltaQSD/./src/parser/DQGrammar.g4 by ANTLR 4.13.2

#pragma once


#include "antlr4-runtime.h"
#include "DQGrammarVisitor.h"


namespace parser {

/**
 * This class provides an empty implementation of DQGrammarVisitor, which can be
 * extended to create a visitor which only needs to handle a subset of the available methods.
 */
class  DQGrammarBaseVisitor : public DQGrammarVisitor {
public:

  virtual std::any visitStart(DQGrammarParser::StartContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitDefinition(DQGrammarParser::DefinitionContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitSystem(DQGrammarParser::SystemContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitComponent_chain(DQGrammarParser::Component_chainContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitComponent(DQGrammarParser::ComponentContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitBehaviorComponent(DQGrammarParser::BehaviorComponentContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitProbeComponent(DQGrammarParser::ProbeComponentContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitProbability_list(DQGrammarParser::Probability_listContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitComponent_list(DQGrammarParser::Component_listContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual std::any visitOutcome(DQGrammarParser::OutcomeContext *ctx) override {
    return visitChildren(ctx);
  }


};

}  // namespace parser

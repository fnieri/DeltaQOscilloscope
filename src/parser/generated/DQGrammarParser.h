
// Generated from DQGrammar.g4 by ANTLR 4.13.2

#pragma once


#include "antlr4-runtime.h"




class  DQGrammarParser : public antlr4::Parser {
public:
  enum {
    T__0 = 1, T__1 = 2, T__2 = 3, T__3 = 4, T__4 = 5, T__5 = 6, T__6 = 7, 
    T__7 = 8, T__8 = 9, T__9 = 10, T__10 = 11, WS = 12, IDENTIFIER = 13, 
    NUMBER = 14, BEHAVIOR_TYPE = 15
  };

  enum {
    RuleStart = 0, RuleDefinition = 1, RuleSystem = 2, RuleComponent = 3, 
    RuleProbability_list = 4, RuleComponent_list = 5, RuleOutcome = 6
  };

  explicit DQGrammarParser(antlr4::TokenStream *input);

  DQGrammarParser(antlr4::TokenStream *input, const antlr4::atn::ParserATNSimulatorOptions &options);

  ~DQGrammarParser() override;

  std::string getGrammarFileName() const override;

  const antlr4::atn::ATN& getATN() const override;

  const std::vector<std::string>& getRuleNames() const override;

  const antlr4::dfa::Vocabulary& getVocabulary() const override;

  antlr4::atn::SerializedATNView getSerializedATN() const override;


  class StartContext;
  class DefinitionContext;
  class SystemContext;
  class ComponentContext;
  class Probability_listContext;
  class Component_listContext;
  class OutcomeContext; 

  class  StartContext : public antlr4::ParserRuleContext {
  public:
    StartContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    antlr4::tree::TerminalNode *EOF();
    std::vector<DefinitionContext *> definition();
    DefinitionContext* definition(size_t i);
    SystemContext *system();


    virtual std::any accept(antlr4::tree::ParseTreeVisitor *visitor) override;
   
  };

  StartContext* start();

  class  DefinitionContext : public antlr4::ParserRuleContext {
  public:
    DefinitionContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    antlr4::tree::TerminalNode *IDENTIFIER();
    std::vector<ComponentContext *> component();
    ComponentContext* component(size_t i);


    virtual std::any accept(antlr4::tree::ParseTreeVisitor *visitor) override;
   
  };

  DefinitionContext* definition();

  class  SystemContext : public antlr4::ParserRuleContext {
  public:
    SystemContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    std::vector<ComponentContext *> component();
    ComponentContext* component(size_t i);


    virtual std::any accept(antlr4::tree::ParseTreeVisitor *visitor) override;
   
  };

  SystemContext* system();

  class  ComponentContext : public antlr4::ParserRuleContext {
  public:
    ComponentContext(antlr4::ParserRuleContext *parent, size_t invokingState);
   
    ComponentContext() = default;
    void copyFrom(ComponentContext *context);
    using antlr4::ParserRuleContext::copyFrom;

    virtual size_t getRuleIndex() const override;

   
  };

  class  ProbeComponentContext : public ComponentContext {
  public:
    ProbeComponentContext(ComponentContext *ctx);

    antlr4::tree::TerminalNode *IDENTIFIER();
    ComponentContext *component();

    virtual std::any accept(antlr4::tree::ParseTreeVisitor *visitor) override;
  };

  class  OutcomeComponentContext : public ComponentContext {
  public:
    OutcomeComponentContext(ComponentContext *ctx);

    OutcomeContext *outcome();
    ComponentContext *component();

    virtual std::any accept(antlr4::tree::ParseTreeVisitor *visitor) override;
  };

  class  BehaviorComponentContext : public ComponentContext {
  public:
    BehaviorComponentContext(ComponentContext *ctx);

    antlr4::tree::TerminalNode *BEHAVIOR_TYPE();
    antlr4::tree::TerminalNode *IDENTIFIER();
    Component_listContext *component_list();
    Probability_listContext *probability_list();
    ComponentContext *component();

    virtual std::any accept(antlr4::tree::ParseTreeVisitor *visitor) override;
  };

  ComponentContext* component();

  class  Probability_listContext : public antlr4::ParserRuleContext {
  public:
    Probability_listContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    std::vector<antlr4::tree::TerminalNode *> NUMBER();
    antlr4::tree::TerminalNode* NUMBER(size_t i);


    virtual std::any accept(antlr4::tree::ParseTreeVisitor *visitor) override;
   
  };

  Probability_listContext* probability_list();

  class  Component_listContext : public antlr4::ParserRuleContext {
  public:
    Component_listContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    std::vector<ComponentContext *> component();
    ComponentContext* component(size_t i);


    virtual std::any accept(antlr4::tree::ParseTreeVisitor *visitor) override;
   
  };

  Component_listContext* component_list();

  class  OutcomeContext : public antlr4::ParserRuleContext {
  public:
    OutcomeContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    antlr4::tree::TerminalNode *IDENTIFIER();


    virtual std::any accept(antlr4::tree::ParseTreeVisitor *visitor) override;
   
  };

  OutcomeContext* outcome();


  // By default the static state used to implement the parser is lazily initialized during the first
  // call to the constructor. You can call this function if you wish to initialize the static state
  // ahead of time.
  static void initialize();

private:
};


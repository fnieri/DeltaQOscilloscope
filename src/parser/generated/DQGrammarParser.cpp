
// Generated from /home/francy/Desktop/RealTimeDeltaQSD/./src/parser/DQGrammar.g4 by ANTLR 4.13.2


#include "DQGrammarVisitor.h"

#include "DQGrammarParser.h"


using namespace antlrcpp;
using namespace parser;

using namespace antlr4;

namespace {

struct DQGrammarParserStaticData final {
  DQGrammarParserStaticData(std::vector<std::string> ruleNames,
                        std::vector<std::string> literalNames,
                        std::vector<std::string> symbolicNames)
      : ruleNames(std::move(ruleNames)), literalNames(std::move(literalNames)),
        symbolicNames(std::move(symbolicNames)),
        vocabulary(this->literalNames, this->symbolicNames) {}

  DQGrammarParserStaticData(const DQGrammarParserStaticData&) = delete;
  DQGrammarParserStaticData(DQGrammarParserStaticData&&) = delete;
  DQGrammarParserStaticData& operator=(const DQGrammarParserStaticData&) = delete;
  DQGrammarParserStaticData& operator=(DQGrammarParserStaticData&&) = delete;

  std::vector<antlr4::dfa::DFA> decisionToDFA;
  antlr4::atn::PredictionContextCache sharedContextCache;
  const std::vector<std::string> ruleNames;
  const std::vector<std::string> literalNames;
  const std::vector<std::string> symbolicNames;
  const antlr4::dfa::Vocabulary vocabulary;
  antlr4::atn::SerializedATNView serializedATN;
  std::unique_ptr<antlr4::atn::ATN> atn;
};

::antlr4::internal::OnceFlag dqgrammarParserOnceFlag;
#if ANTLR4_USE_THREAD_LOCAL_CACHE
static thread_local
#endif
std::unique_ptr<DQGrammarParserStaticData> dqgrammarParserStaticData = nullptr;

void dqgrammarParserInitialize() {
#if ANTLR4_USE_THREAD_LOCAL_CACHE
  if (dqgrammarParserStaticData != nullptr) {
    return;
  }
#else
  assert(dqgrammarParserStaticData == nullptr);
#endif
  auto staticData = std::make_unique<DQGrammarParserStaticData>(
    std::vector<std::string>{
      "start", "definition", "system", "component_chain", "component", "behaviorComponent", 
      "probeComponent", "probability_list", "component_list", "outcome"
    },
    std::vector<std::string>{
      "", "'='", "';'", "'system'", "'->'", "':'", "'['", "']'", "'('", 
      "')'", "','", "'s'"
    },
    std::vector<std::string>{
      "", "", "", "", "", "", "", "", "", "", "", "PROBE_ID", "BEHAVIOR_TYPE", 
      "NUMBER", "IDENTIFIER", "WS"
    }
  );
  static const int32_t serializedATNSegment[] = {
  	4,1,15,89,2,0,7,0,2,1,7,1,2,2,7,2,2,3,7,3,2,4,7,4,2,5,7,5,2,6,7,6,2,7,
  	7,7,2,8,7,8,2,9,7,9,1,0,5,0,22,8,0,10,0,12,0,25,9,0,1,0,3,0,28,8,0,1,
  	0,1,0,1,1,1,1,1,1,1,1,1,1,1,2,1,2,1,2,1,2,3,2,41,8,2,1,3,1,3,1,3,5,3,
  	46,8,3,10,3,12,3,49,9,3,1,4,1,4,1,4,3,4,54,8,4,1,5,1,5,1,5,1,5,1,5,1,
  	5,1,5,3,5,63,8,5,1,5,1,5,1,5,1,5,1,6,1,6,1,6,1,6,1,7,1,7,1,7,4,7,76,8,
  	7,11,7,12,7,77,1,8,1,8,1,8,4,8,83,8,8,11,8,12,8,84,1,9,1,9,1,9,0,0,10,
  	0,2,4,6,8,10,12,14,16,18,0,0,87,0,23,1,0,0,0,2,31,1,0,0,0,4,36,1,0,0,
  	0,6,42,1,0,0,0,8,53,1,0,0,0,10,55,1,0,0,0,12,68,1,0,0,0,14,72,1,0,0,0,
  	16,79,1,0,0,0,18,86,1,0,0,0,20,22,3,2,1,0,21,20,1,0,0,0,22,25,1,0,0,0,
  	23,21,1,0,0,0,23,24,1,0,0,0,24,27,1,0,0,0,25,23,1,0,0,0,26,28,3,4,2,0,
  	27,26,1,0,0,0,27,28,1,0,0,0,28,29,1,0,0,0,29,30,5,0,0,1,30,1,1,0,0,0,
  	31,32,5,14,0,0,32,33,5,1,0,0,33,34,3,6,3,0,34,35,5,2,0,0,35,3,1,0,0,0,
  	36,37,5,3,0,0,37,38,5,1,0,0,38,40,3,6,3,0,39,41,5,2,0,0,40,39,1,0,0,0,
  	40,41,1,0,0,0,41,5,1,0,0,0,42,47,3,8,4,0,43,44,5,4,0,0,44,46,3,8,4,0,
  	45,43,1,0,0,0,46,49,1,0,0,0,47,45,1,0,0,0,47,48,1,0,0,0,48,7,1,0,0,0,
  	49,47,1,0,0,0,50,54,3,10,5,0,51,54,3,12,6,0,52,54,3,18,9,0,53,50,1,0,
  	0,0,53,51,1,0,0,0,53,52,1,0,0,0,54,9,1,0,0,0,55,56,5,12,0,0,56,57,5,5,
  	0,0,57,62,5,14,0,0,58,59,5,6,0,0,59,60,3,14,7,0,60,61,5,7,0,0,61,63,1,
  	0,0,0,62,58,1,0,0,0,62,63,1,0,0,0,63,64,1,0,0,0,64,65,5,8,0,0,65,66,3,
  	16,8,0,66,67,5,9,0,0,67,11,1,0,0,0,68,69,5,11,0,0,69,70,5,5,0,0,70,71,
  	5,14,0,0,71,13,1,0,0,0,72,75,5,13,0,0,73,74,5,10,0,0,74,76,5,13,0,0,75,
  	73,1,0,0,0,76,77,1,0,0,0,77,75,1,0,0,0,77,78,1,0,0,0,78,15,1,0,0,0,79,
  	82,3,6,3,0,80,81,5,10,0,0,81,83,3,6,3,0,82,80,1,0,0,0,83,84,1,0,0,0,84,
  	82,1,0,0,0,84,85,1,0,0,0,85,17,1,0,0,0,86,87,5,14,0,0,87,19,1,0,0,0,8,
  	23,27,40,47,53,62,77,84
  };
  staticData->serializedATN = antlr4::atn::SerializedATNView(serializedATNSegment, sizeof(serializedATNSegment) / sizeof(serializedATNSegment[0]));

  antlr4::atn::ATNDeserializer deserializer;
  staticData->atn = deserializer.deserialize(staticData->serializedATN);

  const size_t count = staticData->atn->getNumberOfDecisions();
  staticData->decisionToDFA.reserve(count);
  for (size_t i = 0; i < count; i++) { 
    staticData->decisionToDFA.emplace_back(staticData->atn->getDecisionState(i), i);
  }
  dqgrammarParserStaticData = std::move(staticData);
}

}

DQGrammarParser::DQGrammarParser(TokenStream *input) : DQGrammarParser(input, antlr4::atn::ParserATNSimulatorOptions()) {}

DQGrammarParser::DQGrammarParser(TokenStream *input, const antlr4::atn::ParserATNSimulatorOptions &options) : Parser(input) {
  DQGrammarParser::initialize();
  _interpreter = new atn::ParserATNSimulator(this, *dqgrammarParserStaticData->atn, dqgrammarParserStaticData->decisionToDFA, dqgrammarParserStaticData->sharedContextCache, options);
}

DQGrammarParser::~DQGrammarParser() {
  delete _interpreter;
}

const atn::ATN& DQGrammarParser::getATN() const {
  return *dqgrammarParserStaticData->atn;
}

std::string DQGrammarParser::getGrammarFileName() const {
  return "DQGrammar.g4";
}

const std::vector<std::string>& DQGrammarParser::getRuleNames() const {
  return dqgrammarParserStaticData->ruleNames;
}

const dfa::Vocabulary& DQGrammarParser::getVocabulary() const {
  return dqgrammarParserStaticData->vocabulary;
}

antlr4::atn::SerializedATNView DQGrammarParser::getSerializedATN() const {
  return dqgrammarParserStaticData->serializedATN;
}


//----------------- StartContext ------------------------------------------------------------------

DQGrammarParser::StartContext::StartContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

tree::TerminalNode* DQGrammarParser::StartContext::EOF() {
  return getToken(DQGrammarParser::EOF, 0);
}

std::vector<DQGrammarParser::DefinitionContext *> DQGrammarParser::StartContext::definition() {
  return getRuleContexts<DQGrammarParser::DefinitionContext>();
}

DQGrammarParser::DefinitionContext* DQGrammarParser::StartContext::definition(size_t i) {
  return getRuleContext<DQGrammarParser::DefinitionContext>(i);
}

DQGrammarParser::SystemContext* DQGrammarParser::StartContext::system() {
  return getRuleContext<DQGrammarParser::SystemContext>(0);
}


size_t DQGrammarParser::StartContext::getRuleIndex() const {
  return DQGrammarParser::RuleStart;
}


std::any DQGrammarParser::StartContext::accept(tree::ParseTreeVisitor *visitor) {
  if (auto parserVisitor = dynamic_cast<DQGrammarVisitor*>(visitor))
    return parserVisitor->visitStart(this);
  else
    return visitor->visitChildren(this);
}

DQGrammarParser::StartContext* DQGrammarParser::start() {
  StartContext *_localctx = _tracker.createInstance<StartContext>(_ctx, getState());
  enterRule(_localctx, 0, DQGrammarParser::RuleStart);
  size_t _la = 0;

#if __cplusplus > 201703L
  auto onExit = finally([=, this] {
#else
  auto onExit = finally([=] {
#endif
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(23);
    _errHandler->sync(this);
    _la = _input->LA(1);
    while (_la == DQGrammarParser::IDENTIFIER) {
      setState(20);
      definition();
      setState(25);
      _errHandler->sync(this);
      _la = _input->LA(1);
    }
    setState(27);
    _errHandler->sync(this);

    _la = _input->LA(1);
    if (_la == DQGrammarParser::T__2) {
      setState(26);
      system();
    }
    setState(29);
    match(DQGrammarParser::EOF);
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- DefinitionContext ------------------------------------------------------------------

DQGrammarParser::DefinitionContext::DefinitionContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

tree::TerminalNode* DQGrammarParser::DefinitionContext::IDENTIFIER() {
  return getToken(DQGrammarParser::IDENTIFIER, 0);
}

DQGrammarParser::Component_chainContext* DQGrammarParser::DefinitionContext::component_chain() {
  return getRuleContext<DQGrammarParser::Component_chainContext>(0);
}


size_t DQGrammarParser::DefinitionContext::getRuleIndex() const {
  return DQGrammarParser::RuleDefinition;
}


std::any DQGrammarParser::DefinitionContext::accept(tree::ParseTreeVisitor *visitor) {
  if (auto parserVisitor = dynamic_cast<DQGrammarVisitor*>(visitor))
    return parserVisitor->visitDefinition(this);
  else
    return visitor->visitChildren(this);
}

DQGrammarParser::DefinitionContext* DQGrammarParser::definition() {
  DefinitionContext *_localctx = _tracker.createInstance<DefinitionContext>(_ctx, getState());
  enterRule(_localctx, 2, DQGrammarParser::RuleDefinition);

#if __cplusplus > 201703L
  auto onExit = finally([=, this] {
#else
  auto onExit = finally([=] {
#endif
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(31);
    match(DQGrammarParser::IDENTIFIER);
    setState(32);
    match(DQGrammarParser::T__0);
    setState(33);
    component_chain();
    setState(34);
    match(DQGrammarParser::T__1);
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- SystemContext ------------------------------------------------------------------

DQGrammarParser::SystemContext::SystemContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

DQGrammarParser::Component_chainContext* DQGrammarParser::SystemContext::component_chain() {
  return getRuleContext<DQGrammarParser::Component_chainContext>(0);
}


size_t DQGrammarParser::SystemContext::getRuleIndex() const {
  return DQGrammarParser::RuleSystem;
}


std::any DQGrammarParser::SystemContext::accept(tree::ParseTreeVisitor *visitor) {
  if (auto parserVisitor = dynamic_cast<DQGrammarVisitor*>(visitor))
    return parserVisitor->visitSystem(this);
  else
    return visitor->visitChildren(this);
}

DQGrammarParser::SystemContext* DQGrammarParser::system() {
  SystemContext *_localctx = _tracker.createInstance<SystemContext>(_ctx, getState());
  enterRule(_localctx, 4, DQGrammarParser::RuleSystem);
  size_t _la = 0;

#if __cplusplus > 201703L
  auto onExit = finally([=, this] {
#else
  auto onExit = finally([=] {
#endif
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(36);
    match(DQGrammarParser::T__2);
    setState(37);
    match(DQGrammarParser::T__0);
    setState(38);
    component_chain();
    setState(40);
    _errHandler->sync(this);

    _la = _input->LA(1);
    if (_la == DQGrammarParser::T__1) {
      setState(39);
      match(DQGrammarParser::T__1);
    }
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- Component_chainContext ------------------------------------------------------------------

DQGrammarParser::Component_chainContext::Component_chainContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

std::vector<DQGrammarParser::ComponentContext *> DQGrammarParser::Component_chainContext::component() {
  return getRuleContexts<DQGrammarParser::ComponentContext>();
}

DQGrammarParser::ComponentContext* DQGrammarParser::Component_chainContext::component(size_t i) {
  return getRuleContext<DQGrammarParser::ComponentContext>(i);
}


size_t DQGrammarParser::Component_chainContext::getRuleIndex() const {
  return DQGrammarParser::RuleComponent_chain;
}


std::any DQGrammarParser::Component_chainContext::accept(tree::ParseTreeVisitor *visitor) {
  if (auto parserVisitor = dynamic_cast<DQGrammarVisitor*>(visitor))
    return parserVisitor->visitComponent_chain(this);
  else
    return visitor->visitChildren(this);
}

DQGrammarParser::Component_chainContext* DQGrammarParser::component_chain() {
  Component_chainContext *_localctx = _tracker.createInstance<Component_chainContext>(_ctx, getState());
  enterRule(_localctx, 6, DQGrammarParser::RuleComponent_chain);
  size_t _la = 0;

#if __cplusplus > 201703L
  auto onExit = finally([=, this] {
#else
  auto onExit = finally([=] {
#endif
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(42);
    component();
    setState(47);
    _errHandler->sync(this);
    _la = _input->LA(1);
    while (_la == DQGrammarParser::T__3) {
      setState(43);
      match(DQGrammarParser::T__3);
      setState(44);
      component();
      setState(49);
      _errHandler->sync(this);
      _la = _input->LA(1);
    }
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- ComponentContext ------------------------------------------------------------------

DQGrammarParser::ComponentContext::ComponentContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

DQGrammarParser::BehaviorComponentContext* DQGrammarParser::ComponentContext::behaviorComponent() {
  return getRuleContext<DQGrammarParser::BehaviorComponentContext>(0);
}

DQGrammarParser::ProbeComponentContext* DQGrammarParser::ComponentContext::probeComponent() {
  return getRuleContext<DQGrammarParser::ProbeComponentContext>(0);
}

DQGrammarParser::OutcomeContext* DQGrammarParser::ComponentContext::outcome() {
  return getRuleContext<DQGrammarParser::OutcomeContext>(0);
}


size_t DQGrammarParser::ComponentContext::getRuleIndex() const {
  return DQGrammarParser::RuleComponent;
}


std::any DQGrammarParser::ComponentContext::accept(tree::ParseTreeVisitor *visitor) {
  if (auto parserVisitor = dynamic_cast<DQGrammarVisitor*>(visitor))
    return parserVisitor->visitComponent(this);
  else
    return visitor->visitChildren(this);
}

DQGrammarParser::ComponentContext* DQGrammarParser::component() {
  ComponentContext *_localctx = _tracker.createInstance<ComponentContext>(_ctx, getState());
  enterRule(_localctx, 8, DQGrammarParser::RuleComponent);

#if __cplusplus > 201703L
  auto onExit = finally([=, this] {
#else
  auto onExit = finally([=] {
#endif
    exitRule();
  });
  try {
    setState(53);
    _errHandler->sync(this);
    switch (_input->LA(1)) {
      case DQGrammarParser::BEHAVIOR_TYPE: {
        enterOuterAlt(_localctx, 1);
        setState(50);
        behaviorComponent();
        break;
      }

      case DQGrammarParser::PROBE_ID: {
        enterOuterAlt(_localctx, 2);
        setState(51);
        probeComponent();
        break;
      }

      case DQGrammarParser::IDENTIFIER: {
        enterOuterAlt(_localctx, 3);
        setState(52);
        outcome();
        break;
      }

    default:
      throw NoViableAltException(this);
    }
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- BehaviorComponentContext ------------------------------------------------------------------

DQGrammarParser::BehaviorComponentContext::BehaviorComponentContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

tree::TerminalNode* DQGrammarParser::BehaviorComponentContext::BEHAVIOR_TYPE() {
  return getToken(DQGrammarParser::BEHAVIOR_TYPE, 0);
}

tree::TerminalNode* DQGrammarParser::BehaviorComponentContext::IDENTIFIER() {
  return getToken(DQGrammarParser::IDENTIFIER, 0);
}

DQGrammarParser::Component_listContext* DQGrammarParser::BehaviorComponentContext::component_list() {
  return getRuleContext<DQGrammarParser::Component_listContext>(0);
}

DQGrammarParser::Probability_listContext* DQGrammarParser::BehaviorComponentContext::probability_list() {
  return getRuleContext<DQGrammarParser::Probability_listContext>(0);
}


size_t DQGrammarParser::BehaviorComponentContext::getRuleIndex() const {
  return DQGrammarParser::RuleBehaviorComponent;
}


std::any DQGrammarParser::BehaviorComponentContext::accept(tree::ParseTreeVisitor *visitor) {
  if (auto parserVisitor = dynamic_cast<DQGrammarVisitor*>(visitor))
    return parserVisitor->visitBehaviorComponent(this);
  else
    return visitor->visitChildren(this);
}

DQGrammarParser::BehaviorComponentContext* DQGrammarParser::behaviorComponent() {
  BehaviorComponentContext *_localctx = _tracker.createInstance<BehaviorComponentContext>(_ctx, getState());
  enterRule(_localctx, 10, DQGrammarParser::RuleBehaviorComponent);
  size_t _la = 0;

#if __cplusplus > 201703L
  auto onExit = finally([=, this] {
#else
  auto onExit = finally([=] {
#endif
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(55);
    match(DQGrammarParser::BEHAVIOR_TYPE);
    setState(56);
    match(DQGrammarParser::T__4);
    setState(57);
    match(DQGrammarParser::IDENTIFIER);
    setState(62);
    _errHandler->sync(this);

    _la = _input->LA(1);
    if (_la == DQGrammarParser::T__5) {
      setState(58);
      match(DQGrammarParser::T__5);
      setState(59);
      probability_list();
      setState(60);
      match(DQGrammarParser::T__6);
    }
    setState(64);
    match(DQGrammarParser::T__7);
    setState(65);
    component_list();
    setState(66);
    match(DQGrammarParser::T__8);
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- ProbeComponentContext ------------------------------------------------------------------

DQGrammarParser::ProbeComponentContext::ProbeComponentContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

tree::TerminalNode* DQGrammarParser::ProbeComponentContext::PROBE_ID() {
  return getToken(DQGrammarParser::PROBE_ID, 0);
}

tree::TerminalNode* DQGrammarParser::ProbeComponentContext::IDENTIFIER() {
  return getToken(DQGrammarParser::IDENTIFIER, 0);
}


size_t DQGrammarParser::ProbeComponentContext::getRuleIndex() const {
  return DQGrammarParser::RuleProbeComponent;
}


std::any DQGrammarParser::ProbeComponentContext::accept(tree::ParseTreeVisitor *visitor) {
  if (auto parserVisitor = dynamic_cast<DQGrammarVisitor*>(visitor))
    return parserVisitor->visitProbeComponent(this);
  else
    return visitor->visitChildren(this);
}

DQGrammarParser::ProbeComponentContext* DQGrammarParser::probeComponent() {
  ProbeComponentContext *_localctx = _tracker.createInstance<ProbeComponentContext>(_ctx, getState());
  enterRule(_localctx, 12, DQGrammarParser::RuleProbeComponent);

#if __cplusplus > 201703L
  auto onExit = finally([=, this] {
#else
  auto onExit = finally([=] {
#endif
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(68);
    match(DQGrammarParser::PROBE_ID);
    setState(69);
    match(DQGrammarParser::T__4);
    setState(70);
    match(DQGrammarParser::IDENTIFIER);
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- Probability_listContext ------------------------------------------------------------------

DQGrammarParser::Probability_listContext::Probability_listContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

std::vector<tree::TerminalNode *> DQGrammarParser::Probability_listContext::NUMBER() {
  return getTokens(DQGrammarParser::NUMBER);
}

tree::TerminalNode* DQGrammarParser::Probability_listContext::NUMBER(size_t i) {
  return getToken(DQGrammarParser::NUMBER, i);
}


size_t DQGrammarParser::Probability_listContext::getRuleIndex() const {
  return DQGrammarParser::RuleProbability_list;
}


std::any DQGrammarParser::Probability_listContext::accept(tree::ParseTreeVisitor *visitor) {
  if (auto parserVisitor = dynamic_cast<DQGrammarVisitor*>(visitor))
    return parserVisitor->visitProbability_list(this);
  else
    return visitor->visitChildren(this);
}

DQGrammarParser::Probability_listContext* DQGrammarParser::probability_list() {
  Probability_listContext *_localctx = _tracker.createInstance<Probability_listContext>(_ctx, getState());
  enterRule(_localctx, 14, DQGrammarParser::RuleProbability_list);
  size_t _la = 0;

#if __cplusplus > 201703L
  auto onExit = finally([=, this] {
#else
  auto onExit = finally([=] {
#endif
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(72);
    match(DQGrammarParser::NUMBER);
    setState(75); 
    _errHandler->sync(this);
    _la = _input->LA(1);
    do {
      setState(73);
      match(DQGrammarParser::T__9);
      setState(74);
      match(DQGrammarParser::NUMBER);
      setState(77); 
      _errHandler->sync(this);
      _la = _input->LA(1);
    } while (_la == DQGrammarParser::T__9);
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- Component_listContext ------------------------------------------------------------------

DQGrammarParser::Component_listContext::Component_listContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

std::vector<DQGrammarParser::Component_chainContext *> DQGrammarParser::Component_listContext::component_chain() {
  return getRuleContexts<DQGrammarParser::Component_chainContext>();
}

DQGrammarParser::Component_chainContext* DQGrammarParser::Component_listContext::component_chain(size_t i) {
  return getRuleContext<DQGrammarParser::Component_chainContext>(i);
}


size_t DQGrammarParser::Component_listContext::getRuleIndex() const {
  return DQGrammarParser::RuleComponent_list;
}


std::any DQGrammarParser::Component_listContext::accept(tree::ParseTreeVisitor *visitor) {
  if (auto parserVisitor = dynamic_cast<DQGrammarVisitor*>(visitor))
    return parserVisitor->visitComponent_list(this);
  else
    return visitor->visitChildren(this);
}

DQGrammarParser::Component_listContext* DQGrammarParser::component_list() {
  Component_listContext *_localctx = _tracker.createInstance<Component_listContext>(_ctx, getState());
  enterRule(_localctx, 16, DQGrammarParser::RuleComponent_list);
  size_t _la = 0;

#if __cplusplus > 201703L
  auto onExit = finally([=, this] {
#else
  auto onExit = finally([=] {
#endif
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(79);
    component_chain();
    setState(82); 
    _errHandler->sync(this);
    _la = _input->LA(1);
    do {
      setState(80);
      match(DQGrammarParser::T__9);
      setState(81);
      component_chain();
      setState(84); 
      _errHandler->sync(this);
      _la = _input->LA(1);
    } while (_la == DQGrammarParser::T__9);
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

//----------------- OutcomeContext ------------------------------------------------------------------

DQGrammarParser::OutcomeContext::OutcomeContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

tree::TerminalNode* DQGrammarParser::OutcomeContext::IDENTIFIER() {
  return getToken(DQGrammarParser::IDENTIFIER, 0);
}


size_t DQGrammarParser::OutcomeContext::getRuleIndex() const {
  return DQGrammarParser::RuleOutcome;
}


std::any DQGrammarParser::OutcomeContext::accept(tree::ParseTreeVisitor *visitor) {
  if (auto parserVisitor = dynamic_cast<DQGrammarVisitor*>(visitor))
    return parserVisitor->visitOutcome(this);
  else
    return visitor->visitChildren(this);
}

DQGrammarParser::OutcomeContext* DQGrammarParser::outcome() {
  OutcomeContext *_localctx = _tracker.createInstance<OutcomeContext>(_ctx, getState());
  enterRule(_localctx, 18, DQGrammarParser::RuleOutcome);

#if __cplusplus > 201703L
  auto onExit = finally([=, this] {
#else
  auto onExit = finally([=] {
#endif
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(86);
    match(DQGrammarParser::IDENTIFIER);
   
  }
  catch (RecognitionException &e) {
    _errHandler->reportError(this, e);
    _localctx->exception = std::current_exception();
    _errHandler->recover(this, _localctx->exception);
  }

  return _localctx;
}

void DQGrammarParser::initialize() {
#if ANTLR4_USE_THREAD_LOCAL_CACHE
  dqgrammarParserInitialize();
#else
  ::antlr4::internal::call_once(dqgrammarParserOnceFlag, dqgrammarParserInitialize);
#endif
}

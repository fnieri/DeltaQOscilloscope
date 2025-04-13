
// Generated from DQGrammar.g4 by ANTLR 4.13.2


#include "DQGrammarVisitor.h"

#include "DQGrammarParser.h"


using namespace antlrcpp;

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
      "start", "definition", "system", "component", "probability_list", 
      "component_list", "outcome"
    },
    std::vector<std::string>{
      "", "'='", "';'", "'system'", "'->'", "':'", "'['", "']'", "'('", 
      "')'", "'s:'", "','"
    },
    std::vector<std::string>{
      "", "", "", "", "", "", "", "", "", "", "", "", "WS", "IDENTIFIER", 
      "NUMBER", "BEHAVIOR_TYPE"
    }
  );
  static const int32_t serializedATNSegment[] = {
  	4,1,15,92,2,0,7,0,2,1,7,1,2,2,7,2,2,3,7,3,2,4,7,4,2,5,7,5,2,6,7,6,1,0,
  	5,0,16,8,0,10,0,12,0,19,9,0,1,0,3,0,22,8,0,1,0,1,0,1,1,1,1,1,1,4,1,29,
  	8,1,11,1,12,1,30,1,1,1,1,1,2,1,2,1,2,4,2,38,8,2,11,2,12,2,39,1,2,3,2,
  	43,8,2,1,3,1,3,1,3,3,3,48,8,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,3,3,57,8,3,
  	1,3,1,3,1,3,1,3,1,3,3,3,64,8,3,1,3,1,3,1,3,1,3,3,3,70,8,3,3,3,72,8,3,
  	1,4,1,4,1,4,5,4,77,8,4,10,4,12,4,80,9,4,1,5,1,5,1,5,5,5,85,8,5,10,5,12,
  	5,88,9,5,1,6,1,6,1,6,0,0,7,0,2,4,6,8,10,12,0,0,97,0,17,1,0,0,0,2,25,1,
  	0,0,0,4,34,1,0,0,0,6,71,1,0,0,0,8,73,1,0,0,0,10,81,1,0,0,0,12,89,1,0,
  	0,0,14,16,3,2,1,0,15,14,1,0,0,0,16,19,1,0,0,0,17,15,1,0,0,0,17,18,1,0,
  	0,0,18,21,1,0,0,0,19,17,1,0,0,0,20,22,3,4,2,0,21,20,1,0,0,0,21,22,1,0,
  	0,0,22,23,1,0,0,0,23,24,5,0,0,1,24,1,1,0,0,0,25,26,5,13,0,0,26,28,5,1,
  	0,0,27,29,3,6,3,0,28,27,1,0,0,0,29,30,1,0,0,0,30,28,1,0,0,0,30,31,1,0,
  	0,0,31,32,1,0,0,0,32,33,5,2,0,0,33,3,1,0,0,0,34,35,5,3,0,0,35,37,5,1,
  	0,0,36,38,3,6,3,0,37,36,1,0,0,0,38,39,1,0,0,0,39,37,1,0,0,0,39,40,1,0,
  	0,0,40,42,1,0,0,0,41,43,5,2,0,0,42,41,1,0,0,0,42,43,1,0,0,0,43,5,1,0,
  	0,0,44,47,3,12,6,0,45,46,5,4,0,0,46,48,3,6,3,0,47,45,1,0,0,0,47,48,1,
  	0,0,0,48,72,1,0,0,0,49,50,5,15,0,0,50,51,5,5,0,0,51,56,5,13,0,0,52,53,
  	5,6,0,0,53,54,3,8,4,0,54,55,5,7,0,0,55,57,1,0,0,0,56,52,1,0,0,0,56,57,
  	1,0,0,0,57,58,1,0,0,0,58,59,5,8,0,0,59,60,3,10,5,0,60,63,5,9,0,0,61,62,
  	5,4,0,0,62,64,3,6,3,0,63,61,1,0,0,0,63,64,1,0,0,0,64,72,1,0,0,0,65,66,
  	5,10,0,0,66,69,5,13,0,0,67,68,5,4,0,0,68,70,3,6,3,0,69,67,1,0,0,0,69,
  	70,1,0,0,0,70,72,1,0,0,0,71,44,1,0,0,0,71,49,1,0,0,0,71,65,1,0,0,0,72,
  	7,1,0,0,0,73,78,5,14,0,0,74,75,5,11,0,0,75,77,5,14,0,0,76,74,1,0,0,0,
  	77,80,1,0,0,0,78,76,1,0,0,0,78,79,1,0,0,0,79,9,1,0,0,0,80,78,1,0,0,0,
  	81,86,3,6,3,0,82,83,5,11,0,0,83,85,3,6,3,0,84,82,1,0,0,0,85,88,1,0,0,
  	0,86,84,1,0,0,0,86,87,1,0,0,0,87,11,1,0,0,0,88,86,1,0,0,0,89,90,5,13,
  	0,0,90,13,1,0,0,0,12,17,21,30,39,42,47,56,63,69,71,78,86
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
    setState(17);
    _errHandler->sync(this);
    _la = _input->LA(1);
    while (_la == DQGrammarParser::IDENTIFIER) {
      setState(14);
      definition();
      setState(19);
      _errHandler->sync(this);
      _la = _input->LA(1);
    }
    setState(21);
    _errHandler->sync(this);

    _la = _input->LA(1);
    if (_la == DQGrammarParser::T__2) {
      setState(20);
      system();
    }
    setState(23);
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

std::vector<DQGrammarParser::ComponentContext *> DQGrammarParser::DefinitionContext::component() {
  return getRuleContexts<DQGrammarParser::ComponentContext>();
}

DQGrammarParser::ComponentContext* DQGrammarParser::DefinitionContext::component(size_t i) {
  return getRuleContext<DQGrammarParser::ComponentContext>(i);
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
    setState(25);
    match(DQGrammarParser::IDENTIFIER);
    setState(26);
    match(DQGrammarParser::T__0);
    setState(28); 
    _errHandler->sync(this);
    _la = _input->LA(1);
    do {
      setState(27);
      component();
      setState(30); 
      _errHandler->sync(this);
      _la = _input->LA(1);
    } while ((((_la & ~ 0x3fULL) == 0) &&
      ((1ULL << _la) & 41984) != 0));
    setState(32);
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

std::vector<DQGrammarParser::ComponentContext *> DQGrammarParser::SystemContext::component() {
  return getRuleContexts<DQGrammarParser::ComponentContext>();
}

DQGrammarParser::ComponentContext* DQGrammarParser::SystemContext::component(size_t i) {
  return getRuleContext<DQGrammarParser::ComponentContext>(i);
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
    setState(34);
    match(DQGrammarParser::T__2);
    setState(35);
    match(DQGrammarParser::T__0);
    setState(37); 
    _errHandler->sync(this);
    _la = _input->LA(1);
    do {
      setState(36);
      component();
      setState(39); 
      _errHandler->sync(this);
      _la = _input->LA(1);
    } while ((((_la & ~ 0x3fULL) == 0) &&
      ((1ULL << _la) & 41984) != 0));
    setState(42);
    _errHandler->sync(this);

    _la = _input->LA(1);
    if (_la == DQGrammarParser::T__1) {
      setState(41);
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

//----------------- ComponentContext ------------------------------------------------------------------

DQGrammarParser::ComponentContext::ComponentContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}


size_t DQGrammarParser::ComponentContext::getRuleIndex() const {
  return DQGrammarParser::RuleComponent;
}

void DQGrammarParser::ComponentContext::copyFrom(ComponentContext *ctx) {
  ParserRuleContext::copyFrom(ctx);
}

//----------------- ProbeComponentContext ------------------------------------------------------------------

tree::TerminalNode* DQGrammarParser::ProbeComponentContext::IDENTIFIER() {
  return getToken(DQGrammarParser::IDENTIFIER, 0);
}

DQGrammarParser::ComponentContext* DQGrammarParser::ProbeComponentContext::component() {
  return getRuleContext<DQGrammarParser::ComponentContext>(0);
}

DQGrammarParser::ProbeComponentContext::ProbeComponentContext(ComponentContext *ctx) { copyFrom(ctx); }


std::any DQGrammarParser::ProbeComponentContext::accept(tree::ParseTreeVisitor *visitor) {
  if (auto parserVisitor = dynamic_cast<DQGrammarVisitor*>(visitor))
    return parserVisitor->visitProbeComponent(this);
  else
    return visitor->visitChildren(this);
}
//----------------- OutcomeComponentContext ------------------------------------------------------------------

DQGrammarParser::OutcomeContext* DQGrammarParser::OutcomeComponentContext::outcome() {
  return getRuleContext<DQGrammarParser::OutcomeContext>(0);
}

DQGrammarParser::ComponentContext* DQGrammarParser::OutcomeComponentContext::component() {
  return getRuleContext<DQGrammarParser::ComponentContext>(0);
}

DQGrammarParser::OutcomeComponentContext::OutcomeComponentContext(ComponentContext *ctx) { copyFrom(ctx); }


std::any DQGrammarParser::OutcomeComponentContext::accept(tree::ParseTreeVisitor *visitor) {
  if (auto parserVisitor = dynamic_cast<DQGrammarVisitor*>(visitor))
    return parserVisitor->visitOutcomeComponent(this);
  else
    return visitor->visitChildren(this);
}
//----------------- BehaviorComponentContext ------------------------------------------------------------------

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

DQGrammarParser::ComponentContext* DQGrammarParser::BehaviorComponentContext::component() {
  return getRuleContext<DQGrammarParser::ComponentContext>(0);
}

DQGrammarParser::BehaviorComponentContext::BehaviorComponentContext(ComponentContext *ctx) { copyFrom(ctx); }


std::any DQGrammarParser::BehaviorComponentContext::accept(tree::ParseTreeVisitor *visitor) {
  if (auto parserVisitor = dynamic_cast<DQGrammarVisitor*>(visitor))
    return parserVisitor->visitBehaviorComponent(this);
  else
    return visitor->visitChildren(this);
}
DQGrammarParser::ComponentContext* DQGrammarParser::component() {
  ComponentContext *_localctx = _tracker.createInstance<ComponentContext>(_ctx, getState());
  enterRule(_localctx, 6, DQGrammarParser::RuleComponent);
  size_t _la = 0;

#if __cplusplus > 201703L
  auto onExit = finally([=, this] {
#else
  auto onExit = finally([=] {
#endif
    exitRule();
  });
  try {
    setState(71);
    _errHandler->sync(this);
    switch (_input->LA(1)) {
      case DQGrammarParser::IDENTIFIER: {
        _localctx = _tracker.createInstance<DQGrammarParser::OutcomeComponentContext>(_localctx);
        enterOuterAlt(_localctx, 1);
        setState(44);
        outcome();
        setState(47);
        _errHandler->sync(this);

        _la = _input->LA(1);
        if (_la == DQGrammarParser::T__3) {
          setState(45);
          match(DQGrammarParser::T__3);
          setState(46);
          component();
        }
        break;
      }

      case DQGrammarParser::BEHAVIOR_TYPE: {
        _localctx = _tracker.createInstance<DQGrammarParser::BehaviorComponentContext>(_localctx);
        enterOuterAlt(_localctx, 2);
        setState(49);
        match(DQGrammarParser::BEHAVIOR_TYPE);
        setState(50);
        match(DQGrammarParser::T__4);
        setState(51);
        match(DQGrammarParser::IDENTIFIER);
        setState(56);
        _errHandler->sync(this);

        _la = _input->LA(1);
        if (_la == DQGrammarParser::T__5) {
          setState(52);
          match(DQGrammarParser::T__5);
          setState(53);
          probability_list();
          setState(54);
          match(DQGrammarParser::T__6);
        }
        setState(58);
        match(DQGrammarParser::T__7);
        setState(59);
        component_list();
        setState(60);
        match(DQGrammarParser::T__8);
        setState(63);
        _errHandler->sync(this);

        _la = _input->LA(1);
        if (_la == DQGrammarParser::T__3) {
          setState(61);
          match(DQGrammarParser::T__3);
          setState(62);
          component();
        }
        break;
      }

      case DQGrammarParser::T__9: {
        _localctx = _tracker.createInstance<DQGrammarParser::ProbeComponentContext>(_localctx);
        enterOuterAlt(_localctx, 3);
        setState(65);
        match(DQGrammarParser::T__9);
        setState(66);
        match(DQGrammarParser::IDENTIFIER);
        setState(69);
        _errHandler->sync(this);

        _la = _input->LA(1);
        if (_la == DQGrammarParser::T__3) {
          setState(67);
          match(DQGrammarParser::T__3);
          setState(68);
          component();
        }
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
  enterRule(_localctx, 8, DQGrammarParser::RuleProbability_list);
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
    setState(73);
    match(DQGrammarParser::NUMBER);
    setState(78);
    _errHandler->sync(this);
    _la = _input->LA(1);
    while (_la == DQGrammarParser::T__10) {
      setState(74);
      match(DQGrammarParser::T__10);
      setState(75);
      match(DQGrammarParser::NUMBER);
      setState(80);
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

//----------------- Component_listContext ------------------------------------------------------------------

DQGrammarParser::Component_listContext::Component_listContext(ParserRuleContext *parent, size_t invokingState)
  : ParserRuleContext(parent, invokingState) {
}

std::vector<DQGrammarParser::ComponentContext *> DQGrammarParser::Component_listContext::component() {
  return getRuleContexts<DQGrammarParser::ComponentContext>();
}

DQGrammarParser::ComponentContext* DQGrammarParser::Component_listContext::component(size_t i) {
  return getRuleContext<DQGrammarParser::ComponentContext>(i);
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
  enterRule(_localctx, 10, DQGrammarParser::RuleComponent_list);
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
    setState(81);
    component();
    setState(86);
    _errHandler->sync(this);
    _la = _input->LA(1);
    while (_la == DQGrammarParser::T__10) {
      setState(82);
      match(DQGrammarParser::T__10);
      setState(83);
      component();
      setState(88);
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
  enterRule(_localctx, 12, DQGrammarParser::RuleOutcome);

#if __cplusplus > 201703L
  auto onExit = finally([=, this] {
#else
  auto onExit = finally([=] {
#endif
    exitRule();
  });
  try {
    enterOuterAlt(_localctx, 1);
    setState(89);
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

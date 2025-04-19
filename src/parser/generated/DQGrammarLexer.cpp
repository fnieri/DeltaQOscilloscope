
// Generated from DQGrammar.g4 by ANTLR 4.13.2


#include "DQGrammarLexer.h"


using namespace antlr4;



using namespace antlr4;

namespace {

struct DQGrammarLexerStaticData final {
  DQGrammarLexerStaticData(std::vector<std::string> ruleNames,
                          std::vector<std::string> channelNames,
                          std::vector<std::string> modeNames,
                          std::vector<std::string> literalNames,
                          std::vector<std::string> symbolicNames)
      : ruleNames(std::move(ruleNames)), channelNames(std::move(channelNames)),
        modeNames(std::move(modeNames)), literalNames(std::move(literalNames)),
        symbolicNames(std::move(symbolicNames)),
        vocabulary(this->literalNames, this->symbolicNames) {}

  DQGrammarLexerStaticData(const DQGrammarLexerStaticData&) = delete;
  DQGrammarLexerStaticData(DQGrammarLexerStaticData&&) = delete;
  DQGrammarLexerStaticData& operator=(const DQGrammarLexerStaticData&) = delete;
  DQGrammarLexerStaticData& operator=(DQGrammarLexerStaticData&&) = delete;

  std::vector<antlr4::dfa::DFA> decisionToDFA;
  antlr4::atn::PredictionContextCache sharedContextCache;
  const std::vector<std::string> ruleNames;
  const std::vector<std::string> channelNames;
  const std::vector<std::string> modeNames;
  const std::vector<std::string> literalNames;
  const std::vector<std::string> symbolicNames;
  const antlr4::dfa::Vocabulary vocabulary;
  antlr4::atn::SerializedATNView serializedATN;
  std::unique_ptr<antlr4::atn::ATN> atn;
};

::antlr4::internal::OnceFlag dqgrammarlexerLexerOnceFlag;
#if ANTLR4_USE_THREAD_LOCAL_CACHE
static thread_local
#endif
std::unique_ptr<DQGrammarLexerStaticData> dqgrammarlexerLexerStaticData = nullptr;

void dqgrammarlexerLexerInitialize() {
#if ANTLR4_USE_THREAD_LOCAL_CACHE
  if (dqgrammarlexerLexerStaticData != nullptr) {
    return;
  }
#else
  assert(dqgrammarlexerLexerStaticData == nullptr);
#endif
  auto staticData = std::make_unique<DQGrammarLexerStaticData>(
    std::vector<std::string>{
      "T__0", "T__1", "T__2", "T__3", "T__4", "T__5", "T__6", "T__7", "T__8", 
      "T__9", "PROBE_ID", "BEHAVIOR_TYPE", "NUMBER", "IDENTIFIER", "WS"
    },
    std::vector<std::string>{
      "DEFAULT_TOKEN_CHANNEL", "HIDDEN"
    },
    std::vector<std::string>{
      "DEFAULT_MODE"
    },
    std::vector<std::string>{
      "", "'='", "'->'", "';'", "'system'", "':'", "'['", "']'", "'('", 
      "')'", "','", "'s'"
    },
    std::vector<std::string>{
      "", "", "", "", "", "", "", "", "", "", "", "PROBE_ID", "BEHAVIOR_TYPE", 
      "NUMBER", "IDENTIFIER", "WS"
    }
  );
  static const int32_t serializedATNSegment[] = {
  	4,0,15,94,6,-1,2,0,7,0,2,1,7,1,2,2,7,2,2,3,7,3,2,4,7,4,2,5,7,5,2,6,7,
  	6,2,7,7,7,2,8,7,8,2,9,7,9,2,10,7,10,2,11,7,11,2,12,7,12,2,13,7,13,2,14,
  	7,14,1,0,1,0,1,1,1,1,1,1,1,2,1,2,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,4,1,4,
  	1,5,1,5,1,6,1,6,1,7,1,7,1,8,1,8,1,9,1,9,1,10,1,10,1,11,1,11,1,12,5,12,
  	63,8,12,10,12,12,12,66,9,12,1,12,1,12,4,12,70,8,12,11,12,12,12,71,1,12,
  	4,12,75,8,12,11,12,12,12,76,3,12,79,8,12,1,13,1,13,5,13,83,8,13,10,13,
  	12,13,86,9,13,1,14,4,14,89,8,14,11,14,12,14,90,1,14,1,14,0,0,15,1,1,3,
  	2,5,3,7,4,9,5,11,6,13,7,15,8,17,9,19,10,21,11,23,12,25,13,27,14,29,15,
  	1,0,5,3,0,97,97,102,102,112,112,1,0,48,57,3,0,65,90,95,95,97,122,4,0,
  	48,57,65,90,95,95,97,122,3,0,9,10,13,13,32,32,99,0,1,1,0,0,0,0,3,1,0,
  	0,0,0,5,1,0,0,0,0,7,1,0,0,0,0,9,1,0,0,0,0,11,1,0,0,0,0,13,1,0,0,0,0,15,
  	1,0,0,0,0,17,1,0,0,0,0,19,1,0,0,0,0,21,1,0,0,0,0,23,1,0,0,0,0,25,1,0,
  	0,0,0,27,1,0,0,0,0,29,1,0,0,0,1,31,1,0,0,0,3,33,1,0,0,0,5,36,1,0,0,0,
  	7,38,1,0,0,0,9,45,1,0,0,0,11,47,1,0,0,0,13,49,1,0,0,0,15,51,1,0,0,0,17,
  	53,1,0,0,0,19,55,1,0,0,0,21,57,1,0,0,0,23,59,1,0,0,0,25,78,1,0,0,0,27,
  	80,1,0,0,0,29,88,1,0,0,0,31,32,5,61,0,0,32,2,1,0,0,0,33,34,5,45,0,0,34,
  	35,5,62,0,0,35,4,1,0,0,0,36,37,5,59,0,0,37,6,1,0,0,0,38,39,5,115,0,0,
  	39,40,5,121,0,0,40,41,5,115,0,0,41,42,5,116,0,0,42,43,5,101,0,0,43,44,
  	5,109,0,0,44,8,1,0,0,0,45,46,5,58,0,0,46,10,1,0,0,0,47,48,5,91,0,0,48,
  	12,1,0,0,0,49,50,5,93,0,0,50,14,1,0,0,0,51,52,5,40,0,0,52,16,1,0,0,0,
  	53,54,5,41,0,0,54,18,1,0,0,0,55,56,5,44,0,0,56,20,1,0,0,0,57,58,5,115,
  	0,0,58,22,1,0,0,0,59,60,7,0,0,0,60,24,1,0,0,0,61,63,7,1,0,0,62,61,1,0,
  	0,0,63,66,1,0,0,0,64,62,1,0,0,0,64,65,1,0,0,0,65,67,1,0,0,0,66,64,1,0,
  	0,0,67,69,5,46,0,0,68,70,7,1,0,0,69,68,1,0,0,0,70,71,1,0,0,0,71,69,1,
  	0,0,0,71,72,1,0,0,0,72,79,1,0,0,0,73,75,7,1,0,0,74,73,1,0,0,0,75,76,1,
  	0,0,0,76,74,1,0,0,0,76,77,1,0,0,0,77,79,1,0,0,0,78,64,1,0,0,0,78,74,1,
  	0,0,0,79,26,1,0,0,0,80,84,7,2,0,0,81,83,7,3,0,0,82,81,1,0,0,0,83,86,1,
  	0,0,0,84,82,1,0,0,0,84,85,1,0,0,0,85,28,1,0,0,0,86,84,1,0,0,0,87,89,7,
  	4,0,0,88,87,1,0,0,0,89,90,1,0,0,0,90,88,1,0,0,0,90,91,1,0,0,0,91,92,1,
  	0,0,0,92,93,6,14,0,0,93,30,1,0,0,0,7,0,64,71,76,78,84,90,1,6,0,0
  };
  staticData->serializedATN = antlr4::atn::SerializedATNView(serializedATNSegment, sizeof(serializedATNSegment) / sizeof(serializedATNSegment[0]));

  antlr4::atn::ATNDeserializer deserializer;
  staticData->atn = deserializer.deserialize(staticData->serializedATN);

  const size_t count = staticData->atn->getNumberOfDecisions();
  staticData->decisionToDFA.reserve(count);
  for (size_t i = 0; i < count; i++) { 
    staticData->decisionToDFA.emplace_back(staticData->atn->getDecisionState(i), i);
  }
  dqgrammarlexerLexerStaticData = std::move(staticData);
}

}

DQGrammarLexer::DQGrammarLexer(CharStream *input) : Lexer(input) {
  DQGrammarLexer::initialize();
  _interpreter = new atn::LexerATNSimulator(this, *dqgrammarlexerLexerStaticData->atn, dqgrammarlexerLexerStaticData->decisionToDFA, dqgrammarlexerLexerStaticData->sharedContextCache);
}

DQGrammarLexer::~DQGrammarLexer() {
  delete _interpreter;
}

std::string DQGrammarLexer::getGrammarFileName() const {
  return "DQGrammar.g4";
}

const std::vector<std::string>& DQGrammarLexer::getRuleNames() const {
  return dqgrammarlexerLexerStaticData->ruleNames;
}

const std::vector<std::string>& DQGrammarLexer::getChannelNames() const {
  return dqgrammarlexerLexerStaticData->channelNames;
}

const std::vector<std::string>& DQGrammarLexer::getModeNames() const {
  return dqgrammarlexerLexerStaticData->modeNames;
}

const dfa::Vocabulary& DQGrammarLexer::getVocabulary() const {
  return dqgrammarlexerLexerStaticData->vocabulary;
}

antlr4::atn::SerializedATNView DQGrammarLexer::getSerializedATN() const {
  return dqgrammarlexerLexerStaticData->serializedATN;
}

const atn::ATN& DQGrammarLexer::getATN() const {
  return *dqgrammarlexerLexerStaticData->atn;
}




void DQGrammarLexer::initialize() {
#if ANTLR4_USE_THREAD_LOCAL_CACHE
  dqgrammarlexerLexerInitialize();
#else
  ::antlr4::internal::call_once(dqgrammarlexerLexerOnceFlag, dqgrammarlexerLexerInitialize);
#endif
}

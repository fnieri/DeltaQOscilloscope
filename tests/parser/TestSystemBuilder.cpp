#include "DQGrammarLexer.h"
#include "src/parser/SystemBuilder.h"
#include "gtest/gtest.h"

System parseSystemFromString(const std::string& input) {
    antlr4::ANTLRInputStream inputStream(input);
    parser::DQGrammarLexer lexer(&inputStream);
    antlr4::CommonTokenStream tokens(&lexer);
    parser::DQGrammarParser parser(&tokens);

    auto tree = parser.start();
    SystemBuilderVisitor builder;
    builder.visit(tree);
    return builder.getSystem();
}


TEST(SystemBuilderVisitorTest, ParsesSimpleProbe) {
    std::string input = "probe = sossio;";
    System system = parseSystemFromString(input);

    auto probes = system.getProbes();
    ASSERT_TRUE(probes.find("probe") != probes.end());
    EXPECT_EQ(probes["probe"]->getName(), "probe");
}

TEST(SystemBuilderVisitorTest, ParseOperators) {
    std::string input = "probe = f:ftf(o1 -> o2, o3);";
    System system = parseSystemFromString(input);

    auto operators = system.getOperators();
    ASSERT_TRUE(operators.find("ftf") != operators.end());
    EXPECT_EQ(operators["ftf"]->getName(), "ftf");
}

TEST(SystemBuilderVisitorTest, ThrowsOnDuplicateProbes) {
    std::string input = "probe1 = o1; probe1 = o4;";
    EXPECT_THROW(parseSystemFromString(input), std::invalid_argument);
}

TEST(SystemBuilderVisitorTest, ParsesProbabilisticOperator) {
    std::string input = "system = p:sys[0.3,0.7](o1,o2);";
    System system = parseSystemFromString(input);

    auto operators = system.getOperators();
    ASSERT_TRUE(operators.find("sys") != operators.end());
    EXPECT_EQ(operators["sys"]->getName(), "sys");
}

TEST(SystemBuilderVisitorTest, ParseWrongProbabilisticOperator) {
    std::string input = "system = p:sys[0.3,0.6](probe1,probe2);";
    EXPECT_THROW(parseSystemFromString(input), std::logic_error);
}

TEST(SystemBuilderVisitorTest, DetectsDirectSelfReference) {
    std::string input = "probe = s:probe;";
    EXPECT_THROW(parseSystemFromString(input), std::invalid_argument);
}

TEST(SystemBuilderVisitorTest, DetectsIndirectSelfReference) {
    std::string input = "probe1 = s:probe2; probe2 = s:probe1;";
    EXPECT_THROW(parseSystemFromString(input), std::invalid_argument);
}


#include "SystemBuilder.h"
#include "../diagram/AllToFinish.h"
#include "../diagram/FirstToFinish.h"
#include "../diagram/ProbabilisticOperator.h"
#include <memory>
#include <stdexcept>

System SystemBuilderVisitor::getSystem() const
{
    return system;
}

std::any SystemBuilderVisitor::visitStart(parser::DQGrammarParser::StartContext *context)
{
    // Visit all definitions first
    for (auto definition : context->definition()) {
        visitDefinition(definition);
    }

    // Then visit system if it exists
    if (context->system()) {
        visitSystem(context->system());
    }

    // Set the collected components in the system
    system.setOutcomes(outcomes);
    system.setOperators(operators);
    system.setProbes(probes);

    return nullptr;
}

std::any SystemBuilderVisitor::visitDefinition(parser::DQGrammarParser::DefinitionContext *context)
{
    std::string probeName = context->IDENTIFIER()->getText();

    std::vector<std::shared_ptr<DiagramComponent>> components;
    for (auto comp : context->component()) {
        auto component = std::any_cast<std::shared_ptr<DiagramComponent>>(visitComponent(comp));
        components.push_back(component);
    }

    for (size_t i = 0; i < components.size() - 1; i++) {
        components[i]->setNext(probeName, components[i + 1]);
    }

    auto probe = std::make_shared<Probe>(probeName);
    probe->setFirstComponent(components[0]);
    probes[probeName] = probe;
    return nullptr;
}

std::any SystemBuilderVisitor::visitSystem(parser::DQGrammarParser::SystemContext *context)
{
    std::vector<std::shared_ptr<DiagramComponent>> components;
    for (auto comp : context->component()) {
        auto component = std::any_cast<std::shared_ptr<DiagramComponent>>(visitComponent(comp));
        components.push_back(component);
    }

    // Process the chain of components (-> operator)
    for (size_t i = 0; i < components.size() - 1; i++) {
        components[i]->setNext("system", components[i + 1]);
    }

    if (!components.empty()) {
        system.setFirstComponent(components[0]);
    }

    return nullptr;
}

std::any SystemBuilderVisitor::visitComponent(parser::DQGrammarParser::ComponentContext *context)
{
    if (context->behaviorComponent()) {
        return visitBehaviorComponent(context->behaviorComponent());
    } else if (context->probeComponent()) {
        return visitProbeComponent(context->probeComponent());
    } else if (context->outcome()) {
        return visitOutcome(context->outcome());
    }
    return nullptr;
}

std::any SystemBuilderVisitor::visitBehaviorComponent(parser::DQGrammarParser::BehaviorComponentContext *context)
{
    std::string type = context->BEHAVIOR_TYPE()->getText();
    std::string name = context->IDENTIFIER()->getText();

    // Check if operator already exists
    if (operators.find(name) != operators.end()) {
        return std::dynamic_pointer_cast<DiagramComponent>(operators[name]);
    }

    // Create a new operator
    std::shared_ptr<Operator> systemOperator;
    if (type == "f") {
        systemOperator = std::make_shared<FirstToFinish>(name);
    } else if (type == "a") {
        systemOperator = std::make_shared<AllToFinish>(name);
    } else if (type == "p") {
        systemOperator = std::make_shared<ProbabilisticOperator>(name);

        // Add probabilities if they exist
        if (context->probability_list()) {
            auto probabilities = std::any_cast<std::vector<double>>(visitProbability_list(context->probability_list()));
            auto probOperator = std::dynamic_pointer_cast<ProbabilisticOperator>(systemOperator);
            probOperator->setProbabilities(probabilities);
        }
    } else {
        throw std::invalid_argument("Unknown operator type: " + type);
    }

    // Add children
    if (context->component_list()) {
        auto children = std::any_cast<std::vector<std::shared_ptr<DiagramComponent>>>(visitComponent_list(context->component_list()));
        for (auto child : children) {
            systemOperator->addChildren(child);
        }
    }

    operators[name] = systemOperator;
    return std::dynamic_pointer_cast<DiagramComponent>(systemOperator);
}

std::any SystemBuilderVisitor::visitProbeComponent(parser::DQGrammarParser::ProbeComponentContext *context)
{
    std::string name = context->IDENTIFIER()->getText();

    // Check if probe already exists
    if (probes.find(name) != probes.end()) {
        return std::dynamic_pointer_cast<DiagramComponent>(probes[name]);
    }

    auto probe = std::make_shared<Probe>(name);
    probes[name] = probe;
    return std::dynamic_pointer_cast<DiagramComponent>(probe);
}

std::any SystemBuilderVisitor::visitProbability_list(parser::DQGrammarParser::Probability_listContext *context)
{
    std::vector<double> probabilities;
    for (auto num : context->NUMBER()) {
        probabilities.push_back(std::stod(num->getText()));
    }
    return probabilities;
}

std::any SystemBuilderVisitor::visitComponent_list(parser::DQGrammarParser::Component_listContext *context)
{
    std::vector<std::shared_ptr<DiagramComponent>> components;
    for (auto comp : context->component()) {
        auto component = std::any_cast<std::shared_ptr<DiagramComponent>>(visitComponent(comp));
        components.push_back(component);
    }
    return components;
}

std::any SystemBuilderVisitor::visitOutcome(parser::DQGrammarParser::OutcomeContext *context)
{
    std::string name = context->IDENTIFIER()->getText();

    // Check if outcome already exists
    if (outcomes.find(name) != outcomes.end()) {
        return std::dynamic_pointer_cast<DiagramComponent>(outcomes[name]);
    }

    auto outcome = std::make_shared<Outcome>(name);
    outcomes[name] = outcome;
    return std::dynamic_pointer_cast<DiagramComponent>(outcome);
}

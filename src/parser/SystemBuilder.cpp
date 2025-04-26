
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
    for (auto definition : context->definition()) {
        visitDefinition(definition);
    }

    if (context->system()) {
        visitSystem(context->system());
    }

    system.setOutcomes(outcomes);
    system.setOperators(operators);
    system.setProbes(probes);

    for (auto [name, link] : definitionLinks) {
        std::cout << name << " [ ";
        for (auto &name2 : link) {
            std::cout << name2 << " ";
        }
        std::cout << "]\n";
    }

    for (auto [name, op] : operatorLinks) {
        std::cout << name << " [ ";
        for (auto link : op) {
            std::cout << " [";
            for (auto lill : link) {
                std::cout << lill << " ";
            }
            std::cout << "]";
        }
        std::cout << "]\n";
    }

    return nullptr;
}

std::any SystemBuilderVisitor::visitDefinition(parser::DQGrammarParser::DefinitionContext *context)
{
    std::string probeName = context->IDENTIFIER()->getText();
    if (std::find(definedProbes.begin(), definedProbes.end(), probeName) != definedProbes.end()) {
        throw std::invalid_argument("Probe has already been defined");
    }

    auto chainComponents = std::any_cast<std::vector<std::shared_ptr<DiagramComponent>>>(visitComponent_chain(context->component_chain()));

    std::vector<std::string> links;
    for (auto &comp : chainComponents) {
        links.push_back(comp->getName());
    }

    auto probe = std::make_shared<Probe>(probeName);

    if (!chainComponents.empty()) {
        probe->setFirstComponent(chainComponents[0]);
    }

    probes[probeName] = probe;
    definedProbes.push_back(probeName);

    definitionLinks[probeName] = links;

    return nullptr;
}

std::any SystemBuilderVisitor::visitSystem(parser::DQGrammarParser::SystemContext *context)
{
    auto chainComponents = std::any_cast<std::vector<std::shared_ptr<DiagramComponent>>>(visitComponent_chain(context->component_chain()));

    std::vector<std::string> links;
    for (auto &comp : chainComponents) {
        links.push_back(comp->getName());
    }

    if (!chainComponents.empty()) {
        system.setFirstComponent(chainComponents[0]);
    }

    systemLinks = links;

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

    if (operators.find(name) != operators.end()) {
        return std::dynamic_pointer_cast<DiagramComponent>(operators[name]);
    }

    std::shared_ptr<Operator> systemOperator;
    if (type == "f") {
        systemOperator = std::make_shared<FirstToFinish>(name);
        if (context->probability_list()) {
            throw std::invalid_argument("First to finish operator cannot have probabilities");
        }
    } else if (type == "a") {
        systemOperator = std::make_shared<AllToFinish>(name);
        if (context->probability_list()) {
            throw std::invalid_argument("All to finish operator cannot have probabilities");
        }
    } else if (type == "p") {
        systemOperator = std::make_shared<ProbabilisticOperator>(name);

        if (context->probability_list()) {
            auto probabilities = std::any_cast<std::vector<double>>(visitProbability_list(context->probability_list()));
            auto probOperator = std::dynamic_pointer_cast<ProbabilisticOperator>(systemOperator);
            probOperator->setProbabilities(probabilities);
        }
    } else {
        throw std::invalid_argument("Unknown operator type: " + type);
    }

    if (context->component_list()) {
        auto childrenChains = std::any_cast<std::vector<std::vector<std::shared_ptr<DiagramComponent>>>>(visitComponent_list(context->component_list()));

        std::vector<std::vector<std::string>> childrenLinks;

        for (auto &chain : childrenChains) {
            if (!chain.empty()) {
                systemOperator->addChildren(chain[0]);

                std::vector<std::string> chainNames;
                for (auto &comp : chain) {
                    chainNames.push_back(comp->getName());
                }
                childrenLinks.push_back(chainNames);
            }
        }

        operatorLinks[name] = childrenLinks;
    }

    operators[name] = systemOperator;
    return std::dynamic_pointer_cast<DiagramComponent>(systemOperator);
}

std::any SystemBuilderVisitor::visitProbeComponent(parser::DQGrammarParser::ProbeComponentContext *context)
{
    std::string name = context->IDENTIFIER()->getText();

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
    std::vector<std::vector<std::shared_ptr<DiagramComponent>>> componentsChains;

    for (auto chainCtx : context->component_chain()) {
        auto chain = std::any_cast<std::vector<std::shared_ptr<DiagramComponent>>>(visitComponent_chain(chainCtx));
        componentsChains.push_back(chain);
    }

    return componentsChains;
}

std::any SystemBuilderVisitor::visitComponent_chain(parser::DQGrammarParser::Component_chainContext *context)
{
    std::vector<std::shared_ptr<DiagramComponent>> components;

    for (auto compCtx : context->component()) {
        auto component = std::any_cast<std::shared_ptr<DiagramComponent>>(visitComponent(compCtx));
        components.push_back(component);
    }

    for (size_t i = 0; i + 1 < components.size(); ++i) {
        components[i]->setNext("chain", components[i + 1]);
    }

    return components;
}

std::any SystemBuilderVisitor::visitOutcome(parser::DQGrammarParser::OutcomeContext *context)
{
    std::string name = context->IDENTIFIER()->getText();

    if (outcomes.find(name) != outcomes.end()) {
        return std::dynamic_pointer_cast<DiagramComponent>(outcomes[name]);
    }

    auto outcome = std::make_shared<Outcome>(name);
    outcomes[name] = outcome;
    return std::dynamic_pointer_cast<DiagramComponent>(outcome);
}

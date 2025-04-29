
#include "SystemBuilder.h"
#include <memory>
#include <stdexcept>

#include <locale>
System SystemBuilderVisitor::getSystem() const
{
    return system;
}

void SystemBuilderVisitor::checkForCycles() const {
    std::set<std::string> visited;
    std::set<std::string> recursionStack;

    for (const auto& [node, _] : dependencies) {
        if (hasCycle(node, visited, recursionStack)) {
            throw std::invalid_argument("Cycle detected in system definition involving: " + node);
        }
    }
}

bool SystemBuilderVisitor::hasCycle(const std::string& node,
                                    std::set<std::string>& visited,
                                    std::set<std::string>& recursionStack) const
{
    if (recursionStack.find(node) != recursionStack.end()) {
        return true;
    }
    if (visited.find(node) != visited.end()) {
        return false;
    }

    visited.insert(node);
    recursionStack.insert(node);

    if (dependencies.find(node) != dependencies.end()) {
        for (const auto& neighbor : dependencies.at(node)) {
            if (hasCycle(neighbor, visited, recursionStack)) {
                return true;
            }
        }
    }

    recursionStack.erase(node);
    return false;
}



std::any SystemBuilderVisitor::visitStart(parser::DQGrammarParser::StartContext *context)
{
    for (const auto definition : context->definition()) {
        visitDefinition(definition);
    }

    if (context->system()) {
        visitSystem(context->system());
    }

    system.setOutcomes(outcomes);
    system.setOperators(operators);
    system.setProbes(probes);

    for (const auto& [name, link] : definitionLinks) {
        std::cout << name << " [ ";
        for (auto &name2 : link) {
            std::cout << name2 << " ";
        }
        std::cout << "]\n";
    }

    for (const auto& [name, op] : operatorLinks) {
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

    checkForCycles();
    return nullptr;
}

std::any SystemBuilderVisitor::visitDefinition(parser::DQGrammarParser::DefinitionContext *context)
{
    std::string probeName = context->IDENTIFIER()->getText();
    if (std::find(definedProbes.begin(), definedProbes.end(), probeName) != definedProbes.end()) {
        throw std::invalid_argument("Probe has already been defined");
    }
    if (allNames.find(probeName) != allNames.end()) {
        throw std::invalid_argument("Duplicate name detected: " + probeName);
    }
    allNames.insert(probeName);
    currentlyBuildingProbe = probeName;

    const auto chainComponents = std::any_cast<std::vector<std::shared_ptr<DiagramComponent>>>(visitComponent_chain(context->component_chain()));
    std::vector<std::shared_ptr<DiagramComponent>> probeCausalLinks;
    std::vector<std::string> links;
    for (auto &comp : chainComponents) {
        probeCausalLinks.push_back(comp);
        links.push_back(comp->getName());
    }

    const auto probe = std::make_shared<Probe>(probeName, probeCausalLinks);

    probes[probeName] = probe;
    definedProbes.push_back(probeName);
    definitionLinks[probeName] = links;
    currentlyBuildingProbe = "";
    return nullptr;
}


std::any SystemBuilderVisitor::visitSystem(parser::DQGrammarParser::SystemContext *context)
{
    const auto chainComponents = std::any_cast<std::vector<std::shared_ptr<DiagramComponent>>>(visitComponent_chain(context->component_chain()));

    std::vector<std::string> links;
    for (auto &comp : chainComponents) {
        links.push_back(comp->getName());
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
    const std::string typeStr = context->BEHAVIOR_TYPE()->getText();
    std::string name = context->IDENTIFIER()->getText();

    if (operators.find(name) != operators.end()) {
        return std::dynamic_pointer_cast<DiagramComponent>(operators[name]);
    }
    if (allNames.find(name) != allNames.end()) {
        throw std::invalid_argument("Duplicate name detected: " + name);
    }

    allNames.insert(name);
    OperatorType type;
    if (typeStr == "a")
        type = OperatorType::ATF;
    else if (typeStr == "f")
        type = OperatorType::FTF;
    else if (typeStr == "p")
        type = OperatorType::PRB;
    else
        throw std::invalid_argument("Unknown operator type: " + typeStr);

    const auto op = std::make_shared<Operator>(name, type);
    if (type == OperatorType::PRB && context->probability_list()) {
        const auto probabilities = std::any_cast<std::vector<double>>(visitProbability_list(context->probability_list()));
        op->setProbabilities(probabilities);
    }
    else if (type == OperatorType::PRB && !context->probability_list()) {
        throw std::invalid_argument("A probabilistic operator must have probabilities");
    }
    else if (type != OperatorType::PRB && context->probability_list()) {
        throw std::invalid_argument("A non probabilistic operator cannot have probabilities");
    }


    std::vector<std::vector<std::shared_ptr<DiagramComponent>>> operatorPtrLinks;

    if (context->component_list()) {
        auto childrenChains = std::any_cast<std::vector<std::vector<std::shared_ptr<DiagramComponent>>>>(visitComponent_list(context->component_list()));

        for (auto &chain : operatorPtrLinks) {
            if (!chain.empty()) {
                std::vector<std::shared_ptr<DiagramComponent>> currentChain;
                for (auto &comp : chain) {
                    currentChain.push_back(comp);
                }
                operatorPtrLinks.push_back(currentChain);
            }
        }
    }
    op->setCausalLinks(operatorPtrLinks);

    if (context->component_list()) {
        auto childrenChains = std::any_cast<std::vector<std::vector<std::shared_ptr<DiagramComponent>>>>(visitComponent_list(context->component_list()));

        std::vector<std::vector<std::string>> childrenLinks;

        for (auto &chain : childrenChains) {
            if (!chain.empty()) {
                std::vector<std::string> chainNames;
                for (auto &comp : chain) {
                    chainNames.push_back(comp->getName());
                }
                childrenLinks.push_back(chainNames);
            }
        }

        operatorLinks[name] = childrenLinks;
    }

    operators[name] = op;
    return std::dynamic_pointer_cast<DiagramComponent>(op);
}
std::any SystemBuilderVisitor::visitProbeComponent(parser::DQGrammarParser::ProbeComponentContext *context)
{
    std::string name = context->IDENTIFIER()->getText();

    if (!currentlyBuildingProbe.empty()) {
        dependencies[currentlyBuildingProbe].push_back(name);
    }

    if (probes.find(name) != probes.end()) {
        return std::dynamic_pointer_cast<DiagramComponent>(probes[name]);
    }

    // Create a stub probe (may be fleshed out later)
    auto probe = std::make_shared<Probe>(name);
    probes[name] = probe;
    return std::dynamic_pointer_cast<DiagramComponent>(probe);
}


std::any SystemBuilderVisitor::visitProbability_list(parser::DQGrammarParser::Probability_listContext *context)
{
    std::locale::global(std::locale("C"));

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
    return components;
}

std::any SystemBuilderVisitor::visitOutcome(parser::DQGrammarParser::OutcomeContext *context)
{
    std::string name = context->IDENTIFIER()->getText();

    if (outcomes.find(name) != outcomes.end()) {
        return std::dynamic_pointer_cast<DiagramComponent>(outcomes[name]);
    }
    if (allNames.find(name) != allNames.end()) {
        throw std::invalid_argument("Duplicate name detected: " + name);
    }
    allNames.insert(name);

    auto outcome = std::make_shared<Outcome>(name);
    outcomes[name] = outcome;
    return std::dynamic_pointer_cast<DiagramComponent>(outcome);
}

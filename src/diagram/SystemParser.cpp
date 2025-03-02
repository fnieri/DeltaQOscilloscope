
//
// Created by francy on 06/12/24.
//

#include "SystemParser.h"
#include "AllToFinish.h"
#include "DiagramComponent.h"
#include "FirstToFinish.h"
#include "Operator.h"
#include "Outcome.h"
#include "ProbabilisticOperator.h"
#include "Probe.h"
#include "System.h"
#include <fstream>
#include <iostream>
#include <memory>
#include <nlohmann/json.hpp>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <unordered_set>
using json = nlohmann::json;

using OutcomesMap = std::unordered_map<std::string, std::shared_ptr<Outcome>>;
using OperatorsMap = std::unordered_map<std::string, std::shared_ptr<Operator>>;
using ProbesMap = std::unordered_map<std::string, std::shared_ptr<Probe>>;

namespace parser
{

OutcomesMap outcomes;
OperatorsMap operators;
ProbesMap probes;

std::shared_ptr<DiagramComponent> parse(const json &componentJson, std::string currentProbe)
{
    std::string name = componentJson["name"];
    std::shared_ptr<DiagramComponent> nextComponent = nullptr;
    if (componentJson.contains("next")) {
        nextComponent = parse(componentJson["next"], currentProbe);
    }

    if (componentJson.contains("children")) {
        std::string type = componentJson["type"];

        // Check if operator already exists
        if (operators.find(name) != operators.end()) {

            auto existingOperator = std::dynamic_pointer_cast<Operator>(operators[name]);

            // Ensure children are the same
            std::unordered_set<std::string> existingChildren;
            for (auto &child : existingOperator->getChildren()) {
                existingChildren.insert(child->getName());
            }

            for (const auto &childJson : componentJson["children"]) {
                std::shared_ptr<DiagramComponent> childComponent = parse(childJson, currentProbe);
                if (existingChildren.find(childComponent->getName()) == existingChildren.end()) {
                    throw std::runtime_error("Error: Operator '" + name + "' exists with different children.");
                }
            }

            return existingOperator;
        }

        // Create a new operator
        std::shared_ptr<Operator> systemOperator;
        if (type == "f") {
            systemOperator = std::make_shared<FirstToFinish>(name);
        } else if (type == "a") {
            systemOperator = std::make_shared<AllToFinish>(name);
        } else if (type == "p") {
            systemOperator = std::make_shared<ProbabilisticOperator>(name);
        } else {
            throw std::invalid_argument("Unknown operator type: " + type);
        }

        for (const auto &childJson : componentJson["children"]) {
            systemOperator->addChildren(parse(childJson, currentProbe));
        }

        if (nextComponent) {
            systemOperator->setNext(currentProbe, nextComponent);
        }

        operators[name] = systemOperator;
        return systemOperator;
    }

    std::string type = componentJson["type"];

    // Check if Outcome already exists
    if (type == "o") {
        if (outcomes.find(name) != outcomes.end()) {
            auto existingOutcome = outcomes[name];
            if (nextComponent) {
                existingOutcome->setNext(currentProbe, nextComponent);
            }
            return existingOutcome;
        }

        auto outcome = std::make_shared<Outcome>(name);
        outcomes[name] = outcome;
        if (nextComponent) {
            outcome->setNext(currentProbe, nextComponent);
        }
        return outcome;
    }

    // Check if Probe already exists
    if (type == "s") {
        if (probes.find(name) != probes.end()) {
            auto existingProbe = probes[name];
            if (nextComponent) {
                existingProbe->setNext(currentProbe, nextComponent);
            }
            return existingProbe;
        }

        auto probe = std::make_shared<Probe>(name);
        probes[name] = probe;
        if (nextComponent) {
            probe->setNext(currentProbe, nextComponent);
        }
        return probe;
    }

    throw std::runtime_error("Unknown component type: " + type);
}

std::shared_ptr<Probe> parseProbe(const json &componentJson)
{
    std::string name = componentJson["name"];
    // Check if Probe already exists
    if (probes.find(name) != probes.end()) {
        return probes[name];
    }

    auto probe = std::make_shared<Probe>(name);
    probe->setFirstComponent(parse(componentJson["components"], name));
    probes[name] = probe;
    return probe;
}
}

namespace reconstructer
{

std::string reconstructComponent(const json &componentJson)
{
    std::string name = componentJson["name"];
    std::string type = componentJson["type"];
    std::string operatorType;
    if (type == "o")
        operatorType = "";
    else
        operatorType = type + ":";
    if (componentJson.contains("children")) {
        // Handle operators with children (e.g., f:ftf1(o5 -> p:probab(o6, o7), o8))
        std::string childrenStr;
        for (const auto &child : componentJson["children"]) {
            if (!childrenStr.empty()) {
                childrenStr += ", "; // Comma separates child elements
            }
            childrenStr += reconstructComponent(child);
        }

        return operatorType + name + "(" + childrenStr + ")";
    }

    if (componentJson.contains("next")) {
        // Handle sequential connections (e.g., o1 -> o2 -> s:o3)
        return operatorType + name + " -> " + reconstructComponent(componentJson["next"]);
    }

    return operatorType + name;
}
}
std::string reconstructFromJson(const json &systemJson)
{
    std::string output;

    // Handle Probes
    if (systemJson.contains("probes")) {
        for (const auto &probeJson : systemJson["probes"]) {
            std::string probeName = probeJson["name"];
            std::string probeComponents = reconstructer::reconstructComponent(probeJson["components"]);
            output += probeName + " = " + probeComponents + ";\n";
        }
    }

    // Handle System
    if (systemJson.contains("system")) {
        std::string systemComponents = reconstructer::reconstructComponent(systemJson["system"]["components"]);
        output += "system = " + systemComponents + ";\n";
    }

    return output;
}

System parseSystemJson(const std::string &fileName)
{
    System system;
    std::ifstream ifs(fileName);
    if (!ifs) {
        throw std::runtime_error("Error: Could not open file " + fileName);
    }

    if (ifs.peek() == std::ifstream::traits_type::eof()) {
        throw std::runtime_error("Error: The input JSON file is empty.");
    }
    std::string jsonString((std::istreambuf_iterator<char>(ifs)), std::istreambuf_iterator<char>());
    if (jsonString.empty()) {
        throw std::runtime_error("Error: JSON file is empty.");
    }

    json systemJson = json::parse(jsonString);
    if (systemJson.contains("probes")) {
        for (auto &probeJson : systemJson["probes"]) {
            auto probe = parser::parseProbe(probeJson);
        }
    }
    if (!systemJson["system"].empty()) {
        auto firstComponent = parser::parse(systemJson["system"]["components"], "system");
        system.setFirstComponent(firstComponent);
    }

    system.setOutcomes(parser::outcomes);
    system.setOperators(parser::operators);
    system.setProbes(parser::probes);
    system.toString();
    std::cout << reconstructFromJson(systemJson) << "\n";

    return system;
}

//
// Created by francy on 06/12/24.
//

#include "SystemParser.h"
#include "AllToFinish.h"
#include "DiagramComponent.h"
#include "Event.h"
#include "FirstToFinish.h"
#include "Operator.h"
#include "Outcome.h"
#include "ProbabilisticOperator.h"
#include "System.h"
#include <fstream>
#include <memory>
#include <nlohmann/json.hpp>
#include <string>
#include <unordered_map>
using json = nlohmann::json;

using OutcomesMap = std::unordered_map<std::string, std::shared_ptr<Outcome>>;
using OperatorsMap = std::unordered_map<std::string, std::shared_ptr<Operator>>;
using EventsMap = std::unordered_map<std::string, std::shared_ptr<Event>>;

namespace
{

OutcomesMap parseOutcomes(const json &systemJson, System &system)
{
    OutcomesMap outcomes;
    EventsMap events;
    for (const auto &outcomeJson : systemJson["outcomes"]) {
        std::string name = outcomeJson["name"];
        std::string startEventName = outcomeJson["start"];
        std::string endEventName = outcomeJson["end"];

        auto startEvent = std::make_shared<Event>(startEventName);
        auto endEvent = std::make_shared<Event>(endEventName);
        auto outcome = std::make_shared<Outcome>(name, startEvent, endEvent);
        events[startEvent->getName()] = startEvent;
        events[endEvent->getName()] = endEvent;

        outcomes[name] = outcome;
    }
    system.setEvents(events);
    return outcomes;
}

OperatorsMap parseOperators(const json &systemJson, System &system)
{
    OperatorsMap operators;
    for (const auto &operatorJson : systemJson["operators"]) {
        std::string name = operatorJson["name"];
        std::string type = operatorJson["type"];
        std::shared_ptr<Operator> systemOperator;
        if (type == "F") {
            systemOperator = std::make_shared<FirstToFinish>(name);
        } else if (type == "A") {
            systemOperator = std::make_shared<AllToFinish>(name);
        } else if (type == "P") {
            systemOperator = std::make_shared<ProbabilisticOperator>(name);
        } else {
            throw std::invalid_argument("Unknown operator type: " + type);
        }
        operators[name] = systemOperator;
    }
    return operators;
}

void connectComponents(const json &systemJson, OutcomesMap &outcomes, OperatorsMap &operators)
{
    // Connect 'next' for outcomes
    for (const auto &outcomeJson : systemJson["outcomes"]) {
        std::string outcomeName = outcomeJson["name"];
        if (outcomeJson.contains("next")) {
            auto outcomeIt = outcomes.find(outcomeName);
            if (outcomeIt == outcomes.end()) {
                throw std::runtime_error("Outcome not found: " + outcomeName);
            }

            auto outcome = outcomeIt->second;
            const auto &nextNames = outcomeJson["next"];
            for (const auto &nextName : nextNames) {
                if (outcomes.count(nextName)) {
                    outcome->setNext(outcomes[nextName]);
                } else if (operators.count(nextName)) {
                    outcome->setNext(operators[nextName]);
                } else {
                }
            }
        }
    }

    // Connect 'next' and 'following' for operators
    for (const auto &operatorJson : systemJson["operators"]) {
        std::string operatorName = operatorJson["name"];
        if (operatorJson.contains("next")) {
            auto operatorIt = operators.find(operatorName);
            if (operatorIt == operators.end()) {
                throw std::runtime_error("Operator not found: " + operatorName);
            }

            auto systemOperator = operatorIt->second;
            const auto &nextNames = operatorJson["next"];
            for (const auto &nextName : nextNames) {
                if (outcomes.count(nextName)) {
                    systemOperator->addNextComponent(outcomes[nextName]);
                } else if (operators.count(nextName)) {
                    systemOperator->addNextComponent(operators[nextName]);
                } else {
                }
            }
        }

        if (operatorJson.contains("following")) {
            std::string followingName = operatorJson["following"];
            auto operatorIt = operators.find(operatorName);
            if (operatorIt == operators.end()) {
                throw std::runtime_error("Operator not found: " + operatorName);
            }

            auto systemOperator = operatorIt->second;
            if (outcomes.count(followingName)) {
                systemOperator->setFollowingComponent(outcomes[followingName]);
            } else if (operators.count(followingName)) {
                systemOperator->setFollowingComponent(operators[followingName]);
            } else {
                throw std::runtime_error("Invalid 'following' target: " + followingName);
            }
        }
    }
}

void setFirstComponent(const json &systemJson, System &system, OutcomesMap outcomes, OperatorsMap operators)
{
    std::string firstComponentName = systemJson["first"];
    auto firstOutcomeIt = outcomes.find(firstComponentName);
    if (firstOutcomeIt != outcomes.end()) {
        // Found as an outcome
        system.setFirstComponent(firstOutcomeIt->second);
    } else {
        auto firstOperatorIt = operators.find(firstComponentName);
        if (firstOperatorIt != operators.end()) {
            // Found as an operator
            system.setFirstComponent(firstOperatorIt->second);
        } else {
            // Not found in either outcomes or operators
            throw std::runtime_error("First component '" + firstComponentName + "' not found in outcomes or operators.");
        }
    }
}
}
System parseSystemJson(const std::string &fileName)
{
    System system;
    std::ifstream ifs(fileName);
    json systemJson = json::parse(ifs);
    OutcomesMap outcomes = parseOutcomes(systemJson, system);
    OperatorsMap operators = parseOperators(systemJson, system);
    setFirstComponent(systemJson, system, outcomes, operators);
    system.setOutcomes(outcomes);
    system.setOperators(operators);
    return system;
}

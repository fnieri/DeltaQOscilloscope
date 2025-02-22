
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
#include "System.h"
#include <fstream>
#include <iostream>
#include <memory>
#include <nlohmann/json.hpp>
#include <string>
#include <unordered_map>
using json = nlohmann::json;

using OutcomesMap = std::unordered_map<std::string, std::shared_ptr<Outcome>>;
using OperatorsMap = std::unordered_map<std::string, std::shared_ptr<Operator>>;

namespace
{

OutcomesMap outcomes;
OperatorsMap operators;
std::shared_ptr<DiagramComponent> parse(const json &componentJson)
{
    std::string name = componentJson["name"];
    std::shared_ptr<DiagramComponent> nextComponent = nullptr;
    if (componentJson.contains("next")) {
        nextComponent = parse(componentJson["next"]);
    }
    if (componentJson.contains("children")) {

        std::shared_ptr<Operator> systemOperator;
        std::string type = componentJson["type"];
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
            systemOperator->addNextComponent(parse(childJson));
        }

        if (nextComponent) {
            systemOperator->setFollowingComponent(nextComponent);
        }
        operators[name] = systemOperator;
        return systemOperator;
    }

    auto outcome = std::make_shared<Outcome>(name);
    outcomes[name] = outcome;
    if (nextComponent) {
        outcome->setNext(nextComponent);
    }

    return outcome;
}
}

System parseSystemJson(const std::string &fileName)
{
    System system;
    std::ifstream ifs(fileName);
    json systemJson = json::parse(ifs);

    if (!systemJson["system"].empty()) {
        auto firstComponent = parse(systemJson["system"][0]);
        system.setFirstComponent(firstComponent);
    }

    if (systemJson.contains("probes")) {
        std::unordered_map<std::string, std::shared_ptr<Probe>> probes;
        for (const auto &probeName : systemJson["probes"]) {
            probes[probeName] = std::make_shared<Probe>(probeName);
        }
        system.setProbes(probes);
    }

    system.setOutcomes(outcomes);
    system.setOperators(operators);
    system.toString();
    return system;
}

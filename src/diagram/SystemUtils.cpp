#include "SystemUtils.h"
#include "Observable.h"
using namespace nlohmann;

void addParametersToJson(json &currentJson, std::unordered_map<std::string, std::shared_ptr<Observable>> components)
{
    currentJson["parameters"] = nlohmann::json::array();
    for (auto &[name, comp] : components) {
        if (comp) {
            currentJson["parameters"].push_back({
                {"name", name                },
                {"n",    comp->getDeltaTExp()},
                {"N",    comp->getNBins()    }
            });
        }
    }
}

json systemToJson(std::shared_ptr<System> system)
{
    json j;
    j["definition"] = system->getSystemDefinitionText();
    addParametersToJson(j, system->getObservables());
    std::cout << j << "\n";
    return j;
}

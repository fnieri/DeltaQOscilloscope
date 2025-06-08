#pragma once

#include "System.h"
#include <nlohmann/json.hpp>

/**
 * @brief Add parameters for all components in a system to the json
 * @note Called when saving system definition
 */
void addParametersToJson(nlohmann::json &, std::unordered_map<std::string, std::shared_ptr<Observable>>);

/**
 * @brief Translate a system to json
 * @note Called when saving a system
 */
nlohmann::json systemToJson(std::shared_ptr<System>);

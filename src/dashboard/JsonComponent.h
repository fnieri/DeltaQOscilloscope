#pragma once

#include <nlohmann/json.hpp>
#include <string>

struct JsonComponent {
    std::string name;
    std::string startEvent;
    std::string endEvent;
    std::string type;

    nlohmann::json toJson() const;
};

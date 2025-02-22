#pragma once

#include <nlohmann/json.hpp>
#include <string>

struct JsonComponent {
    std::string name;
    std::string type;

    nlohmann::json toJson() const;
};

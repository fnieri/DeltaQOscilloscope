#include "JsonComponent.h"

nlohmann::json JsonComponent::toJson() const
{
    nlohmann::json json;
    json["name"] = name;
    json["type"] = type;
    return json;
}

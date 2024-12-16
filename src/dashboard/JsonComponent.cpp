#include "JsonComponent.h"

nlohmann::json JsonComponent::toJson() const
{
    nlohmann::json json;
    json["name"] = name;
    json["start"] = startEvent;
    json["end"] = endEvent;
    json["type"] = type;
    return json;
}

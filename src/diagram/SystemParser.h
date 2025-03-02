//
// Created by francy on 06/12/24.
//

#ifndef SYSTEMPARSER_H
#define SYSTEMPARSER_H

#pragma once

#include "System.h"
#include <nlohmann/json.hpp>
#include <string>
System parseSystemJson(const std::string &fileName);
std::string reconstructFromJson(const nlohmann::json &systemJson);
#endif // SYSTEMPARSER_H

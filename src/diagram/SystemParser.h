//
// Created by francy on 06/12/24.
//

#ifndef SYSTEMPARSER_H
#define SYSTEMPARSER_H

#pragma once

#include "System.h"
#include <string>

System parseSystemJson(const std::string &fileName);
std::string componentToString(const std::shared_ptr<DiagramComponent> &component);
#endif // SYSTEMPARSER_H

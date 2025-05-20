/**
 * @author Francesco Nieri
 * @date 25/10/2024
 * Class representing an outcome O_n in a system
 */
#pragma once

#include "Observable.h"

class Outcome : public Observable
{
public:
    Outcome(const std::string &name);
    ~Outcome();
};

#include "Outcome.h"
#include "DiagramComponent.h"
#include "src/maths/ConfidenceInterval.h"
#include "src/maths/DeltaQRepr.h"
#include <iostream>

Outcome::Outcome(const std::string &name)
    : DiagramComponent(name)
    , Observable(name)
{
}

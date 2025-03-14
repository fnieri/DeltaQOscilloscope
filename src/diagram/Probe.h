#pragma once
#include <string>
#include <vector>

#include "../maths/DeltaQ.h"
#include "DiagramComponent.h"
#include "Primitive.h"

struct ProbeDeltaQ {
    DeltaQ probeDeltaQ;
    DeltaQ calculatedProbeDeltaQ;
};

class Probe : public Primitive
{
    std::shared_ptr<DiagramComponent> firstComponent;

public:
    explicit Probe(const std::string &name);

    explicit Probe(const std::string &name, std::shared_ptr<DiagramComponent> firstComponent);
    DeltaQ calculateDeltaQ(const double &binWidth, std::string currentProbe) override;

    ProbeDeltaQ getDeltaQ(double binWidth);

    void setFirstComponent(std::shared_ptr<DiagramComponent> component);

    std::string toString() const override;

    void print(int depth, std::string currentProbe) override;
};

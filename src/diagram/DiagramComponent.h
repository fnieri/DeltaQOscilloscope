/**
 * @file DiagramComponent.h
 * @author Francesco Nieri
 * @brief Abstract class representing an outcome diagram component
 * @date 25/10/2024
 */

#pragma once

#include "../maths/DeltaQ.h"
#include <memory>
#include <unordered_map>
class DiagramComponent
{

protected:
    std::string name;
    explicit DiagramComponent(const std::string &name);
    // Unordered map indicating in probe x which component is next (by name)
    // probeNextComponent["probe1"]["o2"] indicates that in probe1 said component has o2 as next component (o1 -> o2).
    std::unordered_map<std::string, std::shared_ptr<DiagramComponent>> probeNextComponent;

public:
    DiagramComponent() = default;

    virtual ~DiagramComponent() { };

    void setNext(const std::string &probeName, std::shared_ptr<DiagramComponent> next);

    [[nodiscard]] virtual DeltaQ calculateDeltaQ(const double &binWidth, std::string currentProbe, uint64_t timeLowerBound, uint64_t timeUpperBound) = 0;

    std::string getName() const &;

    std::unordered_map<std::string, std::shared_ptr<DiagramComponent>> getProbeNextComponent();
};

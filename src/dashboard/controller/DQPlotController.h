#pragma once

class DQPlotController
{

public:
    DQPlotController(std::shared_ptr<System> system, DeltaQ (*operation)(const std::vector < DeltaQ &) = nullptr);
}

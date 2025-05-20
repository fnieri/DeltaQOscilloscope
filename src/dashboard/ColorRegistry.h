#pragma once
#include <QColor>
#include <string>
#include <unordered_map>

class ColorRegistry
{
public:
    static QColor getColorFor(const std::string &name);

private:
    static QColor generateDistinctColor(int index);
    static std::unordered_map<std::string, QColor> colorMap;
};

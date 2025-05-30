#include "ColorRegistry.h"
#include <cmath>

std::unordered_map<std::string, QColor> ColorRegistry::colorMap;

QColor ColorRegistry::getColorFor(const std::string &name)
{
    if (colorMap.count(name))
        return colorMap[name];

    int index = colorMap.size();
    QColor color = generateDistinctColor(index);
    colorMap[name] = color;
    return color;
}

// Taken from
// https://martin.ankerl.com/2009/12/09/how-to-create-random-colors-programmatically/
QColor ColorRegistry::generateDistinctColor(int index)
{
    const double golden_ratio_conjugate = 137.508; // degrees
    double hue = std::fmod(index * golden_ratio_conjugate, 360.0);
    QColor color;
    color.setHsvF(hue / 360.0, 0.7, 0.95); // Saturation and Value tuned for brightness
    return color;
}

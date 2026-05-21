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

// Converts OKLCH -> OKLab -> linear sRGB -> sRGB (gamma encoded).
// OKLCH is perceptually uniform: equal numerical distance = equal perceived
// difference, unlike HSV where hue steps cluster visually near yellow/blue.
static double oklabGamma(double x)
{
    if (x <= 0.0031308)
        return 12.92 * x;
    return 1.055 * std::pow(x, 1.0 / 2.4) - 0.055;
}

static QColor oklchToQColor(double L, double C, double hueDeg)
{
    double h = hueDeg * M_PI / 180.0;
    double a = C * std::cos(h);
    double b = C * std::sin(h);

    // OKLab -> linear sRGB (Björn Ottosson's matrix)
    double l_ = L + 0.3963377774 * a + 0.2158037573 * b;
    double m_ = L - 0.1055613458 * a - 0.0638541728 * b;
    double s_ = L - 0.0894841775 * a - 1.2914855480 * b;

    double l = l_ * l_ * l_;
    double m = m_ * m_ * m_;
    double s = s_ * s_ * s_;

    double r =  4.0767416621 * l - 3.3077115913 * m + 0.2309699292 * s;
    double g = -1.2684380046 * l + 2.6097574011 * m - 0.3413193965 * s;
    double bl = -0.0041960863 * l - 0.7034186147 * m + 1.7076147010 * s;

    // Clamp to [0,1] — some OKLCH values fall outside sRGB gamut
    r  = std::max(0.0, std::min(1.0, oklabGamma(r)));
    g  = std::max(0.0, std::min(1.0, oklabGamma(g)));
    bl = std::max(0.0, std::min(1.0, oklabGamma(bl)));

    return QColor::fromRgbF(r, g, bl);
}

QColor ColorRegistry::generateDistinctColor(int index)
{
    // Golden ratio hue rotation gives maximally spread hues
    const double golden_ratio_conjugate = 137.508;
    double hue = std::fmod(index * golden_ratio_conjugate, 360.0);

    // Alternate lightness and chroma so even perceptually-close hues
    // (e.g. consecutive blues) differ on a second visual axis
    static const double lightnesses[] = {0.70, 0.60, 0.75};
    static const double chromas[]     = {0.15, 0.18, 0.12};
    double L = lightnesses[index % 3];
    double C = chromas[index % 3];

    return oklchToQColor(L, C, hue);
}

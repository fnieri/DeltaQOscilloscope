

#ifndef QTA_H
#define QTA_H
#include <iostream>
#include <limits>
#define QTA_EPSILON std::numeric_limits<double>::epsilon()

struct QTA {
    double perc_25 {0};
    double perc_50 {0};
    double perc_75 {0};
    double cdfMax {1};

    static QTA create(double p25, double p50, double p75, double cdf) {
        if (!(p25 <= p50 && p50 <= p75)) {
            throw std::invalid_argument("Percentiles must be ordered: perc_25 < perc_50 < perc_75.");
        }
        if (cdf <= 0.0 || cdf - 1.0 > QTA_EPSILON) {
            throw std::invalid_argument("cdfMax must be between 0 and 1 (exclusive lower bound, inclusive upper).");
        }
        return QTA{p25, p50, p75, cdf};
    }
};

#endif //QTA_H

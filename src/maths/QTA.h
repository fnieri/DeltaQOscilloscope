#ifndef QTA_H
#define QTA_H

#include <iostream>
#include <limits>
#define QTA_EPSILON std::numeric_limits<double>::epsilon()
/**
 * @struct QTA Quantitative Timeliness Agreement for an observable
 */
struct QTA {
    double perc_25 {0}; ///< 25 percentile value
    double perc_50 {0}; ///< 50 percentile value
    double perc_75 {0}; ///< 75 percentile value
    double cdfMax {0}; ///< Least failure rate
    bool defined = false; ///< If defined or not

    static QTA create(double p25, double p50, double p75, double cdf)
    {
        return QTA::create(p25, p50, p75, cdf, true);
    }

    static QTA create(double p25, double p50, double p75, double cdf, bool isDefined)
    {
        if (!(p25 <= p50 && p50 <= p75)) {
            throw std::invalid_argument("Percentiles must be ordered: perc_25 < perc_50 < perc_75.");
        }
        if (cdf < 0.0 || cdf - 1.0 > QTA_EPSILON) {
            throw std::invalid_argument("cdfMax must be between 0 and 1 (exclusive lower bound, inclusive upper).");
        }

        return QTA {p25, p50, p75, cdf, isDefined};
    }
};

#endif // QTA_H

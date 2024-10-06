import matplotlib.pyplot as plt

import numpy as np

def convolve(*args):
    if len(args) == 2:
        first = args[0]
        second = args[1]

        pdf1, bins1 = first.get_pdf_and_bin_edges()
        pdf2, bins2 = second.get_pdf_and_bin_edges()



    else:
        pdf1, bins1 = args[0], args[1]
        pdf2, bins2 = args[2], args[3]

    bin_width1 = bins1[1] - bins1[0]
    bin_width2 = bins2[1] - bins2[0]

    # Here, we normalize pdf before convoluting
    pdf1_normalized = pdf1 / np.sum(pdf1 * bin_width1)
    pdf2_normalized = pdf2 / np.sum(pdf2 * bin_width2)

    convolved_pdf = np.convolve(pdf1_normalized, pdf2_normalized, mode='full')
    convolved_pdf *= bin_width1 * bin_width2  # Scale according to original scale
    convolved_bins = compute_convolved_bins(bins1, bins2)
    return convolved_pdf, convolved_bins

def compute_convolved_bins(bins1, bins2) -> np.ndarray:
    """
    Compute new x-axis for convolved distribution
    :param bins1:
    :param bins2:
    :return:
    """
    min_bin = bins1[0] + bins2[0]
    max_bin = bins1[-1] + bins2[-1]

    convolved_bins = np.linspace(min_bin, max_bin, len(bins1) + len(bins2) - 1)
    return convolved_bins

def compute_cdf_from_pdf(pdf: np.ndarray) -> np.ndarray:
    #Normalize cumulative frequency to cumulative distribution (https://stackoverflow.com/questions/73176551/how-to-calculate-cdfv-without-loops-using-numpy-for-each-pixel-v-cdfv-eq)
    cdf = np.cumsum(pdf)
    cdf /= cdf[-1]
    return cdf

def normalize_pdf(pdf, bin_width):
    return pdf / np.sum(pdf * bin_width)

def multiply_cdf(cdf: np.ndarray, constant: int) -> np.ndarray:
    return constant * cdf

def add_cdfs(cdfs):
    """
    Add multiple cdfs by interpolating
    :param cdfs:
    :return:
    """

    x_common = np.union1d(cdfs[0][0], cdfs[1][0])
    for i in range(2, len(cdfs)):
        x_common = np.union1d(x_common, cdfs[i][0])

    y_interp_list = []
    for x, y in cdfs:
        y_interp = np.interp(x_common, x, y)  # Interpolate each CDF
        y_interp_list.append(y_interp)

    return x_common, np.sum(y_interp_list, axis=0)

import numpy as np
import matplotlib.pyplot as plt

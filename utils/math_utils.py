import matplotlib.pyplot as plt
import scipy.signal
from scipy.signal import convolve, fftconvolve

import numpy as np

def convolve_pdf(*args):
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

    normalized_pdf1 = normalize_pdf(pdf1, bin_width1)
    normalized_pdf2 = normalize_pdf(pdf2, bin_width2)

    convolved_pdf = np.convolve(normalized_pdf1, normalized_pdf2, mode='full')

    convolved_pdf *= bin_width1 * bin_width2
    convolved_bins = np.linspace(bins1[0] + bins2[0], bins1[-1] + bins2[-1], len(convolved_pdf))
    convolved_pdf = normalize_pdf(convolved_pdf, convolved_bins[1] - convolved_bins[0])
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
    total_area = np.sum(pdf * bin_width)
    if total_area == 0:
        raise ValueError("The total area under the PDF is zero, which is invalid for normalization.")
    return pdf / total_area

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
        y_interp = np.interp(x_common, x, y)
        y_interp_list.append(y_interp)

    return x_common, np.sum(y_interp_list, axis=0),


def compute_pdf_from_cdf(cdf, x_values):
    #From ChatGPT
    delta_cdf = np.diff(cdf)
    delta_x = np.diff(x_values)
    pdf = delta_cdf / delta_x
    return pdf, x_values[:-1]

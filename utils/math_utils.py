import matplotlib.pyplot as plt
import scipy.signal
from scipy.signal import convolve, fftconvolve

import numpy as np


def convolve_pdf(pdf1, bins1, pdf2, bins2):
    # Calculate bin widths
    bin_width1 = bins1[1] - bins1[0]
    bin_width2 = bins2[1] - bins2[0]

    #normalized_pdf1 = normalize_pdf(pdf1, bin_width1)
    #normalized_pdf2 = normalize_pdf(pdf2, bin_width2)

    convolved_pdf = np.convolve(pdf1, pdf2, mode='full')

    convolved_bin_edges = np.linspace(bins1[0] + bins2[0],
                                      bins1[-1] + bins2[-1],
                                      len(convolved_pdf))

    convolved_pdf_normalized = normalize_pdf(convolved_pdf, convolved_bin_edges[1] - convolved_bin_edges[0])

    return convolved_pdf_normalized, convolved_bin_edges

def compute_cdf_from_pdf(pdf: np.ndarray, bins):
    bin_widths = np.diff(bins)
    if (len(bin_widths) == len(pdf) - 1):
        bin_widths = np.append(bin_widths, bin_widths[-1])  # Append last width for broadcasting
    cdf = np.cumsum(pdf * bin_widths)
    cdf /= cdf[-1]
    return cdf, bins

def normalize_pdf(pdf, bin_width):
    return pdf / np.sum(pdf * bin_width)

def multiply_cdf(cdf: np.ndarray, constant: float) -> np.ndarray:
    return constant * cdf

def add_cdfs(cdfs):
    """
    Add multiple cdfs by interpolating
    :param cdfs:
    :return:
    """
    x_common = cdfs[0][0]
    for x, _ in cdfs[1:]:
        x_common = np.union1d(x_common, x)

    y_interp_list = []
    for i, (x, y) in enumerate(cdfs):
        y_interp = np.interp(x_common, x, y)
        y_interp_list.append(y_interp)

    return x_common, np.sum(y_interp_list, axis=0)


def compute_pdf_from_cdf(cdf, x_values):
    #From ChatGPT
    delta_cdf = np.diff(cdf)
    delta_x = np.diff(x_values)
    pdf = delta_cdf / delta_x
    return pdf, x_values[:-1]

import matplotlib.pyplot as plt

from diagram.ObservationPoint import ObservationPoint
import numpy as np

def convolve(*args) -> np.ndarray:
    if len(args) == 2:
        first = args[0]
        second = args[1]

        pdf1, bins1 = first.get_pdf_and_bin_edges()
        pdf2, bins2 = second.get_pdf_and_bin_edges()

        # Here, we normalize pdf before convoluting

        bin_width1 = bins1[1] - bins1[0]
        bin_width2 = bins2[1] - bins2[0]

        pdf1_normalized = pdf1 / np.sum(pdf1 * bin_width1)
        pdf2_normalized = pdf2 / np.sum(pdf2 * bin_width2)

        convolved_pdf = np.convolve(pdf1_normalized, pdf2_normalized, mode='full')
        convolved_pdf *= bin_width1 * bin_width2  # Scale according to original scale

        return convolved_pdf
    else:
        pass

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


def add_n_pdfs_and_convert_to_cdf(pdfs: list, bin_width):
    pdf_sum = np.sum(pdfs, axis=0)
    pdf_normalized = normalize_pdf(pdf_sum, bin_width)
    cdf = compute_cdf_from_pdf(pdf_normalized)
    return pdf_normalized, cdf


def multiply_pdf_and_convert_to_cdf(pdf, constant, bin_width):
    pdf_scaled = pdf * constant
    pdf_normalized = normalize_pdf(pdf_scaled, bin_width)
    cdf = compute_cdf_from_pdf(pdf_normalized)
    return pdf_normalized, cdf


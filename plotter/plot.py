import time
import random
import threading
import numpy as np
import pyqtgraph as pg

from ObservationPoint import ObservationPoint
from PyQt5 import QtWidgets
from PyQt5.QtCore import QTimer
from PyQt5.QtWidgets import QLabel, QVBoxLayout, QHBoxLayout, QWidget


class RealTimePlotter(QtWidgets.QMainWindow):
    def __init__(self):
        super().__init__()

        self.plot_widget1 = pg.PlotWidget()
        self.plot_widget2 = pg.PlotWidget()
        self.info_label = QLabel()

        self.switch_button = QtWidgets.QPushButton("Switch to CDF")

        self.set_up_window()
        self.set_up_plot_widgets()
        self.set_up_info_display()
        self.set_up_buttons()

        self.obs_point1 = ObservationPoint()
        self.obs_point2 = ObservationPoint()

        self.curve1 = self.plot_widget1.plot([], [], pen=pg.mkPen('b', width=2))
        self.curve2 = self.plot_widget2.plot([], [], pen=pg.mkPen('r', width=2))

        self.mode = 0

        self.timer = QTimer()
        self.set_up_timer()

    def switch_plot_type(self):
        self.mode = (self.mode + 1) % 3

        if self.mode == 0:
            self.switch_button.setText("Switch to CDF")
            self.plot_widget1.setLabel('left', 'PDF')
            self.plot_widget2.setLabel('left', 'PDF')
            self.plot_widget2.show()

        elif self.mode == 1:
            self.switch_button.setText("Switch to Convolution CDF")
            self.plot_widget1.setLabel('left', 'CDF')
            self.plot_widget2.setLabel('left', 'CDF')
            self.plot_widget2.show()

        else:
            self.switch_button.setText("Switch to PDF")
            self.plot_widget1.setLabel('left', 'Convolution CDF')
            self.plot_widget2.hide()

    def set_up_window(self):
        self.setWindowTitle("PDF, CDF, and Convolution of Delays - Side by Side")
        self.setGeometry(100, 100, 1000, 600)

    def set_up_plot_widgets(self):
        self.plot_widget1.setLabel('bottom', 'Delay (ms)')
        self.plot_widget2.setLabel('bottom', 'Delay (ms)')

        self.plot_widget1.showGrid(x=True, y=True)
        self.plot_widget2.showGrid(x=True, y=True)

    def set_up_timer(self):
        self.timer.timeout.connect(self.update_plot)
        self.timer.start(500)

    def set_up_buttons(self):
        self.switch_button.clicked.connect(self.switch_plot_type)

    def add_delay(self, delay, obs_point):
        obs_point.add_value(delay)

    def set_up_info_display(self):
        layout = QVBoxLayout()
        container = QWidget()
        container.setLayout(layout)

        self.plot_layout = QHBoxLayout()
        self.plot_layout.addWidget(self.plot_widget1)
        self.plot_layout.addWidget(self.plot_widget2)

        layout.addLayout(self.plot_layout)
        layout.addWidget(self.info_label)
        layout.addWidget(self.switch_button)

        self.setCentralWidget(container)

    def convolve(self, first: ObservationPoint, second: ObservationPoint) -> np.ndarray:
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

    def compute_convolved_bins(self, bins1, bins2) -> np.ndarray:
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

    def compute_cdf_from_pdf(self, pdf: np.ndarray) -> np.ndarray:
        #Normalize cumulative frequency to cumulative distribution (https://stackoverflow.com/questions/73176551/how-to-calculate-cdfv-without-loops-using-numpy-for-each-pixel-v-cdfv-eq)
        cdf = np.cumsum(pdf)
        cdf /= cdf[-1]
        return cdf

    def update_plot(self):
        if self.mode == 0:
            pdf1, bin_edges1 = self.obs_point1.get_pdf_and_bin_edges()
            pdf2, bin_edges2 = self.obs_point2.get_pdf_and_bin_edges()

            self.curve1.setData(bin_edges1, pdf1)
            self.curve2.setData(bin_edges2, pdf2)

            self.info_label.setText("PDF - Observation 1 (Blue), Observation 2 (Red)")

        elif self.mode == 1:
            cdf1, sorted_data1 = self.obs_point1.get_cdf_and_sorted_values()
            cdf2, sorted_data2 = self.obs_point2.get_cdf_and_sorted_values()

            self.curve1.setData(sorted_data1, cdf1)
            self.curve2.setData(sorted_data2, cdf2)

            self.info_label.setText(
                f"Observation 1 - Min: {self.obs_point1.get_min_value():.2f} ms, 25 percentile: {self.obs_point1.get_25_percentile():.2f}, 50 percentile: {self.obs_point1.get_50_percentile():.2f}, 75 percentile: {self.obs_point1.get_75_percentile():.2f} Max: {self.obs_point1.get_max_value():.2f} ms | "
                f"Observation 2 - Min: {self.obs_point2.get_min_value():.2f} ms, 25 percentile: {self.obs_point2.get_25_percentile():.2f}, 50 percentile: {self.obs_point2.get_50_percentile():.2f}, 75 percentile: {self.obs_point2.get_75_percentile():.2f} Max: {self.obs_point2.get_max_value():.2f} ms | "
            )

        elif self.mode == 2:

            convolved_pdf = self.convolve(self.obs_point1, self.obs_point2)
            convolved_cdf = self.compute_cdf_from_pdf(convolved_pdf)
            convolved_bins = self.compute_convolved_bins(self.obs_point1.get_pdf_and_bin_edges()[1],
                                                         self.obs_point2.get_pdf_and_bin_edges()[1])
            self.curve1.setData(convolved_bins, convolved_cdf)
            self.curve2.clear()
            self.info_label.setText("Convolved CDF")


def send_end_packet(poisson_rate, start_time, plotter, obs_point):
    delay = random.expovariate(poisson_rate)
    time.sleep(delay)
    end_time = time.time()

    delay_in_ms = (end_time - start_time) * 1000

    plotter.add_delay(delay_in_ms, obs_point)


def send_uniform_end_packet(min_delay, max_delay, start_time, plotter, obs_point):
    delay = random.uniform(min_delay, max_delay)
    time.sleep(delay)
    end_time = time.time()

    delay_in_ms = (end_time - start_time) * 1000

    plotter.add_delay(delay_in_ms, obs_point)


def generate_uniform_packets(rate=1000, duration=5, plotter=None, obs_point=None, min_delay=0.5, max_delay=1):
    start_time = time.time()

    while time.time() - start_time < duration:
        current_time = time.time()

        threading.Thread(target=send_uniform_end_packet, args=(min_delay, max_delay, current_time, plotter, obs_point)).start()

        uniform_rate = 1.0 / rate
        time.sleep(uniform_rate)

def generate_poisson_packets(rate=1000, duration=5, plotter=None, obs_point=None):
    start_time = time.time()

    while time.time() - start_time < duration:
        current_time = time.time()

        poisson_rate = 1
        threading.Thread(target=send_end_packet, args=(poisson_rate, current_time, plotter, obs_point)).start()
        time.sleep(1.0 / rate)


def send_normal_packet(mean, stddev, start_time, plotter, obs_point):
    delay = max(np.random.normal(mean, stddev), 0)
    time.sleep(delay / 1000.0)
    end_time = time.time()
    delay_in_ms = (end_time - start_time) * 1000
    plotter.add_delay(delay_in_ms, obs_point)

def generate_normal_packets(mean=0.1, stddev=0.3, rate=1000, duration=5, plotter=None, obs_point=None):
    start_time = time.time()

    while time.time() - start_time < duration:
        current_time = time.time()

        threading.Thread(target=send_normal_packet, args=(mean, stddev, current_time, plotter, obs_point)).start()

        time.sleep(1.0 / rate)

def run_app(rate, duration):
    app = QtWidgets.QApplication([])

    plotter = RealTimePlotter()
    plotter.show()

    threading.Thread(target=generate_uniform_packets, args=(rate, duration, plotter, plotter.obs_point1)).start()
    threading.Thread(target=generate_uniform_packets, args=(rate, duration, plotter, plotter.obs_point2)).start()

    """
    threading.Thread(target=generate_normal_packets, args=(100, 53, 1000, 5, plotter, plotter.obs_point1)).start()
    threading.Thread(target=generate_normal_packets, args=(70, 5, 1000, 5, plotter, plotter.obs_point2)).start()

    threading.Thread(target=generate_poisson_packets, args=(rate, duration, plotter, plotter.obs_point1)).start()
    threading.Thread(target=generate_poisson_packets, args=(rate, duration, plotter, plotter.obs_point2)).start()
     """

    app.exec_()


rate = 10000
duration = 10

if __name__ == '__main__':
    run_app(rate, duration)

import time
import random
import threading
import numpy as np
import pyqtgraph as pg

from ObservationPoint import ObservationPoint

from PyQt5 import QtWidgets
from PyQt5.QtCore import QTimer
from PyQt5.QtWidgets import QLabel, QVBoxLayout, QWidget


class RealTimeCDFPlotter(QtWidgets.QMainWindow):
    def __init__(self):
        super().__init__()

        self.plot_widget = pg.PlotWidget()
        self.info_label = QLabel()

        self.switch_button = QtWidgets.QPushButton("Switch to PDF")

        self.set_up_window()
        self.set_up_plot_widget()
        self.set_up_info_display()
        self.set_up_button()

        self.obs_point = ObservationPoint()

        self.curve = self.plot_widget.plot([], [], pen=pg.mkPen('b', width=2))
        self.is_cdf = True

        self.timer = QTimer()
        self.set_up_timer()

    def switch_plot_type(self):
        self.is_cdf = not self.is_cdf
        if self.is_cdf:
            self.switch_button.setText("Switch to PDF")
            self.plot_widget.setLabel('left', 'CDF')
        else:
            self.switch_button.setText("Switch to CDF")
            self.plot_widget.setLabel('left', 'PDF')

    def set_up_window(self):
        self.setWindowTitle("CDF of delays")
        self.setGeometry(100, 100, 800, 600)

    def set_up_plot_widget(self):
        self.setCentralWidget(self.plot_widget)

        self.plot_widget.setLabel('left', 'CDF')
        self.plot_widget.setLabel('bottom', 'Delay (ms)')
        self.plot_widget.showGrid(x=True, y=True)

    def set_up_timer(self):
        self.timer.timeout.connect(self.update_plot)
        self.timer.start(500)

    def set_up_button(self):
        self.switch_button.clicked.connect(self.switch_plot_type)

    def add_delay(self, delay):
        self.obs_point.add_value(delay)

    def set_up_info_display(self):
        layout = QVBoxLayout()
        container = QWidget()
        container.setLayout(layout)

        layout.addWidget(self.plot_widget)
        layout.addWidget(self.info_label)
        layout.addWidget(self.switch_button)

        self.setCentralWidget(container)

    def update_plot(self):
        if self.is_cdf:
            cdf, sorted_data = self.obs_point.get_cdf_and_sorted_values()
            self.curve.setData(sorted_data, cdf)

            self.info_label.setText(
                f"Min: {self.obs_point.get_min_value():.2f} ms | "
                f"25th Percentile: {self.obs_point.get_25_percentile():.2f} ms | "
                f"50th Percentile: {self.obs_point.get_50_percentile():.2f} ms | "
                f"75th Percentile: {self.obs_point.get_75_percentile():.2f} ms | "
                f"Max: {self.obs_point.get_max_value():.2f} ms"
            )

        else:
            pdf, bin_edges = self.obs_point.get_pdf_and_bin_edges()
            self.curve.setData(bin_edges, pdf)
            self.info_label.setText("PDF")


def send_end_packet(poisson_rate, start_time, plotter):
    delay = random.expovariate(poisson_rate)
    time.sleep(delay)
    end_time = time.time()

    delay_in_ms = (end_time - start_time) * 1000
    plotter.add_delay(delay_in_ms)


def send_uniform_end_packet(min_delay, max_delay, start_time, plotter):
    delay = random.uniform(min_delay, max_delay)
    time.sleep(delay)
    end_time = time.time()

    delay_in_ms = (end_time - start_time) * 1000
    plotter.add_delay(delay_in_ms)


def generate_poisson_packets(rate=1000, duration=5, plotter=None):
    start_time = time.time()

    while time.time() - start_time < duration:
        current_time = time.time()

        poisson_rate = 1 / 0.001  # 1 ms
        threading.Thread(target=send_end_packet, args=(poisson_rate, current_time, plotter)).start()
        time.sleep(1.0 / rate)


def generate_uniform_packets(rate=1000, duration=5, plotter=None, min_delay=0.1, max_delay=5.0):
    start_time = time.time()

    while time.time() - start_time < duration:
        current_time = time.time()

        threading.Thread(target=send_uniform_end_packet, args=(min_delay, max_delay, current_time, plotter)).start()

        uniform_rate = 1.0 / rate
        time.sleep(uniform_rate)


def run_app(rate, duration):
    app = QtWidgets.QApplication([])

    plotter = RealTimeCDFPlotter()
    plotter.show()

    threading.Thread(target=generate_uniform_packets, args=(rate, duration, plotter)).start()

    app.exec_()


rate = 10000
duration = 10

if __name__ == '__main__':
    run_app(rate, duration)

import time
import random
import threading
import numpy as np
import pyqtgraph as pg
from PyQt5.QtCore import QTimer

from cache import CacheSystem
from diagram.ObservationPoint import ObservationPoint
from PyQt5 import QtWidgets, QtCore
from PyQt5.QtWidgets import QLabel, QVBoxLayout, QHBoxLayout, QWidget, QScrollArea, QPushButton, QDialog, QTextEdit

from diagram.probabilistic import ProbabilisticOperator
from diagram.sequential import SequentialOperator
from system import System


class RealTimePlotter(QtWidgets.QMainWindow):
    def __init__(self, system):
        super().__init__()

        self.info_label = QLabel()
        self.plot_widget1 = pg.PlotWidget()
        self.plot_widget_convolution = pg.PlotWidget()

        self.switch_button = QtWidgets.QPushButton("Switch to CDF")
        self.system_diagram_button = QtWidgets.QPushButton("System Convolution Diagram")
        self.convolution_button = QPushButton("Show System Convolution Diagram")

        self.system = system
        self.plottable_components = system.get_all_plottable_components()
        self.current_obs_index = None

        self.set_up_window()
        self.set_up_plot_widgets()
        self.set_up_info_display()
        self.set_up_buttons()

        self.curve1 = self.plot_widget1.plot([], [], pen=pg.mkPen('b', width=2))
        self.convolution_curve = self.plot_widget_convolution.plot([], [], pen=pg.mkPen('r', width=2))
        self.mode = 0
        self.timer = QTimer()
        self.set_up_timer()

    def switch_plot_type(self):
        self.mode = (self.mode + 1) % 2

        if self.mode == 0:
            self.switch_button.setText("Switch to CDF")
            self.plot_widget1.setLabel('left', 'PDF')

            self.plot_widget1.show()

        elif self.mode == 1:
            self.switch_button.setText("Switch to PDF")
            self.plot_widget1.setLabel('left', 'CDF')

            self.plot_widget1.show()

    def set_up_window(self):
        self.setWindowTitle("PDF, CDF, Convolution")
        self.setGeometry(100, 100, 1000, 600)

    def set_up_plot_widgets(self):
        self.plot_widget1.setLabel('bottom', 'Delay (ms)')
        self.plot_widget1.showGrid(x=True, y=True)

    def set_up_timer(self):
        self.timer.timeout.connect(self.update_plot)
        self.timer.start(500)

    def set_up_buttons(self):
        self.switch_button.clicked.connect(self.switch_plot_type)
        self.system_diagram_button.clicked.connect(self.show_system_convolution_diagram)

    def add_delay(self, delay, obs_point):
        obs_point.add_value(delay)

    def set_up_info_display(self):
        main_layout = QHBoxLayout()
        container = QWidget()
        container.setLayout(main_layout)

        self.plot_layout = QVBoxLayout()
        self.plot_layout.addWidget(self.plot_widget1)

        right_layout = QVBoxLayout()

        right_layout.addWidget(self.info_label)
        right_layout.addWidget(self.switch_button)
        right_layout.addWidget(self.system_diagram_button)

        self.scroll_area = QScrollArea()
        self.scroll_area.setWidgetResizable(True)

        scroll_content = QWidget()
        scroll_layout = QVBoxLayout(scroll_content)

        for index, component in enumerate(self.plottable_components):
            btn = QPushButton(component.name)
            btn.clicked.connect(lambda _, idx=index: self.plot_observation_point(idx))
            scroll_layout.addWidget(btn)

        self.scroll_area.setWidget(scroll_content)

        right_layout.addWidget(self.scroll_area)

        main_layout.addLayout(self.plot_layout, stretch=3)
        main_layout.addLayout(right_layout, stretch=1)

        self.setCentralWidget(container)

    def update_plot(self):
        if self.current_obs_index is None:
            return

        obs_point = self.plottable_components[self.current_obs_index]

        if self.mode == 0:
            pdf, bin_edges = obs_point.get_pdf_and_bin_edges()
            self.curve1.setData(bin_edges, pdf)
            self.info_label.setText(f"PDF for {obs_point.name}")

        elif self.mode == 1:
            cdf, sorted_data = obs_point.get_cdf_and_sorted_values()
            self.curve1.setData(sorted_data, cdf)
            self.info_label.setText(f"CDF for {obs_point.name}")

    def plot_observation_point(self, idx):
        self.current_obs_index = idx
        self.update_plot()

    def show_system_convolution_diagram(self):
        """
        Show the System Convolution Diagram.
        This will recalculate the system's DQ every second and plot the CDF over time.
        """
        dialog = QDialog(self)
        dialog.setWindowTitle("System Convolution Diagram")

        plot_widget_convolution = pg.PlotWidget()

        layout = QVBoxLayout()
        layout.addWidget(plot_widget_convolution)

        dialog.setLayout(layout)
        dialog.resize(600, 400)

        convolution_curve = plot_widget_convolution.plot([], [], pen=pg.mkPen('r', width=2))

        self.update_convolution_plot(convolution_curve)

        dialog.exec_()

    def update_convolution_plot(self, convolution_curve):

        cdf, values = self.system.calculate_dq()
        convolution_curve.setData(values, cdf)


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


def simulate_memory_access(duration, cache_system):
    """
    Simulate memory access for 'duration' seconds using the CacheSystem.
    """
    start_time = time.time()
    while time.time() - start_time < duration:
        index = random.randint(0, len(cache_system.main_memory) - 1)
        cache_system.access_cache(index)

def run_app(rate, duration):
    app = QtWidgets.QApplication([])

    syst = System()
    hit = ObservationPoint("hit")
    miss = ObservationPoint("miss")
    main = ObservationPoint("main")
    syst.add_component(hit)
    syst.add_component(miss)
    syst.add_component(main)

    probabilities = {hit: 0.95, miss: 0.05}
    read_prob = ProbabilisticOperator("read_prob", probabilities)
    syst.add_component(read_prob)

    ret = SequentialOperator("return")
    main_seq = SequentialOperator("main_seq")
    syst.add_component(ret)
    syst.add_component(main_seq)

    read = SequentialOperator("read")
    syst.add_component(read)
    syst.set_first_component("read")

    read.set_next_operator(read_prob)
    hit.set_next(ret)
    main.set_next(ret)
    miss.set_next(main_seq)
    main_seq.set_next_operator(main)

    plotter = RealTimePlotter(syst)
    plotter.show()

    main_memory = [i * 10 for i in range(10)]
    cache_system = CacheSystem(main_memory, 25, syst)

    threading.Thread(target=simulate_memory_access, args=(duration, cache_system)).start()

    app.exec_()

rate = 1000
duration = 10

if __name__ == '__main__':
    run_app(rate, duration)

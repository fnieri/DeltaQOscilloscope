import pyqtgraph as pg
from PyQt5.QtCore import QTimer

from PyQt5 import QtWidgets, QtCore
from PyQt5.QtWidgets import QLabel, QVBoxLayout, QHBoxLayout, QWidget, QScrollArea, QPushButton, QDialog, QTextEdit


class RealTimePlotter(QtWidgets.QMainWindow):
    def __init__(self, system):
        super().__init__()

        self.info_label = QLabel()
        self.plot_widget1 = pg.PlotWidget()
        self.plot_widget_convolution = pg.PlotWidget()

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
        self.timer = QTimer()
        self.set_up_timer()


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

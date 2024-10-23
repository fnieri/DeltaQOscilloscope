import random
import threading
import time

import numpy as np

from cache import CacheSystem
from diagram.ObservationPoint import ObservationPoint
from diagram.probabilistic import ProbabilisticOperator
from diagram.sequential import SequentialOperator
from plotter.plot import RealTimePlotter
from system import System

import pyqtgraph as pg
from PyQt5.QtCore import QTimer

from PyQt5 import QtWidgets, QtCore
from PyQt5.QtWidgets import QLabel, QVBoxLayout, QHBoxLayout, QWidget, QScrollArea, QPushButton, QDialog, QTextEdit



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

    probabilities = {hit: 1, miss: 0}
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

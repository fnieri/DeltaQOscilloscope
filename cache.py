import random
import time
from collections import deque
from turtledemo.forest import start

from diagram.ObservationPoint import ObservationPoint
from diagram.probabilistic import ProbabilisticOperator
from diagram.sequential import SequentialOperator
from system import System


class CacheSystem:
    def __init__(self, main_memory, cache_size, system, hit_probability=0.95):
        self.main_memory = main_memory  # Main memory array
        self.cache = deque(maxlen=cache_size)  # Cache implemented as a deque with max size
        self.hit_probability = hit_probability
        self.system = system

    def access_cache(self, index):
        """
        Simulates reading from the cache.
        - If cache hit, return the data from cache.
        - If cache miss, load from main memory, store in cache, and return data.
        """
        # Generate a random number to decide hit or miss
        is_hit = random.random() < self.hit_probability
        start_time = time.time()
        if is_hit and index in self.cache:
            print(f"Cache HIT: Returning data for index {index} from cache.")
            ret = self.main_memory[index]
            end_time = time.time()
            delay = end_time - start_time
            self.system.add_time("hit", delay)
            return ret
        else:
            print(f"Cache MISS: Loading data for index {index} from main memory.")
            ret = self.load_from_main_memory(index)
            end_time = time.time()

            delay = end_time - start_time
            self.system.add_time("miss", delay)
            return ret

    def load_from_main_memory(self, index):
        """Fetch data from main memory and store it in the cache."""
        start_time = time.time()
        data = self.main_memory[index]

        self.cache.append(index)
        end_time = time.time()
        delay = end_time - start_time
        self.system.add_time("main", delay)
        return data

    def cache_status(self):
        """Returns the current cache status (indexes stored in cache)."""
        return list(self.cache)

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
# Example usage:
main_memory = [i * 10 for i in range(100)]  # Simulated main memory with 100 elements
cache_system = CacheSystem(main_memory, 5, syst)

# Simulate accessing some data
for i in range(1000):
    i = random.randint(0, 99)
    print(f"\nAccessing index {i}:")
    result = cache_system.access_cache(i)
    print(f"Data: {result}")
    print(f"Cache Status: {cache_system.cache_status()}")

print(cache_system.system.components["miss"].values)

syst.calculate_dq()
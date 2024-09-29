import random
import time
from collections import deque
from turtledemo.forest import start

from diagram.ObservationPoint import ObservationPoint
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



# Example usage:
main_memory = [i * 10 for i in range(100)]  # Simulated main memory with 100 elements
cache_system = CacheSystem(main_memory, 5, syst)

# Simulate accessing some data
for i in [1, 10, 3, 1, 20, 10]:
    print(f"\nAccessing index {i}:")
    result = cache_system.access_cache(i)
    print(f"Data: {result}")
    print(f"Cache Status: {cache_system.cache_status()}")

print(cache_system.system.components["miss"].values)
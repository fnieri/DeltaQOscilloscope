import random
import time
from collections import deque

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
        is_hit = random.random() < self.hit_probability
        start_time = time.time()
        if is_hit and index in self.cache:
            ret = self.main_memory[index]
            end_time = time.time()
            delay = end_time - start_time
            self.system.add_time("hit", delay)
            return ret
        else:
            ret = self.load_from_main_memory(index)
            end_time = time.time()

            delay = end_time - start_time
            self.system.add_time("miss", delay)
            return ret

    def load_from_main_memory(self, index):
        """Fetch data from main memory and store it in the cache."""
        start_time = time.perf_counter()
        data = self.main_memory[index]

        self.cache.append(index)
        end_time = time.perf_counter()

        delay = end_time - start_time
        self.system.add_time("main", delay)
        return data

    def cache_status(self):
        """Returns the current cache status (indexes stored in cache)."""
        return list(self.cache)

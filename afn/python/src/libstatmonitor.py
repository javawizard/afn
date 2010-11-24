
import re
import time

__doc__ = """\
This module contains functions that can be used to extract CPU usage info and
memory usage info from /proc/stat and /proc/meminfo. For /proc/stat
specifically, this module stores the old values obtained on the last read and
uses those to compute the processor's usage since then. It also tracks
multiple processors independently.
"""

class CPUMonitor(object):
    """
    A class that monitors CPU usage via /proc/stat. Each instance of
    CPUMonitor keeps track of the values at the last read and uses these to
    compute the processor's usage since then.
    """
    types = (("user", 0), ("user_nice", 1), ("system", 2))
    
    def __init__(self):
        self.last_map = {} # Maps CPUs to maps of types to their last values
        self._populate(self.last_map) # Initial population
        self.last_time = time.time()
        self.results = {}
        for number in self.last_map:
            self.results[number] = dict((type, 0.0) for type, _ in self.types)
    
    def _populate(self, map):
        with open("/proc/stat") as stat:
            for line in stat:
                if re.match("cpu[0-9] .*", line):
                    number, params = re.match("cpu([0-9]) +(.*)",
                            line).groups()
                    number = int(number)
                    params = params.split(" ")
                    map[number] = dict((key, int(params[index])) for key, index
                            in CPUMonitor.types)
    
    def refresh(self):
        """
        Reads /proc/stat and computes the CPU usage since the last time this
        function was called. The computed usage will then be available through
        all of the get_* functions. The results range from 0.0 to 100.0.
        """
        current_map = {}
        results = {}
        new_time = time.time()
        time_difference = new_time - self.last_time
        self.last_time = new_time
        self._populate(current_map)
        for number, this_map in current_map.items():
            last_map = self.last_map[number]
            results[number] = dict((type, (this_map[type] - last_map[type]) / time_difference)
                    for type, _ in CPUMonitor.types)
        self.last_map = current_map
        self.results = results
    
    def get(self, processor):
        """
        Returns a tuple (user, user_nice, system) for the specified processor.
        """
        results = self.results[processor]
        return results["user"], results["user_nice"], results["system"]
    
    def get_condensed(self, processor):
        """
        Returns a tuple (user, system), where user is user and user_nice
        added together.
        """
        results = self.results[processor]
        return (results["user"] + results["user_nice"]), results["system"]
    
    def get_processors(self):
        """
        Returns the number of processors present on this system.
        """
        return len(self.results)
    








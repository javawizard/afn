
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
    
    def get(self, processor=None):
        """
        Returns a tuple (user, user_nice, system) for the specified processor.
        If processor is None, stats for all of the CPUs will be averaged and
        returned.
        """
        if processor is None:
            results = dict((type, 0.0) for type, _ in CPUMonitor.types)
            for cpu_results in self.results.values():
                for type, _ in CPUMonitor.types:
                    results[type] += cpu_results[type]
            for type, _ in CPUMonitor.types:
                results[type] /= len(self.results)
        else:
            results = self.results[processor]
        return results["user"], results["user_nice"], results["system"]
    
    def get_condensed(self, processor=None):
        """
        Returns a tuple (user, system), where user is user and user_nice
        added together.
        """
        user, user_nice, system = self.get(processor)
        return (user + user_nice), system
    
    def get_processors(self):
        """
        Returns the number of processors present on this system.
        """
        return len(self.results)

class MemoryMonitor(object):
    types = ["MemTotal", "MemFree", "SwapTotal", "SwapFree", "Buffers", "Cached"]
    def __init__(self):
        self.map = dict((key, 0) for key in MemoryMonitor.types)
        self.refresh()
    
    def refresh(self):
        with open("/proc/meminfo") as meminfo:
            for line in meminfo:
                if ":" not in line:
                    continue
                type = line[:line.find(":")]
                if type not in MemoryMonitor.types:
                    continue
                line = line[line.find(":") + 1:].strip()
                linesplit = line.split(" ")
                number = linesplit[0]
                specifier = None
                if len(linesplit) > 1:
                    specifier = linesplit[1].lower()
                number = int(number)
                if specifier == "kb":
                    number *= 1000
                elif specifier == "mb":
                    number *= 1000000
                self.map[type] = number
    
    def get(self):
        """
        returns a tuple (resident, buffers, cached, free, total, swap,
        swapfree, swaptotal) of memory statistics. Each item is an integer (or
        a long) representing the number of bytes.
        """
        total, free, swaptotal, swapfree, buffers, cached = (
                self.map["MemTotal"], self.map["MemFree"],
                self.map["SwapTotal"], self.map["SwapFree"],
                self.map["Buffers"], self.map["Cached"])
        swap = swaptotal - swapfree
        resident = total - (free + buffers + cached)
        return (resident, buffers, cached, free, total, swap,
                swapfree, swaptotal)






def slicer(length, start=None, stop=None, step=None):
    if step <= 0:
        raise ValueError("step can't be 0 or negative. Negative steps will be supported soon.")
    if start is None:
        start = 0
    if stop is None:
        stop = length
    if step is None:
        step = 1
    if start < 0:
        start += length
    if stop < 0:
        stop += length
    while start < stop:
        yield start
        start += step

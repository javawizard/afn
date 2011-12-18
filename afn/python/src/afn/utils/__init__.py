
def slicer(length, start=None, stop=None, step=None):
    if step == 0:
        raise ValueError("step can't be 0")
    if start is None:
        if step < 0:
            start = length
        else:
            start = 0
    if stop is None:
        if step < 0:
            stop = 0
        else:
            stop = length
    if step is None:
        step = 1
    if start < 0:
        start += length
    if stop < 0:
        stop += length
    if step > 0:
        while start < stop:
            yield start
            start += step
    else:
        while start > stop:
            yield start
            start += step
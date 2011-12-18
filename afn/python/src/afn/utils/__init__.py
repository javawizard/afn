
def slicer(length, start=None, stop=None, step=None):
    start, stop, step = slice(start, stop, step).indices(length)
    if(step > 0):
        while start < stop:
            yield start
            start += step
    else:
        while start > stop:
            yield start
            start += step


import functools

def tracker(track_list):
    """
    A decorator that can be used on an RTK connect function thus:
    
    connections = []
    @tracker(connections)
    def start(connection):
        ...
    
    Every connection passed into the method will be added to the specified
    list. A listener is registered on every such connection that will cause
    it to be removed from the list when it closes.
    """
    def decorator(function):
        def wrapper(connection):
            def close():
                track_list.remove(connection)
            connection.add_close_function(close)
            track_list.append(connection)
            function(connection)
        functools.update_wrapper(wrapper, function)
        return wrapper
    return decorator

















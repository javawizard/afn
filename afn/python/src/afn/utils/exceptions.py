
class SemanticExceptionMixin(object):
    """
    A mixin that can be used to turn an exception into a semantic exception.
    Semantic exceptions expect an attribute (usually defined as the class
    level) named _format that contains a printf-style format string. The
    exception's __str__ will return this format string, formatted with a
    dictionary of all keyword arguments passed to an exception instance's
    constructor. All keyword arguments are also set as attributes on instances
    of the exception.
    
    An example will probably make things easier to understand:
    
    >>> class TestException(SemanticExceptionMixin, Exception):
    ...     _format = "The %(thing)s messed up with a %(reason)s."
    ... 
    >>> e = TestException(thing='foo', reason='bar')
    >>> e
    __main__.TestException(thing='foo', reason='bar')
    >>> e.thing
    'foo'
    >>> e.reason
    'bar'
    >>> raise e
    Traceback (most recent call last):
      File "<stdin>", line 1, in <module>
    __main__.TestException: The foo messed up with a bar.
    """
    def __init__(self, **kwargs):
        self._values = kwargs
        for k, v in kwargs.items():
            setattr(self, k, v)
    
    def __repr__(self):
        return "%s.%s(%s)" % (
                    type(self).__module__,
                    type(self).__name__,
                    ", ".join("%s=%r" % (k, v) for k, v in self._values.items())
                )
    
    def __str__(self):
        return self._format % self._values

class Function(object):
    def get_closures(self, arg_count):
        raise NotImplementedError
    
    def call_function(self, dynamic_context, args):
        raise NotImplementedError
    
    def get_min_args(self):
        raise NotImplementedError
    
    def get_max_args(self):
        raise NotImplementedError
    
    def call_with_values(self, dynamic_context, args):
        # TODO: What this should do is check to see if any of the args are
        # closures, and if they are, wrap them in closures that just return
        # the values provided here.
        return self.call(dynamic_context, args)


class Module(Function):
    def get_function(self, name):
        raise NotImplementedError
    
    def get_default_bind_name(self):
        raise NotImplementedError













































"""
Zelden Pipeline Framework: Both fell silent, peering out through a pipeline.

This module provides the Zelden Pipeline Framework. It's used throughout
Zelden to allow various components to process information as it flows through
Zelden. It could be considered a replacement for glib's signals as they're
used in Pidgin and its plugin architecture.

Pipelines allow objects to be injected into a pipeline. Pipelines are made up
of a number of stages, and each stage can process the object as it makes its
way through the pipeline. Stages can replace the object with another one,
instruct the pipeline to discard the object without passing it through any
further stages, fork the object into several objects that will each be
processed by the remaining stages, or simply observe the object and not
actually change it in any way.

Stages have an associated priority. This can be any type of object as long as
the priorities of all of the stages in a pipeline are mutually comparable with
Python's cmp function. Stages with /lower/ priority will process objects
/first/, so that objects progress from stages with lower priorities to stages
with higher priorities as they make their way through the pipeline. I would
imagine that priorities would typically be integers, floating-point numbers,
or instances of decimal.Decimal (all of which happen to be mutually
comparable, so a mix of the three could be used in a single pipeline).

Objects can be injected at a particular priority. Only stages with a priority
equal to or greater than this will process the object. This allows objects to
be injected into the middle of a pipeline.

Among stages with the same priority, those added to a pipeline with attach
will process objects last while those added to a pipeline with attach_before
will process objects first.

If an object makes it through a pipeline without being discarded, the
pipeline's optional consumer function will be called, passing in the object.
This allows pipelines to be chained together. In fact, pipelines have a
function, chain, that sets the consumer function accordingly to chain the two
pipelines together.

Typically, you'll use a pipeline's attach and attach_before functions to
attach stages to a pipeline. However, you might want the list of stages to be
pulled from somewhere else at the particular instant that an object enters the
pipeline. In this case, you can implement your own StageManager instance.
StageManager provide three functions: get_stages, attach, and attach_before.

The attach and attach_before functions have the exact same signature and
behavior as their matching functions on the Pipeline class. Indeed, Pipeline's
implementation of these two methods simply calls these functions on the stage
manager. A stage manager is not required to provide implementations of these
functions if it doesn't make sense. For example, a stage manager that
dynamically computes the list of stages to be executed on any particular
object most likely wouldn't allow modification via those two functions. This
is perfectly fine.

The get_stages function is the one that does the actual work. It takes an
object and the priority at which the object was injected and returns the list
of stages that should process the object. DefaultStageManager, the stage
manager class used when no stage manager is specified when creating a
pipeline, implements this as per the traditional semantics of a pipeline: it
returns all of the stages registered to it with attach or attach_before with a
priority greater than or equal to the specified object's priority, in order
from lowest priority to highest priority. Custom stage managers are free to
order their results however they like.

Right now, the inject function doesn't return anything, with the expectation
that any final processing of the object will either be attached as a stage
with a relatively high priority or provided as the consumer function. In the
future, I may provide a function such as process that functions similar to
inject but keeps track of the object as it flows through the pipeline and
returns it as soon as processing has completed. I would probably also provide
a process_all function that returns a list of result objects in case any of
the stages fork the object.

So, let's get on to some examples. First we'll import the module:

from zelden.server import pipeline

Now let's define the following functions:

def add_five(object): # adds 5 to the specified object and returns it, causing
    # the pipeline to replace the passed-in object with the new value
    return object + 5

def times_three(object): # multiplies the object by 3 and returns it
    return object * 3

def print_value(object): # Prints the object without modifying it further
    print object

Now, let's create an example pipeline that adds five to an object, then
multiplies it by three, then prints it:

p1 = pipeline.Pipeline()
p1.attach(add_five)
p1.attach(times_three)
p1.attach(print_value)

Now we'll inject the number 4 into our pipeline:

>>> p1.inject(4)
27

The first stage replaced our input, 4, with 4 + 5, or 9. The second stage then
multiplied that by 3 to get 27. The third stage then printed that number.

Note, however, that if we had attached the second stage with add_before, then
the number 4 would have been processed with that stage first. The result would
have been 17: 4 times 3 is 12, and 5 added to that is 17.

The same behavior would have resulted if we had specified priorities for all
of the stages and specified a lower priority for the second stage.
"""

class StageManager(object):
    def get_stages(self, object, priority):
        raise Exception("Stage manager error: " + str(type(self)) + 
                " doesn't provide a get_stages function.")
    
    def attach(self, stage, priority):
        raise Exception("Stage manager " + str(type(self)) + 
                "doesn't support the attach function.")
    
    def attach_before(self, stage, priority):
        raise Exception("Stage manager " + str(type(self)) + 
                "doesn't support the attach_before function.")

class DefaultStageManager(object):
    def __init__(self):
        self.stages = []
    
    def attach(self, stage, priority):
        self.stages.append((priority, stage))
        self.sort_stages()
    
    def attach_before(self, stage, priority):
        self.stages[0:0] = [(priority, stage)] # Prepend the tuple to the
        # beginning of the stage list
        self.sort_stages()
    
    def get_stages(self, object, object_priority):
        for index, (stage_priority, stage) in enumerate(self.stages):
            if object_priority <= stage_priority:
                return zip(*self.stages[index:])[1] # Only get the last items
                # in the tuples stored in self.stages, since those are the
                # actual stage functions
        # This object's priority is higher than all of the stages, so we
        # return the empty list.
        return []
    
    def sort_stages(self):
        self.stages.sort(key=lambda item: item[0]) # Sort by priority, the
        #first item in each tuple in the stage list

class Pipeline(object):
    def __init__(self, consumer=None, stage_manager=None):
        """
        Creates a new pipeline. If a consumer is specified, it should be a
        one-argument function. It will be called and passed all objects that
        pass through the pipeline. If the consumer is not specified, it's
        assumed that all necessary processing on the object will be done by
        various stages attached to the pipeline.
        
        The stage manager is the object that looks up the list of stages to be
        applied to a particular object entering the pipeline. Calls to attach
        and attach_before delegate to the stage manager, and a call to inject
        will cause the stage manager to be asked for the list of stages to
        apply to the specified entering object. This allows the list of stages
        to be sourced elsewhere. If a stage manager is not specified, a
        DefaultStageManager will be used, which provides "traditional"
        behavior expected from a pipeline.
        """
        if consumer is None:
            consumer = lambda result: None
        if stage_manager is None:
            stage_manager = DefaultStageManager()
        self.consumer = consumer
        self.stage_manager = stage_manager
    
    def attach(self, stage, priority=1000):
        self.stage_manager.attach(stage, priority)
    
    def attach_before(self, stage, priority=1000):
        self.stage_manager.attach_before(stage, priority)
    
    def inject(self, object, priority=0):
        # First, we get the list of stages to process this object with.
        stages = self.stage_manager.get_stages(object, priority)
        # Then we do the actual processing.
        self._process_stages(object, stages)
    
    def chain(self, other_pipeline, priority=0):
        self.consumer = lambda object: other_pipeline.inject(object, priority)
    
    def _process_stages(self, object, stages):
        for index, stage in enumerate(stages):
            result = stage(object)
            if result is None:
                # Stage isn't replacing the object, so we'll continue
                # processing with the next stage.
                pass
            elif isinstance(result, _DiscardInstruction):
                # Stage is discarding the object, so we quit processing.
                return
            elif isinstance(result, _ForkInstruction):
                # Stage forked the object into several objects. We'll
                # recursively process each one, then return.
                remaining_stages = stages[index + 1:]
                for forked_object in result.objects:
                    self._process_stages(forked_object, remaining_stages)
                return
            else:
                # Stage is replacing the object, so we store the new object to
                # process.
                object = result
        # Last stage has processed the object and we still have a result, so
        # we pass it into the consumer function.
        self.consumer(object)


class _DiscardInstruction(object):
    pass

discard = _DiscardInstruction()

class _ForkInstruction(object):
    def __init__(self, objects):
        self.objects = objects

def fork(*objects):
    return _ForkInstruction(list(objects))



































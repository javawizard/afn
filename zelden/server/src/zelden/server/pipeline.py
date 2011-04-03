
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



































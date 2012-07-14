"""
This module contains classes and functions relating to publishing services.
"""

from Queue import Queue, Empty

from autobus2 import net, messaging, exceptions, common, constants
from autobus2.get_function_doc import get_function_doc
import autobus2
from utils import no_exceptions
from traceback import print_exc
from threading import Thread
import functools
from afn.utils import Suppress
from afn.utils.concurrent import synchronized_on
from afn.utils.multimap import Multimap as _Multimap
from afn.utils.listener import Event, EventTable, PropertyTable
from afn.utils.partial import Partial
import itertools
import copy
from abc import ABCMeta, abstractmethod

class RemoteConnection(object):
    """
    A connection from a remote client attached to a particular local service.
    This is for Autobus's use only; external code won't ever need to see this.
    
    The class that external code receives instances of when connecting to a
    service is Connection, not RemoteConnection. When a Connection connects,
    the other end of the socket, the one on the listening end, will be a
    RemoteConnection.
    """
    def __init__(self, bus, socket):
        self.bus = bus
        self.socket = socket
        self.queue = Queue()
        self.service = None
        self.watched_objects = set()
        self.listened_events = set()
        net.OutputThread(socket, self.queue.get).start()
        net.InputThread(socket, self.received, name="Input thread for " + repr(self)).start()
    
    def received(self, message):
        # print "Received: " + str(message)
        if message is None: # Connection lost
            self.cleanup()
            return
        try:
            if self.service is None: 
                if message["_command"] != "bind":
                    raise Exception("First message must be bind")
                name = message["service"]
                with self.bus.lock:
                    service = self.bus.local_services.get(name)
                    if not service:
                        self.send_error(message, "The service you attempted to bind to does not exist.")
                        self.close()
                        return
                    self.service = service
                    # The response includes the service since specifying None
                    # as the service causes the introspection service to be
                    # bound to, and this lets the client know what the
                    # introspection service's service id currently is
                    self.send(messaging.create_response(message, service=name))
                    return
            processor = getattr(self, "process_" + message["_command"], None)
            if not processor:
                self.send_error(message, "That command does not exist.")
            processor(message)
        except:
            print_exc()
            with no_exceptions:
                self.send_error(message, "An exception was thrown while processing that command.")
    
    def send(self, message):
        # print "Sending " + str(message)
        if message:
            self.queue.put(message)
    
    def send_error(self, command_or_id, reason):
        self.send(messaging.create_error(command_or_id, reason))

    def close(self):
        """
        Forces this connection to close and disconnect its client.
        """
        self.queue.put(None)
        net.shutdown(self.socket)
    
    def cleanup(self):
        """
        Cleans up this connection, removing all event listeners and object
        watches it's registered, closing network connections, shutting down
        threads, etc. This is used as the InputThread's cleanup function.
        """
        with self.bus.lock:
            # Make sure the OutputThread closes down
            self.queue.put(None)
            # Remove ourselves from the bus's list of connections
            self.bus.bound_connections.remove(self)
            # Shut down our socket
            net.shutdown(self.socket)
            # If we've bound to a service, remove any object watches and event
            # listeners we've registered
            if self.service:
                for name in self.watched_objects:
                    self.service.object_values.unwatch(name, Partial(self.watched_object_changed, name))
                for name in self.listened_events:
                    self.service.event_listeners.unlisten(name, Partial(self.listened_event_fired, name))
    
    def process_call(self, message):
        """
        Called to process incoming call commands.
        """
        name = message["name"]
        args = message["args"]
        # Look up the functions' policy
        policy = self.service.provider.__autobus_policy__(name)
        # If the policy is SYNC, invoke the function and send the response
        # synchronously
        if policy == autobus2.SYNC:
            self.send(self.call_function(message, name, args))
        # If the policy is THREAD, invoke the function and send the
        # response on a new thread
        elif policy == autobus2.THREAD:
            Thread(name="autobus2.local.RemoteConnection-function-caller",
                    target=lambda: self.send(self.call_function(message, name, args))).start()
        # If the policy is ASYNC, send a null response now, then invoke the
        # function, discarding its value, on a thread.
        else:
            self.send(messaging.create_response(message, result=None))
            Thread(name="autobus2.local.RemoteConnection-function-async-caller",
                    target=functools.partial(self.call_function, message, name, args)).start()
    
    def call_function(self, message, name, args):
        """
        Calls self.service.provider.__autobus_call__ to call the specified
        function. A response object is then created and returned, containing an
        appropriate response. If an exception is thrown, it is caught, and
        an appropriate error response generated and returned.
        
        The message will be generated as a reply to the message passed into
        this method.
        
        This method is only really used from process_call; process_call looks
        up the method's calling policy with
        self.service.provider.__autobus_policy__ and delegates to call_function
        either in a thread or synchronously depending on the policy.
        """
        try:
            result = self.service.provider.__autobus_call__(*args)
            return messaging.create_response(message, result=result)
        except exceptions.NoSuchFunctionException:
            return messaging.create_error(message, "That function (\"%s\") does not exist." % name)
        except Exception as e:
            return messaging.create_error(message, "Remote function threw an exception: %s: %s" % (type(e), e))
    
    def process_watch(self, message):
        """
        Called to process incoming watch commands.
        """
        with self.bus.lock:
            # print "watch: ", message
            name = message["name"]
            # Check to make sure we're not already watching the object
            if name in self.watched_objects:
                self.send_error(message, "You're already watching that object.")
                return
            # Add the object to our internal list of objects we're watching
            self.watched_objects.add(name)
            # Send a response including the object's current value
            object_value = self.service.object_values.get(name, None)
            self.send(messaging.create_response(message, name=name, value=object_value))
            # Add ourselves to the service's object_values property table to
            # be notified when things actually change. This cause a duplicate
            # notification of the object's value to be sent to the client; how
            # to deal with this in the future needs to be better thought out.
            # (Perhaps not including the value in the response sent above and
            # just relying on the notification is the way to go.)
            self.service.object_watchers.listen(name, Partial(self.watched_object_changed, name))
    
    def process_listen(self, message):
        raise NotImplementedError
    
    def process_unwatch(self, message):
        raise NotImplementedError
    
    def process_unlisten(self, message):
        raise NotImplementedError
    
    def watched_object_changed(self, name, value):
        """
        Called when an object being watched by this connection changes. This
        method is added to the object_values property table of the LocalService
        to which this connection is bound. The property table will take care of
        calling this when the object appears or disappears or when we start
        watching or stop watching the object.
        """
        self.send(messaging.create_command("changed", True, name=name, value=value))
    
    def process_ping(self, message):
        self.send(messaging.create_response(message))


class LocalService(common.AutoClose):
    """
    A service created locally and published to remote clients. This is the
    class of objects returned from bus.create_service().
    
    The various create_* functions can be called on LocalService instances to
    create functions, events, and objects that are published remotely.
    
    The function activate() can be called for services created by
    bus.create_service(..., active=False). This causes the service to become
    active. Inactive services are hidden from the outside world; this allows
    all of the functions needed for a particular service to be set up before
    it's published to the network.
    """
    def __init__(self, bus, id, info, provider):
        self.provider_event = self.provider_event # Make sure we have an
        # identitywise-persistent bound function so that we can properly use it
        # as a listener on the provider
        self.id = id
        self.info = info
        self.bus = bus
        self.provider = provider
        self.doc = None
        # Register ourselves as a listener on the provider
        provider.__autobus_listen__(self.provider_event)
        # Not active by default. TODO: change this?
        self.active = False
        self.is_alive = True
        # Map of function names to info dicts. This is a property table to
        # allow the Autobus introspection service to watch it for changes to
        # the functions provided by this service.
        self.functions = PropertyTable()
        # Ditto, but for events
        self.events = PropertyTable()
        # Ditto, but for objects
        self.objects = PropertyTable()
        # Map of object names to object values. This is what's used to detect
        # changed objects from RemoteConnection instances.
        self.object_values = PropertyTable()
        # EventTable of event listeners listening for events to be fired.
        # Listeners are of the form listener(args), i.e. the arguments are
        # passed as a sequence, not expanded.
        self.event_listeners = EventTable()
    
    # TODO: I'm writing this at 3 in the morning; make sure my head was on
    # straight when I synchronized on bus.lock
    # Update: Yep, it was on straight. Things would majorly break without
    # said synchronization.
    @synchronized_on("bus.lock")
    def provider_event(self, event, *args):
        """
        Processes an event issued by the provider this service is observing.
        """
        if event is constants.OBJECT_ADDED:
            # Object added. Store the object's info and update the property
            # table holding its value.
            name, info, value = args
            # If the value isn't jsonable, print an error and rewrite it to null
            if not net.is_jsonable(value):
                print "ERROR: Value %r for object %r in %r is not jsonable." % (
                        value, name, self)
            self.objects[name] = info
            # Deep copy it so that changes won't propagate
            self.object_values[name] = copy.deepcopy(value)
        elif event is constants.OBJECT_UPDATED:
            # Object's info object updated. Store the new info object.
            name, info = args
            self.objects[name] = info
        elif event is constants.OBJECT_CHANGED:
            # Object's value changed. Store the new value.
            name, value = args
            # If the value isn't jsonable, print an error and rewrite it to null
            if not net.is_jsonable(value):
                print "ERROR: Value %r for object %r in %r is not jsonable." % (
                        value, name, self)
            # Deep copy it so that changes won't propagate
            self.object_values[name] = copy.deepcopy(value)
        elif event is constants.OBJECT_REMOVED:
            # Object removed. Delete the info object and the value.
            name, = args
            del self.objects[name]
            del self.object_values[name]
        elif event is constants.EVENT_ADDED:
            # Event added. Store the event's info object.
            name, info = args
            self.events[name] = info
        elif event is constants.EVENT_UPDATED:
            # Event's info object changed. Store the new info object.
            name, info = args
            self.events[name] = info
        elif event is constants.EVENT_FIRED:
            # Event fired. Notify the event's listeners.
            name, event_args = args
            self.event_listeners(name, event_args)
        elif event is constants.EVENT_REMOVED:
            # Event removed. Remove the event's info object.
            name, = args
            del self.events[name]
        elif event is constants.FUNCTION_ADDED:
            # Function added. Store the function's info object.
            name, info = args
            self.functions[name] = info
        elif event is constants.FUNCTION_UPDATED:
            # Function's info object updated. Update the info object.
            name, info = args
            self.functions[name] = info
        elif event is constants.FUNCTION_REMOVED:
            # Function removed. Remove the info object.
            name, = args
            del self.functions[name]
        elif event is constants.SERVICE_CHANGED:
            # Service changed. We're ignoring this for now as I'm not yet
            # having service information actually published; I'll add that
            # later.
            pass
        else:
            print "Invalid event %s received from service provider %s" % (
                    event, self.provider)
    
    @synchronized_on("bus.lock")
    def activate(self):
        """
        Activates this service. Services that are not active are not published
        to the bus's publishers; they can still be connected to by a client
        that knows the service's host, port, and service id.
        """
        if self.active: # Already active
            return
        if not self.is_alive:
            raise exceptions.ClosedException("This LocalService has been closed.")
        self.active = True
        self.bus._i_update(self.id)
        for publisher in self.bus.publishers:
            publisher.add(self)
    
    @synchronized_on("bus.lock")
    def deactivate(self):
        if not self.active: # Not active
            return
        if not self.is_alive:
            return
        self.active = False
        for publisher in self.bus.publishers:
            publisher.remove(self)
        self.bus._i_update(self.id)
    
    @synchronized_on("bus.lock")
    def close(self):
        self.is_alive = False
        self.deactivate()
        self.bus._close_service(self)
    
    @synchronized_on("bus.lock")
    def _i_details_function(self):
        details = {}   
        details["id"] = self.id         
        details["active"] = self.active
        details["doc"] = self.doc
        details["info"] = self.info
        functions = {}
        events = {}
        objects = {}
        details["functions"] = functions
        details["events"] = events
        details["objects"] = objects
        for name, function in self.functions.items():
            functions[name] = {"name": name, "info": function}
        for name, event in self.events.items():
            events[name] = {"name": name, "info": event}
        for name, object in self.objects.items():
            objects[name] = {"name": name, "info": object}
        return details








# New classes that will replace most of the above logic soon

class ServiceProvider(object):
    """
    An abstract class specifying the interface that services must expose in
    order for Autobus to be able to publish them on the network.
    """
    __metaclass__ = ABCMeta
    
    @abstractmethod
    def __autobus_call__(self, name, args):
        """
        Calls the function with the specified name using the specified args,
        which will be some sort of iterable (usually a list). This method
        should return the return value of the function in question.
        
        This method may be called with names of functions that do not exist;
        an autobus2.exceptions.NoSuchFunctionException should be raised in such
        a case.
        """
    
    def __autobus_policy__(self, name):
        """
        Requests the calling policy that should be used for the specified
        function. This is one of the constants SYNC, THREAD, or ASYNC defined
        in autobus2.constants. If the function does not exist, any of these may
        be returned; SYNC is the usual one to return in such a case, but THREAD
        may also be used. Using ASYNC in such a case will suppress the "That
        function does not exist" error that would otherwise result.
        
        The default implementation returns THREAD in all cases.
        
        Note that I might do away with the notion of calling policies at some
        point, and just have THREAD be the default policy.
        """
    
    @abstractmethod
    def __autobus_listen__(self, listener):
        """
        Adds a new listener to this ServiceProvider. Listeners will be
        explained in more detail later, but have a look at
        technotes/autobus2-restructure.txt for more information.
        """
    
    @abstractmethod
    def __autobus_unlisten__(self, listener):
        """
        Removes a listener previously added with __autobus_listen__.
        """



















































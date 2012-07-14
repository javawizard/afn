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
                    self.service.object_values.unwatch(name, self.watched_object_changed)
                for name in self.listened_events:
                    self.service.event_listeners.unlisten(name, self.listened_event_fired)
    
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
            result = self.service.provider.__autobus_call__(name, args)
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
            self.service.object_values.watch(name, self.watched_object_changed)
    
    def process_listen(self, message):
        with self.bus.lock:
            name = message["name"]
            # Check to make sure we're not already listening for this event
            if name in self.listened_events:
                self.send_error(message, "You're already listening for that event.")
                return
            # Add the event to our internal list of events we're listening for
            self.listened_events.add(name)
            # Send a response. Unlike object watches, we don't need to include
            # anything in the response. We probably don't even need to include
            # the name, but why not.
            self.send(messaging.create_response(message, name=name))
            # Add ourselves to the underlying service's event table to be
            # notified when the event is actually fired by the service
            self.service.event_listeners.listen(name, self.listened_event_fired)
    
    def process_unwatch(self, message):
        with self.bus.lock:
            name = message["name"]
            # Check to make sure we're actually watching this object
            if name not in self.watched_objects:
                self.send_error(message, "You're not watching that object.")
                return
            # Remove our listener on our service's property table, which will
            # cause an object change to be issued to the client, changing the
            # value to None. This should be fine.
            self.service.object_values.unwatch(name, self.watched_object_changed)
            # And now remove the object from our list of watched objects.
            self.watched_objects.remove(name)
    
    def process_unlisten(self, message):
        with self.bus.lock:
            name = message["name"]
            # Check to make sure we're actually listening for this event
            if name not in self.listened_events:
                self.send_error(message, "You're not listening for that event.")
                return
            # Remove our listener on our service's event table
            self.service.event_listeners.unlisten(name, self.listened_event_fired)
            # And now remove the event from our list of listened events.
            self.listened_events.remove(name)
    
    def watched_object_changed(self, name, old, new):
        """
        Called when an object being watched by this connection changes. This
        method is added to the object_values property table of the LocalService
        to which this connection is bound. The property table will take care of
        calling this when the object appears or disappears or when we start
        watching or stop watching the object.
        """
        self.send(messaging.create_command("changed", True, name=name, value=new))
    
    def listened_event_fired(self, name, args):
        """
        Called when an event being listened for by this connection fires. This
        method is added to the event_listeners event table of the LocalService
        to which this connection is bound. LocalService takes care of notifying
        event_listeners whenever the event fires.
        """
        self.send(messaging.create_command("fired", True, name=name, args=args))
    
    def process_ping(self, message):
        self.send(messaging.create_response(message))


class LocalService(common.AutoClose):
    """
    A service created locally and published to remote clients. This is the
    class of objects returned from bus.create_service(). You'll typically want
    to call bus.create_service() instead of constructing instances of this
    class yourself.
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
        # TODO: The notion of a service being active has been done away with.
        # Make sure this isn't used anywhere else, then remove it.
        self.active = True
        # True if the service is alive, False if it's been closed
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
        # Register ourselves as a listener on the provider. Note that this
        # step must be done last as it will cause things to be added to
        # self.objects etc if the provider in question already has things
        # provided on it.
        provider.__autobus_listen__(self.provider_event)
    
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
    def close(self):
        self.is_alive = False
        # FIXME: Are we closing remote connections to this service anywhere?
        # UPDATE: We're closing all connections in bus.close(), but it doesn't
        # look like we close connections to a service when the service itself
        # is closed. This needs to be fixed.
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



























































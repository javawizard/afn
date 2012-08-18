[This is the readme for the entire AFN project. See also ../README.txt.]

The AFN Project is a collection of libraries and tools initially created by Alexander Boyd and with contributions from others.

It could be considered Alex's "Labs" project, the place where Alex develops various things he's not sure will ever be big enough or in wide-enough use to warrant having their own project. (Once something sees wide enough use, it gets its own project.)

The authoritative version of the AFN repository can be found at http://hg.opengroove.org/afn. It's currently versioned using Mercurial; it used to be versioned using Git until June 12, 2012. (It was versioned using Subversion even earlier; look through the project history for the date when the transition to Git was made.)

The structure of the AFN repository is somewhat disorganized. All of the folders in this folder are original to the AFN repository, with the exception of the projects folder. This folder, and its revision history, were originally the jwutils project, which was Alex's original collection of libraries and tools. He later created the AFN project to hold primarily home automation software, and as its scope expanded he decided to merge jwutils into afn. The version history of jwutils is preserved in the AFN repository.

A brief summary of some of the projects provided as part of AFN:
    
    Filer is an in-development distributed version control system. Have a look at the filer folder for more information.
    
    Sixjet is a project to create a music-synchronized water-jet fountain. 

    Autobus 2 is a decentralized message bus/cross-language RPC system, with service discovery built-in. It allows applications to provide functions that can be called, events that can be listened for, and objects that can be observed, and it allows other applications on the network to automatically discover other applications providing functions. There is no central server involved. For more information, see afn/python/src/autobus2/__init__.py.
    
        Autosend2 is a command-line tool that allows Autobus functions to be called, events to be watched, objects to be observed, services (groups of functions, events, and objects) to be discovered, and even functions to be published.
        
        Autobus 1 is the now-obsolete original incarnation of Autobus. It operated using a client-server model; an Autobus server was started, and clients wishing to provide functions and clients wishing to call other clients' functions would connect to the server. Autobus 2 was a rewrite of Autobus 1 to remove the server component and add service discovery. For more information on Autobus 1, see afn/python/src/autobus (the server) and afn/python/src/libautobus (the Python client library).
    
    JPath is a JSON query language and associated query engine. JPath is inspired by XQuery but designed to query JSON instead. True to one of XQuery's guiding philosophies, JPath is a superset (note superset, not subset) of JSON. JPath is also a superset of XML; any XML constructs are expressions equivalent to their JsonML-encoded counterparts.
    
        JPath Database is an in-development JSON database management system and application server that uses JSON to store and represent data. Queries and web pages are written in JPath, and data is stored as JSON. As development of JPath Database progresses, I'll post more information here. (As of February 2012, development is currently on hold due to Alex not having enough time; if anyone else wants to pick this up, let Alex know, alex at opengroove dot org.)
    
    RTK, the Remote ToolKit, is a windowing toolkit that functions using a client-server model: applications are servers, and clients can connect to those applications with RTK viewers. Each client views their own instance of the application, but all instances run in the same server-side executable so they can communicate easily with each other. It's quite similar to Network Transparent Widgets (http://ntw.sourceforge.net), but the widget set can be extended on top of the existing RTK protocol.
    
        librtk is a Python library for writing RTK applications. It's currently the only RTK server library; when I have time, I'll write some RTK libraries for other languages.
        
        librtkclient is a Python library for writing an RTK client. It's agnostic as to the actual widget toolkit used to display the application. RTKinter and Ravenswood both use it to connect to an RTK application.
        
        RTKinter is an RTK viewer that uses Tkinter to show the application it's viewing. It's currently the most feature-complete RTK client, and the only one that works properly on Mac and Windows. It also functions as a library that allows existing Tkinter applications to embed an RTK viewer.
        
        Ravenswood is an RTK viewer that uses GTK+ to show the application it's viewing. It makes the application look like a native Linux application, but currently it only works correctly on Linux. I hope to continue development of this viewer and have it eventually eplace RTKinter as the main viewer. Like RTKinter, it also functions as a library that allows existing GTK+ applications to embed an RTK viewer.
    
    SVNWeb is a program to serve static files from a certain path in a subversion repository over HTTP. It's a really lightweight alternative to something such as mod_dav_svn when you want to serve, for example, a website from a subversion repository. It include a MediaWiki page renderer for pages with the subversion property svnweb:display set to mediawiki. [NOTE: SVNWeb isn't under active development anymore; I used to use it to serve my website, www.opengroove.org, but I've since switched to Git. I'll most likely write a suitable git replacement soon.]
    
    monitord is a simple program to monitor a computer's CPU, memory, disk, and network usage and publish it via Autobus. It uses libstatmonitor to read these statistics from the machine itself. JZBot (see below) includes a plugin for letting users view statistics published by monitord from an IRC channel.
    
    libstatmonitor is a library that can read information about CPU, memory, disk, and network usage from /proc and ifconfig (with the result that it only works on Linux at the moment; I plan on adding Windows support at some point). monitord uses this to read the information that it publishes via Autobus.
    
    KeyEditor is a program for generating XKB keyboard layouts. It shows a virtual keyboard with text boxes for entering the character that each key should output. It has support for eight characters per key; each for the four levels (no modifiers, shift, altgr, and altgr+shift), and two separate groups (so that, for example, the right control key could be used to switch between the two sets of four levels).
    
    JZBot is an IRC (and XMPP, BZFlag, and Email) bot. It allows commands to be written simply by sending it messages. It provides a scripting language, Fact, which allows commands to be written concisely enough to fit in a single IRC message. It also allows plugins to be written in Java and Python.
    
    SuperTunnel is a TCP-over-HTTP tunneling application. It allows TCP traffic to be encapsulated in HTTP, which allows, for example, one to make arbitrary connections through a firewall that only allows HTTP requests. SuperTunnel is quite similar to GNU HTTPTunnel, but with a few differences that allow it to traverse some firewalls that HTTPTunnel is unable to.
    
    json.py is a command-line tool (written in Python) that allows for reading and manipulating JSON data from the command line. It's the sort of thing you might use when writing a shell script that needs to read and write JSON data. It can also manipulate Apple p-list files, and in this regard, it complements Mac OS X's defaults command by allowing nested paths to be modified as well. (The defaults command only allows modifying entries one level deep.)

There are some others (such as speakd, record.py, saytime, timerd, TrayTimer, and doorbell.py) that I haven't yet documented, and there are others (such as Sixjet) that will be merged into AFN soon.

(For those curious about the origins of the name "AFN", it's an acronym for Attic Full of Nerds. This, in turn, is derived from a quote from Artemis Fowl: The Arctic Incident, where Jerbal Argon mentions that every fairy that has a "crazy old uncle in the attic" can now have them treated in the lap of luxury (Argon's own hospital), and from the fact that Alex's family is predominantly made up of nerds, and AFN started out as a project providing home automation tools for Alex's house.)

    

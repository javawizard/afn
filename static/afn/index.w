
AFN is an umbrella project containing a bunch of projects, most of which were started by Alexander Boyd (the owner of opengroove.org). It's the successor to [http://jwutils.googlecode.com JWUtils] that I'm hosting on my own servers.

I intend to make AFN open-source soon. All of the code is released under the LGPL; the repository just isn't publicly-accessible yet, but if you want access, feel free to send an email to alex<!-- comment -->@opengroove.<!-- comment -->org.

I'll get a detailed list of projects contained within the AFN repository up soon. Here's a brief summary of some of them:

* '''JPath''' is a JSON query language and associated query engine. JPath is inspired by XQuery but designed to query JSON instead. True to one of XQuery's guiding philosophies, JPath is a superset (note superset, not subset) of JSON. Right now, JPath is just a query engine, where you hand it JSON data and a query and it gives you back the results. At some point I hope to write a full JSON database on top of it, and ideally a JSON application server similar to MarkLogic (an application server that uses XQuery and XML).
* '''Zelden''' is an in-progress multi-protocol chat client. Once complete, it will be similar to Pidgin, but it uses Quassel's idea of multiple clients connected to a single core. This means that users can run Zelden Server on a machine that's always online, and it will connect to their chat networks. The user can then connect with (potentially multiple) instances of Zelden Client (perhaps running on different computers), see all messages sent by any of their clients, and send and receive messages. I'm working on a Zelden desktop client and a Zelden client for Android devices.
* '''Autobus''' is a message bus. It's similar to D-Bus, but with an emphasis on network connectivity. Bindings are available for Python and Java. Since Autobus's protocol is based on JSON, writing bindings for other languages is quite simple.
** '''libautobus''' is a client library for Autobus written in Python. I've also written a similar library for Java.
** '''Autosend''' is a command-line client for Autobus. It connects as a normal Autobus client and allows the user to call functions registered by other clients, listen for event notifications, and watch Autobus objects for changes made by the client that publishes them.
* '''Speak''' is a system for recording short audio files and playing lists of these files in a row. It connects as an Autobus client and publishes functions for instructing it what lists of files to play. I've recorded myself speaking several time-related phrases such as "o'clock", "30", etc, which I've included with Speak. In combination with Saytime, this allows Speak to function as a speaking clock.
* '''record.py''' is a script for recording a user saying a set of spoken phrases, each of which will then be saved to an individual file. record.py takes a list of phrases to ask the user to say, takes care of automatically listening for when they start saying the phrase and when they stop saying the phrase, and saves each phrase to its own file automatically.
* '''Saytime''' is a small Python program that connects to an Autobus server with a Speak instance connected to it. Every fifteen minutes from 8:00 AM to 8:00 PM (this can be easily configured by changing some numbers in saytime.py), saytime instructs Speak to announce the time (for example, "The time is twelve fifteen pm", and on the hour it also announces the date like "Today is Saturday, April sixteen. The time is twelve o'clock pm"). Saytime also registers functions to allow other Autobus clients to say the current date, current time, or both.
* '''timerd''' is a daemon that connects to Autobus and provides a mechanism to create timers (both count-down timers and count-up timers). You can tell it to start, pause, reset, change the time on, change the direction of (counting up or counting down), or delete a timer. Individual timers can also be flagged for announcement; timerd will announce over Speak when such timers go off. Invididual timers can also be set to have timerd announce their current time at regular intervals over Speak.
* '''TrayTimer''' is a timerd client. It shows each timer in a tab and allows for configuring every setting that timerd makes available. It's written in Java at the moment; I'm in the process of re-writing it as an RTK application.
* '''RTK''', the Remote ToolKit, is a windowing toolkit that functions using a client-server model: applications are servers, and clients can connect to those applications with RTK viewers. Each client views their own instance of the application, but all instances run in the same server-side executable so they can communicate easily with each other. It's quite similar to [http://ntw.sourceforge.net Network Transparent Widgets], but the widget set can be extended on top of the existing RTK protocol.
** '''librtk''' is a Python library for writing RTK applications. It's currently the only RTK server library; when I have time, I'll write some RTK libraries for other languages.
** '''librtkclient''' is a Python library for writing an RTK client. It's agnostic as to the actual widget toolkit used to display the application. RTKinter and Ravenswood both use it to connect to an RTK application.
** '''RTKinter''' is an RTK viewer that uses Tkinter to show the application it's viewing. It's currently the most feature-complete RTK client, and the only one that works properly on Mac and Windows. It also functions as a library that allows existing Tkinter applications to embed an RTK viewer.
** '''Ravenswood''' is an RTK viewer that uses GTK+ to show the application it's viewing. It makes the application look like a native Linux application, but currently it only works correctly on Linux. I hope to continue development of this viewer and have it eventually eplace RTKinter as the main viewer. Like RTKinter, it also functions as a library that allows existing GTK+ applications to embed an RTK viewer.
* '''SVNWeb''' is a program to serve static files from a certain path in a subversion repository over HTTP. It's a really lightweight alternative to something such as mod_dav_svn when you want to serve, for example, a website from a subversion repository. It include a MediaWiki page renderer for pages with the subversion property svnweb:display set to mediawiki. Everything under www.opengroove.org/static (including this page) is served from an SVNWeb instance. This page, in particular, uses the MediaWiki engine to display itself.
* '''monitord''' is a simple program to monitor a computer's CPU, memory, disk, and network usage and publish it as an Autobus object. It uses libstatmonitor to read these statistics from the machine itself. [http://jzbot.googlecode.com JZBot] includes a plugin for letting users view statistics published by monitord from an IRC channel.
* '''libstatmonitor''' is a library to read information about CPU, memory, disk, and network usage from /proc and ifconfig (with the result that it only works on Linux at the moment; I plan on adding Windows support at some point). monitord uses this to read the information that it then sends to Autobus.
* '''libfact''' is a Fact interpreter written in Python. Fact is the language that JZBot factoids are written in; at some point I plan on re-writing JZBot in Python, and it will use libfact to run factoids.
* '''activehomed''' is a daemon that connects to Autobus and allows Autobus clients to send commands to an X10 ActiveHome home automation device. I use it to allow me to control all of the lights in my house.
* '''KeyEditor''' is a program for generating XKB keyboard layouts. It shows a virtual keyboard with text boxes for entering the character that each key should output. It has support for eight characters per key; each for the four levels (no modifiers, shift, altgr, and altgr+shift), and two separate groups (so that, for example, the right control key could be used to switch between the two sets of four levels).































 
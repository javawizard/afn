
AFN is an umbrella project containing a bunch of projects, most of which were started by Alexander Boyd (the owner of opengroove.org). 

I intend to make AFN open-source soon. All of the code is released under the LGPL; the repository just isn't publicly-accessible yet, but if you want access, feel free to send an email to alex<!-- comment -->@opengroove.<!-- comment -->org.

I'll get a detailed list of projects contained within the AFN repository up soon. Here's a brief summary of some of them:

* '''Autobus''' is a message bus. It's similar to D-Bus, but with an emphasis on network connectivity. Bindings are available for Python and Java. Since Autobus's protocol is based on JSON, writing bindings for other languages is quite simple.
** '''Autosend''' is a command-line client for Autobus. It connects as a normal Autobus client and allows the user to call functions registered by other clients, listen for event notifications, and watch Autobus objects for changes made by the client that publishes them.
* '''Speak''' is a system for recording short audio files and playing lists of these files in a row. It connects as an Autobus client and publishes functions for instructing it what lists of files to play. I've recorded myself speaking several time-related phrases such as "o'clock", "30", etc, which I've included with Speak. In combination with Saytime, this allows Speak to function as a speaking clock.
* '''record.py''' is a script for recording a user saying a set of spoken phrases, each of which will then be saved to an individual file. record.py takes a list of phrases to ask the user to say, takes care of automatically listening for when they start saying the phrase and when they stop saying the phrase, and saves each phrase to its own file automatically.
* '''Saytime''' is a small Python program that connects to an Autobus server with a Speak instance connected to it. Every fifteen minutes from 8:00 AM to 8:00 PM (this can be easily configured by changing some numbers in saytime.py), saytime instructs Speak to announce the time (for example, "The time is twelve fifteen pm", and on the hour it also announces the date like "Today is Saturday, April sixteen. The time is twelve o'clock pm"). Saytime also registers functions to allow other Autobus clients to say the current date, current time, or both.
* '''RTK''', the Remote ToolKit, is a windowing toolkit that functions using a client-server model: applications are servers, and clients can connect to those applications with RTK viewers. Each client views their own instance of the application, but all instances run in the same server-side executable so they can communicate easily with each other. It's quite similar to [http://ntw.sourceforge.net Network Transparent Widgets], but the widget set can be extended on top of the existing RTK protocol.
** '''librtk''' is a Python library for writing RTK applications. It's currently the only RTK server library; when I have time, I'll write some RTK libraries for other languages.
** '''librtkclient''' is a Python library for writing an RTK client. It's agnostic as to the actual widget toolkit used to display the application. RTKinter and Ravenswood both use it to connect to an RTK application.
** '''RTKinter''' is an RTK viewer that uses Tkinter to show the application it's viewing. It's currently the most feature-complete RTK client, and the only one that works properly on Mac and Windows. It also functions as a library that allows existing Tkinter applications to embed an RTK viewer.
** '''Ravenswood''' is an RTK viewer that uses GTK+ to show the application it's viewing. It makes the application look like a native Linux application, but currently it only works correctly on Linux. I hope to continue development of this viewer and have it eventually eplace RTKinter as the main viewer. Like RTKinter, it also functions as a library that allows existing GTK+ applications to embed an RTK viewer.

 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
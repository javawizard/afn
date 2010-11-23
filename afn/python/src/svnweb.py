
"""
This module starts up an HTTP server on the port specified as the first
command-line parameter serving from the subversion path specified as the
second command-line parameter. All files in the specified folder and all
folders underneath it will be served back. The svn:mime-type property is
correctly interpreted and used as the mime type sent back; if it's not present,
a mime-type is chosen based on the file's extension. If a folder is visited,
its index.html file, if any, is sent back. Otherwise, an error is sent.
Listing of files and folders is not yet supported.

If svnweb should authenticate with the subversion server, the username and
password should be specified as the third and fourth command-line parameters,
respectively.
"""

import sys
from BaseHTTPServer import BaseHTTPRequestHandler, HTTPServer
from pysvn import Client
from traceback import print_exc
import mimetypes

mimetypes.init()

class HTTPHandler(BaseHTTPRequestHandler):
    def do_GET(self):
        path = self.path
        if ".." in path:
            raise Exception("Path can't contain two dots in a row")
        print "Initial path: " + path
        if path == "":
            path = "/"
        if path[0] != "/":
            raise Exception("Path doesn't start with a forward slash")
        path = root_path + path
        print "Getting result"
        try:
            result = client.cat(path)
        except:
            print "Result not there; trying index.html under it"
            if path[-1] != "/":
                path += "/"
            path += "index.html"
            try:
                result = client.cat(path)
            except:
                print "Couldn't find index.html either"
                print_exc()
                result = None
        if result == None:
            self.send_response(404)
            self.send_header("Content-Type", "text/html")
            self.no_cache()
            self.end_headers()
            self.wfile.write("<html><body>The specified page can't be "
                    "found.</body></html>")
            return
        print "Got it! Propgetting..."
        # We have the file. Now we send it to the client. If svn:mime-type
        # was specified, we'll use that. Otherwise, we'll use the mimetypes
        # module to guess the file's mime type.
        try:
            mime_type = client.propget("svn:mime-type", path).values[0]
        except:
            print "Couldn't propget, looking up dynamically"
            mime_type, _ = mimetypes.guess_type(path)
        print "Mime type is " + mime_type
        self.send_response(200)
        print "Response sent"
        if mime_type != None:
            print "Sending mime type"
            self.send_header("Content-Type", mime_type)
        print "Sending no-cache headers"
        self.no_cache()
        print "Ending headers"
        self.end_headers()
        print "Writing..."
        self.wfile.write(result)
        print "Output sent!"
        # That's it!
    
    def no_cache(self):
        self.send_header("Pragma", "no-cache")
        self.send_header("Cache-Control", "no-cache")
        self.send_header("Expires", "Mon, 22 Nov 2010 01:00:00 GMT")

def get_login(*args):
    return True, username, password, False

def main():
    global port
    global root_path
    global username
    global password
    global client
    if len(sys.argv) <= 2:
        print "You need to specify the port to serve on and the URL under"
        print "which files will be looked up, respectively."
        sys.exit()
    client = Client()
    client.callback_get_login = get_login
    port = int(sys.argv[1])
    root_path = sys.argv[2]
    if root_path[-1] == "/":
        root_path = root_path[:-1]
    if len(sys.argv) <= 4:
        username = None
        password = None
    else:
        username = sys.argv[3]
        password = sys.argv[4]
    print "Port: " + str(port)
    print "Root path: " + root_path
    print "Username: " + str(username)
    print "Password: " + str(("*" * len(password)) if 
            isinstance(password, basestring) else password)
    print
    print "svnweb has successfully started up."
    HTTPServer(("", port), HTTPHandler).serve_forever()























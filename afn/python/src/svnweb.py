
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

This module was designed to have very high performance. timing wget on a page
2KB in size served from svnweb reports 144 milliseconds taken by wget to
download and save the page. Note, though, that this was with svnweb and the
backing subversion repository on separate machines but the same local network;
serving from a remote subversion repository may be quite a bit slower.

To those of you that might ask why I'm writing this when mod_dav_svn already
provides read access to a repository, the #1 reason is that this script is
considerably easier to run than a full-blown apache install with mod_dav_svn
running inside it. Especially since I run my website, www.opengroove.org,
using Cherokee, which supports reverse-proxying (if you go to
www.opengroove.org, you'll get redirected to www.opengroove.org/static/,
which Cherokee proxies over to an instance of svnweb) but not mod_dav_svn.
"""

import sys
from BaseHTTPServer import BaseHTTPRequestHandler, HTTPServer
from pysvn import Client
from traceback import print_exc
import mimetypes
from mwlib.uparser import simpleparse
from mwlib.xhtmlwriter import MWXHTMLWriter
from xml.etree import ElementTree

mimetypes.init()

error_response = """
<html><body>
There doesn't appear to be a page with that name. Sorry about that.
</html><body>
"""

class HTTPHandler(BaseHTTPRequestHandler):
    def do_GET(self):
        path = self.path
        if ".." in path:
            raise Exception("Path can't contain two dots in a row")
        print "Initial path: " + path
        if path == "":
            path = "/"
        if path[0] != "/":
            raise Exception("Path doesn't start with a forward slash. It's "
                    + path)
        path = root_path + path
        print "Getting result for path " + path
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
            self.wfile.write(error_response)
            return
        print "Got it! Propgetting..."
        # We have the file. Now we go figure out if we're supposed to use an
        # alternate displayer.
        try:
            display_type = client.propget("svnweb:display", path).values()[0]
        except:
            display_type = None
        print "svnweb:display is " + str(display_type)
        if display_type == "mediawiki":
            print "Forwarding to mediawiki renderer"
            mime_type, result = display_mediawiki(self, path, result)
        else:
            print "No known renderer set with svnweb:display, displaying directly"
            try:
                mime_type = client.propget("svn:mime-type", path).values()[0]
            except:
                print "Couldn't propget, looking up dynamically"
                mime_type, _ = mimetypes.guess_type(path)
                if mime_type is None:
                    print "Dynamic lookup didn't find anything. Using text/plain."
                    mime_type = "text/plain"
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
    
    def address_string(self):
        # This stops the server from looking up the client's hostname,
        # which was hugely slowing things down when I tested it out. We'll
        # just return their IP address instead.
        return str(self.client_address[0])

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


def display_mediawiki(request, path, text):
    article = simpleparse(text)
    article.caption = path.rpartition("/")[2]
    if "." in article.caption:
        article.caption = article.caption.rpartition(".")[0]
    caption = article.caption
    element = MWXHTMLWriter().write(article)
    result = ElementTree.tostring(element)
    result = """
    <html><head><title>%s</title></head><body>
    """ % caption + result + """
    </body></html>
    """
    return "text/html", result






















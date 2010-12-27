
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

index_names = ["index.html", "index.xhtml", "index.w"]

def make_index_paths(path):
    return [(path if path[-1:] == "/" else path + "/") 
            + index for index in index_names]

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
        check_paths = [path] + make_index_paths(path)
        print "Trying possible paths"
        result = None
        for test_path in check_paths:
            print "Getting result for path " + test_path
            try:
                result = client.cat(test_path)
                path = test_path
                break
            except:
                print "No such file."
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
    try:
        # Try to figure out a title by looking up svn properties
        caption = client.propget("svnweb:mediawiki:title", path).values()[0]
    except:
        caption = None
    if caption is None: # No explicit name, so we'll derive it from the filename
        caption = path.rpartition("/")[2]
        if "." in caption:
            caption = caption.rpartition(".")[0]
    article.caption = caption
    writer = MWXHTMLWriter()
    writer.xwriteStyle = mediawiki_xwriteStyle
    writer.xwriteArticleLink = mediawiki_xwriteArticleLink
    element = writer.write(article)
    result = ElementTree.tostring(element)
    result = """
    <html><head><title>%s</title>
    
    <style type="text/css">
    
    body {font-size: 13px; font-family: sans-serif; padding: 30px;
          background-color: #f9f9f9}
    
    .content {border: 1px solid #aaa; padding: 10px; padding-top: 0px;
              background-color: white}
    
    .content div[class~="mwx.paragraph"] {margin-bottom: 12px}
    
    .content > div > h1 {font-size: 24px; width: 100%%;
                         border-bottom: 1px solid #aaa; margin-bottom: 12px;
                         margin-top: 8px}
    
    .content > div > div > h2 {font-size: 19px; width: 100%%; 
                           border-bottom: 1px solid #aaa;
                           margin-bottom: 8px}
    .content > div > div > div > h2 {font-size: 17px; margin-bottom: 7px;
                                     margin-top: 18px}
    .content > div > div > div > div > h2 {font-size: 15px; margin-bottom: 4px}
    .content > div > div > div > div > div > h2 {font-size: 13px; margin-bottom: 4px}
    
    span[class~="mwx.svnweb.bold"] {font-weight: bold}
    span[class~="mwx.svnweb.italic"] {font-style: italic}
    
    </style>
    
    </head><body>
    <div class="content">
    """ % caption + result + """
    </div></body></html>
    """
    return "text/html", result


def mediawiki_xwriteStyle(style):
    """
    A custom writer function that writes style elements. This is the part that
    generates the html from ''' and '' in mediawiki pages.
    """
    e = ElementTree.Element("span")
    caption_map = {"'''": "mwx.svnweb.bold", "''": "mwx.svnweb.italic"}
    try:
        e.set("class", caption_map[style.caption])
    except KeyError:
        raise Exception("No style information for " + str(style.caption))
    return e

def mediawiki_xwriteArticleLink(obj):
    a = ElementTree.Element("a", href=obj.url or obj.target or "#")
    a.set("class", "mwx.link.article")
    if not obj.children:
        a.text = obj.target
    return a



















from BaseHTTPServer import BaseHTTPRequestHandler, HTTPServer
from pysvn import Client
from traceback import print_exc, format_exc
import sys
from urlparse import parse_qs
from urllib import quote_plus
from jpath4.query import interpreter
import json

error_response = """
<html><body>That page doesn't exist.</body></html>
"""

# List of name, query, data
jpath_examples = [
    ["Hello world (the short way)", '"Hello, world!"', "{}"],
    ["Hello world (the long way)", "a/b/c", '{"a": {"b": {"c": "Hello, world!"}}}'],
    ["Predicate 1", "*[p=true]/a", '[{"a": "b", "p": true}, {"a": "c", "p": false}, {"a": "d", "p": true}]'],
    ["Predicate 1 with comments", 
            """
(: The asterisk gets all of the items in the current list :)
*
(: The brackets are known as a predicate (brackets after an expression are a predicate; brackets /as/
an expression create a new list). They evaluate what's inside them for every item and filter out items
for which the expression was not true. :)
[
(: So now we're going to check whether the current value, which is presumably a dictionary, has a key
"p" whose value is the boolean true. :)
p=true
(: And then of course the bracket to close the predicate :)
]
(: The slash runs the expression on the right against all of the items on the left, and the result is
all of the items on the right combined into a single sequence :)
/
(: So now we just get the value of the key "a". :)
a
(: And that's it! As you've probably worked out, this query will get the value of the key "a" in every
dict in the list handed it where there's a key "p" whose value is true. :)
""", 
            '[{"a": "b", "p": true}, {"a": "c", "p": false}, {"a": "d", "p": true}]'],
    ["Predicate 2 (a practical use of Predicate 1)", '*[age>=21]/name', """
[
    {
        "name": "Alice",
        "age": 19,
        "flavors": ["phish food", "cookie dough", "cherry garcia"]
    },
    {
        "name": "Bob",
        "age": 22,
        "flavors": ["chocolate", "cookie dough", "vanilla", "cookies n cream"]
    },
    {
        "name": "Carol",
        "age": 21
    }
]
"""],
]

pages = {}
def handler(function):
    pages[function.__name__] = function
    return function


class HTTPHandler(BaseHTTPRequestHandler):
    def do_POST(self):
        self.do_GET()
    
    def do_GET(self):
        path, _, query_string = self.path.partition("?")
        params = dict((k, v[0]) for k, v in parse_qs(query_string).iteritems())
        if path == "/":
            path = "/index"
        
        function = pages.get(path[1:])
        if function is None:
            self.send_response(404)
            self.send_header("Content-Type", "text/html")
            self.no_cache()
            self.end_headers()
            self.wfile.write(error_response)
            return
        
        self.send_response(200)
        self.send_header("Content-Type", "text/html")
        self.no_cache()
        self.end_headers()
        try:
            function(params, self.wfile)
        except Exception as e:
            self.wfile.write("EXCEPTION OF TYPE " + str(type(e)) + " AND VALUE " + repr(e) + " OCCURRED: <br/><pre>")
            self.wfile.write(format_exc().replace("&", "&amp;").replace("<", "&lt;"))
            self.wfile.write("</pre>")
    
    def no_cache(self):
        self.send_header("Pragma", "no-cache")
        self.send_header("Cache-Control", "no-cache")
        self.send_header("Expires", "Mon, 22 Nov 2010 01:00:00 GMT")
    
    def address_string(self):
        # This stops the server from looking up the client's hostname,
        # which was hugely slowing things down when I tested it out. We'll
        # just return their IP address instead.
        return str(self.client_address[0])


@handler
def index(params, out):
    query = params.get("query", "")
    data = params.get("data", "")
    sidebar = "<br/>".join(
            '<a href="index?query=%s&data=%s">%s</a>' %
            (quote_plus(q), quote_plus(d), e.replace("&", "&amp;").replace("<", "&lt;"))
            for e, q, d in jpath_examples)
    out.write("""
    <html><body>
    <div style="float: right; margin-right: 20px; margin-top: 3px"><b>Examples:</b> <br/>%s</div>
    Type a JPath query and the JSON data to run the query against below, then click <b>Run Query</b>.
    Or click one of the examples to the right, then click <b>Run Query</b> to run the example.<br/><br/>
    <form method="GET" action="run">
    Query:<br/>
    <textarea rows="14" cols="150" name="query">%s</textarea><br/>
    JSON data to run the query against:<br/>
    <textarea rows="10" cols="150" name="data">%s</textarea></br>
    <button type="submit"><b>Run Query</b></button>
    </form>
    </body></html>
    """ % (sidebar,
            query.replace("&", "&amp;").replace("<", "&lt;"),
            data.replace("&", "&amp;").replace("<", "&lt;")))


@handler
def run(params, out):
    if "query" not in params:
        out.write("<html><body>No 'query' query parameter specified</body></html>")
        return
    query = params["query"]
    data_text = params.get("data", "{}")
    data = json.loads(data_text)
    result = interpreter.query(query, data, as_tuple=True)
    formatted_results = [json.dumps(v, indent=4) for v in result]
    
    out.write("""
    <html>
    <head>
    <style type="text/css">
    pre {
        margin: 1px;
    }
    </style>
    </head>
    <body><b>Query results:</b><br/><br/>
    """)
    if len(result) == 0:
        out.write("<i>The query did not produce any results.</i>")
    else:
        out.write("<ol>")
        for f in formatted_results:
            out.write("<li><pre>")
            out.write(f)
            out.write("</pre></li>")
        out.write("</ol>")
    out.write('<br/><a href="index?query=%s&data=%s">Run another query</a></body></html>'
            % (quote_plus(query), quote_plus(data_text)))


def main():
    if len(sys.argv) <= 1:
        print "You need to specify the port to listen on."
        sys.exit()
    port = int(sys.argv[1])
    print "webquery is now listening for HTTP requests on port " + str(port)
    HTTPServer(("", port), HTTPHandler).serve_forever()


if __name__ == "__main__":
    main()




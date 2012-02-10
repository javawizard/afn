
from BeautifulSoup import BeautifulSoup
import urllib
import urllib2

def main():
    print "Testing out the University Federal Credit Union client..."


class Session(object):
    # Thanks to http://stackoverflow.com/a/4836113/992720 for the general idea
    # of how to track cookies and redirects
    def __init__(self, username, password, login=True):
        cookies = urllib2.HTTPCookieProcessor()
        redirector = urllib2.HTTPRedirectHandler()
        self._opener = urllib2.build_opener(redirector, cookies)
        if login:
            self.login()
    
    def _get(self, url, parameters=None):
        return self._opener.open(url + ("?" + urllib.urlencode(parameters) if parameters else "")).read()
    
    def _post(self, url, parameters):
        return self._opener.open(url, urllib.urlencode(parameters)).read()

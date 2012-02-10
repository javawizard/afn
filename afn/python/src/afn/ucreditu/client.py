
from BeautifulSoup import BeautifulSoup
import urllib
import urllib2
from getpass import getpass
import re
from decimal import Decimal

def main():
    print "Testing out the University Federal Credit Union client..."


class Session(object):
    # Thanks to http://stackoverflow.com/a/4836113/992720 for the general idea
    # of how to track cookies and redirects
    def __init__(self, username, password=None, login=True):
        if password is None:
            password = getpass("Enter your password: ")
        self.username = username
        self.password = password
        cookies = urllib2.HTTPCookieProcessor()
        redirector = urllib2.HTTPRedirectHandler()
        self._opener = urllib2.build_opener(redirector, cookies)
        if login:
            self.login()
    
    def _get(self, url, parameters=None):
        return self._opener.open(url + ("?" + urllib.urlencode(parameters) if parameters else "")).read()
    
    def _post(self, url, parameters):
        return self._opener.open(url, urllib.urlencode(parameters)).read()
    
    def login(self):
        main_page = BeautifulSoup(self._get("https://mobile.ucreditu.com/"))
        login_url = main_page.find("a", {"name": "LogIn"})["href"]
        login_page = BeautifulSoup(self._get("https://mobile.ucreditu.com/" + login_url[1:]))
        login_submit_url = login_page.find("form", {"name": "signin"})["action"]
        summary_page = self._post("https://mobile.ucreditu.com/" + login_submit_url[1:],
                {"userNumber": self.username, "password": self.password, "Login": "Login"})
        try:
            checking_location = summary_page.find("E-Checking")
            available_location = summary_page.find("Available", checking_location)
            close_b_location = summary_page.find("</b>", available_location)
            open_br_location = summary_page.find("<br>", close_b_location)
            available = summary_page[close_b_location+len("</b>"):open_br_location]
            available = Decimal(available)
            self.checking_balance = available
        except Exception as e:
            # TODO: handle this in some manner
            raise
































from selenium import webdriver
import getpass

class Client(object):
    def __init__(self, username, password=None):
        self.username = username
        if password is None:
            password = getpass.getpass()
        self.driver = webdriver.Chrome()
    
    def login(self):

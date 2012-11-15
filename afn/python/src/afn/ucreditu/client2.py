
from selenium import webdriver
import getpass
import time

class Client(object):
    def __init__(self, username, password=None):
        self.username = username
        if password is None:
            password = getpass.getpass()
        self.driver = webdriver.Chrome()
    
    def login(self):
        self.driver.get("http://my.ucreditu.com")
        self.driver.find_element_by_id("UsernameField").send_keys(self.username)
        self.driver.find_element_by_id("SubmitNext").click()
        self.driver.find_element_by_id("bogus")

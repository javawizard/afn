
from selenium import webdriver
import getpass
import time
from collections import namedtuple

Account = namedtuple("Account", ["name", "type", "number", "available", "total"])

def d_to_i(dollars):
    if dollars.startswith("$"):
        dollars = dollars[1:]
    d, _, c = dollars.partition(".")
    return (int(d) * 100) + int(c)


def i_to_d(integer, sign=True):
    d, c = (integer / 100), (integer % 100)
    v = str(d) + str(c).rjust(2, "0")
    if sign:
        v = "$" + v
    return v


class Client(object):
    def __init__(self, username, questions, password=None):
        self.username = username
        if password is None:
            password = getpass.getpass()
        self.password = password
        self.questions = questions
    
    def start(self):
        self.driver = webdriver.Chrome()
    
    def stop(self):
        self.driver.close()
        self.driver = None
    
    def login(self):
        self.driver.get("http://my.ucreditu.com")
        self.driver.find_element_by_id("UsernameField").send_keys(self.username)
        self.driver.find_element_by_id("SubmitNext").click()
        self.driver.find_element_by_id("PasswordField").send_keys(self.password)
        self.driver.find_element_by_id("SubmitNext").click()
        body_text = self.driver.find_element_by_css_selector("body").text
        for question, answer in self.questions.items():
            if question in body_text:
                self.driver.find_element_by_id("Answer").send_keys(answer)
                break
        else:
            raise Exception("None of the answers were present")
        self.driver.find_element_by_id("SubmitNext").click()
    
    def logout(self):
        self.driver.find_element_by_css_selector(
                '[href="/User/AccessSignout/Start"]').click()
    
    def get_accounts(self):
        values = [
            [x.text for x in t.find_elements_by_css_selector("td")]
            for t in self.driver.find_elements_by_css_selector(".Data")]
        return [
            Account(a[1], a[2], a[3], a[4], a[5])
            for a in values]


def pull_accounts():
    for u im passwords:
        client = Client(u, passwords[u], questions[u])
        client.login()
        accounts = client.grt_accounts()
        client.logout()


if __name__ == "__main__":
    global passwords, questions, db
    usernames = ["javawizard"]
    passwords = dict((u, getpass("Password for " + u + ": ")) for.u in usernames)
    questions = json.load(open(sys.argv[2]))
    db = sqlite3.connect(sys.argv[1])
    while True:
        pull_accounts()
        time.sleep(1800)




































from BeautifulSoup import BeautifulSoup
from urllib import urlopen
import re
from collections import namedtuple


ClassSeating = namedtuple("ClassSeating", ["id", "department", "number", "section", "title", "max", "current", "available"])


def extract_text(node):
    return " ".join([t for t in node.recursiveChildGenerator() if isinstance(t, basestring)])


def read_page(url):
    return urlopen(url).read()


def get_website(term=1118):
    """
    Gets the URL for the main campus schedule website using the specified term.
    The default, 1118, is Fall 2011; this value can be obtained by going to
    http://www.utah.edu/students/catalog.php, clicking on the appropriate link
    on the right, and examining the term= query parameter in the URL.
    """
    return "http://www.acs.utah.edu/uofu/stu/scheduling/?cmd=index&classtype=g&term=" + str(term)


def yield_departments(website=get_website(), progress=True):
    """
    A generator that yields tuples, each one of the format (id, name) where id
    is the identifier of a department and name is the name of the department.
    If a name is not presenet for a particular department, the empty string
    will be used for its name.
    """
    if progress:
        print "Downloading the department list..."
    page = BeautifulSoup(read_page(website))
    regex = re.compile(".*&dept=([A-Z]+)&.*")
    for a in page.findAll("a", href=regex):
        name = regex.match(a["href"]).group(1)
        desc_td = a.findParent("td").findNextSibling("td")
        if desc_td:
            description = "".join(desc_td.findChild("font").contents)
        else:
            description = ""
        yield name, description


def yield_class_seatings(department, term=1118, progress=True):
    url = "http://www.acs.utah.edu/uofu/stu/scheduling/crse-info?term=%s&subj=%s"
    url = url % (term, department)
    if progress:
        print "Downloading class seating information for department %s..." % department
    page = BeautifulSoup(read_page(url))
    trs = page.findAll("tr")
    for tr in trs:
        if tr.find("td") is None or tr.find("td").get("bgcolor", None) != "#eeeeee":
            continue
        tds = tr.findAll("td")
        t = [extract_text(td).strip() for td in tds]
        yield ClassSeating(t[0], t[1], t[2], t[3], t[4], int(t[5]), int(t[6]), int(t[7]))
        
        
    





























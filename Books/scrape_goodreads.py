from bs4 import BeautifulSoup
import requests
import pandas as pd
from selenium.webdriver.chrome.options import Options
import json

def get_added_by(url):
	page = requests.get(url)
	soup = BeautifulSoup(page.content, "html.parser")
	#soup2 = BeautifulSoup(page.content, "lxml-xml")

	# path = '/Users/christopherlee/anaconda3/bin/phantomjs'
	# driver = webdriver.Chrome(options=chrome_options)
	# driver = webdriver.PhantomJS(executable_path=path) 
	# driver.get(url)

	scripts = soup.findAll('script')
	try:
		navig = scripts[18].string
	except IndexError as error:
		return(None)
	except Exception as exception:
		return(str(Exception))
	add_string = 'added by <span class=\\"value\\">'
	n = navig.find(add_string)
	added_by = navig[(n + len(add_string)): (n + len(add_string) + 9)]

	return(added_by)



def create_url(id, name):
	formatted_name = name.replace(' ', '_')
	formatted_name = formatted_name.split(':')[0]
	formatted_name = formatted_name.split('(')[0]
	return "https://www.goodreads.com/book/show/" + str(id) + '.' + formatted_name

def apply_added_by(csv_path):
	goodreads_data = pd.read_csv(csv_path)
	urls = goodreads_data.apply(lambda x: create_url(x['Book Id'], x['Title']), axis=1)
	added = []
	for url in urls:
		added.append(get_added_by(url))
	goodreads_data['added_by'] = added
	goodreads_data['numerical_added'] = goodreads_data['added_by'].str.extract('(\d+)')  

	return goodreads_data
from bs4 import BeautifulSoup
import requests
import pandas as pd
import sys

S = requests.Session()

URL = "https://www.google.com/search?q="
def get_search_url(name):
	return URL + name.replace(' ', '+') + '+nationality'

def get_soup(url):
	headers = {
    'User-agent':
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/70.0.3538.102 Safari/537.36 Edge/18.19582"
	}
	page = requests.get(url, headers=headers) 
	soup = BeautifulSoup(page.content, "html.parser")
	return soup

def get_result(soup):
	raw_results = soup.findAll('div', {'data-attrid': 'kc:/people/person:nationality'}) 
	results = []
	for raw in raw_results:
		results.append(raw.text)
	return results


authors = ['Rob Halford', 'Dear Damsels', 'Jeff Goodell', 'Robin Hobb', 'K.X. Song', 'Ilya Kaminsky', 
'Scarlett Thomas', 'Malinda Lo', 'Philip Reeve', 'Peace Adzo Medie', 'Kazuo Ishiguro']
urls = {}
soups = {}
nationalities = {}
for author in authors: 
	urls[author] = get_search_url(author)
	soups[author] = get_soup(urls[author])
	nationalities[author] = get_result(soups[author])
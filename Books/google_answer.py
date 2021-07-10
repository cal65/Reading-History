from bs4 import BeautifulSoup
import requests
import pandas as pd
import numpy as np
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
		children = raw.findChildren('a', recursive = True)
		if len(children) > 1:
			for child in children:
				results.append(child.get('aria-label'))
		else:
			results.append(raw.text)
	return results


authors = ['Rob Halford', 'Dear Damsels', 'Jeff Goodell', 'Robin Hobb', 'K.X. Song', 'Ilya Kaminsky', 
'Scarlett Thomas', 'Malinda Lo', 'Philip Reeve', 'Peace Adzo Medie', 'Kazuo Ishiguro', 'Lemony Snicket', 
'Semezdin Mehmedinović', 'Aung San Suu Kyi']
urls = {}
soups = {}
nationalities = {}
for author in authors: 
	urls[author] = get_search_url(author)
	soups[author] = get_soup(urls[author])
	ns = get_result(soups[author])
	ns = [n.replace('Nationality: ', '') for n in ns if n is not None ]
	ns = [n.split(', ') for n in ns] 
	ns = [n for nats in ns for n in nats]
	nationalities[author] = pd.unique(ns)

def lookup_author_nationality(author):
	url = get_search_url(author)
	soup = get_soup(url)
	answers = get_result(soup)
	answers = [answer.replace('Nationality: ', '') for answer in answers if answer is not None]
	answers = [answer.split(', ') for answer in answers] # sometimes we get "American, British" as a response
	answers = [a for answer in answers for a in answer] # flatten list
	answers = pd.unique(answers)
	return answers

def listlen(data):
    return len(data) if isinstance(data, np.ndarray) else 1

def convert_nats_list_to_df(nats_list):
	max_nat = max([listlen(n) for n in nats_list])
	columns = ['author'] + ['nationality_' + str(i) for i in np.arange(1, max_nat+1)] 
	output_dicts = []
	for nationality in nats_list:
		output_dict = {}
		for i in np.arange(0, max_nat):
			c = 'nationality' + str(i + 1)
			if i < len(nationality):
				output_dict[c] = [nationality[i]]
			else:
				output_dict[c] = [None]
		output_dicts.append(pd.DataFrame(output_dict))
	return pd.concat(output_dicts).reset_index(drop=True)

def append_nationalities(df, author_col='Author'):
	nats_list = df[author_col].apply(lookup_author_nationality)
	nats_df = convert_nats_list_to_df(nats_list)
	return pd.concat([df, nats_df], axis=1)
 
if __name__ == "__main__":
	file_path = sys.argv[1]
	df = pd.read_csv(file_path)
	append_nationalities(df).to_csv(file_path, index=False)
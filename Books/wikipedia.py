from bs4 import BeautifulSoup
import requests
import pandas as pd
import sys

S = requests.Session()

URL = "https://en.wikipedia.org/w/api.php"
PARAMS = {
	"action": "opensearch",
	"namespace": "0",
	"search": "Placeholder",
	"limit": "1",
	"format": "json"
}

def grab_first_result(search_term):
	PARAMS["search"] = search_term
	R = S.get(url=URL, params=PARAMS)
	DATA = R.json()
	try:
		first_result = DATA[-1][0]
	except:
		print('No Wikipedia page found for: ' + search_term)
		return "No result"
	return first_result

def eval_page(url):
	page = requests.get(url)
	soup = BeautifulSoup(page.content, "html.parser")
	paragraphs = soup.findAll("p")
	return paragraphs

def search_paragraph_for_gender(paragraph):
	words = paragraph.split(' ')
	for word in words:
		if word.lower() in ['he', 'his', 'him']:
			return 'male'
		elif word.lower() in ['she', 'her', 'hers']:
			return 'female'
	return None

def search_raw_page_for_gender(paragraphs):
	for paragraph_raw in paragraphs:
		gender = search_paragraph_for_gender(paragraph_raw.text)
		if gender:
			return gender
	return "unknown"

def search_people_for_gender(people):
	genders = []
	for person in people:
		result = grab_first_result(person)
		if result == 'No result':
			genders.append('unknown')
		else:
			paragraphs = eval_page(result)
			genders.append(search_raw_page_for_gender(paragraphs))
	return genders

def evaluate_file(csv, fix_col='gender_fixed', name_col = 'Author'):
	df = pd.read_csv(csv)
	authors = df.loc[pd.isnull(df[fix_col]), name_col]
	genders = search_people_for_gender(authors)
	df.loc[pd.isnull(df[fix_col]), fix_col] = genders
	return df

if __name__ == "__main__":
	file_path = sys.argv[1]
	evaluate_file(file_path).to_csv(file_path, index=False)
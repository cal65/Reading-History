from bs4 import BeautifulSoup
import requests
import pandas as pd
import numpy as np
import json
import random
import time
import sys
import gender_guesser.detector as gender

def get_stats(url, wait=0):
    '''
    Mega block to pull Goodreads website contents using BeautifulSoup
    Extract numerous useful book info from the page
    Returns a dictionary of that extract
    '''
    page = requests.get(url)
    soup = BeautifulSoup(page.content, "html.parser")

    scripts = soup.findAll("script")
    try:
        navig = scripts[18].string
    except IndexError as error:
        return None
    except Exception as exception:
        return str(Exception)
    add_string = 'added by <span class=\\"value\\">'
    n = navig.find(add_string)
    added_by = navig[(n + len(add_string)) : (n + len(add_string) + 9)]

    to_read_string = "<\\/span> to-reads"
    n2 = navig.find(to_read_string)
    to_reads = navig[(n2 - 8) : n2]

    try:
        title = soup.find("h1").text.replace("\n", "")
    except:
        title = None

    try:
        author = soup.find("span", {"itemprop": "name"}).text
    except:
        author = None

    try:
        publish_info = soup.find('div', {'id': 'details'}).findAll('div', {'class': 'row'})[1].text
        publish_info = publish_info.replace("\n", "")
    except:
        publish_info = None

    try:
        language = soup.find('div', {'itemprop': 'inLanguage'}).text
    except:
        language = None
    try:
        rating = soup.find('span', {'itemprop': 'ratingValue'}).text.replace("\n", "")
    except:
        rating = None
    try:
        shelves = soup.findAll('a', {'class': 'actionLinkLite bookPageGenreLink'})  
        shelves = [shelf.text for shelf in shelves]
        shelf1 = shelves[0] if len(shelves) > 0 else ""
        shelf2 = shelves[1] if len(shelves) > 1 else ""
        shelf3 = shelves[2] if len(shelves) > 2 else ""
        shelf4 = shelves[3] if len(shelves) > 3 else ""
    except:
        shelf1 = shelf2 = shelf3 =  shelf4 = None

    try:
        original_title = soup.find('div', {'class': 'infoBoxRowItem'})  
    except:
        original_title = None

    time.sleep(wait)

    return {
        "Added_by": added_by,
        "To_reads": to_reads,
        "Title": title,
        "Author": author,
        "Publish_info": publish_info,
        "Language": language,
        "Rating": rating,
        "Shelf1": shelf1,
        "Shelf2": shelf2,
        "Shelf3": shelf3,
        "Shelf4": shelf4,
        "Original_title": original_title,
    }


def create_url(id, name):
    formatted_name = name.replace(" ", "_")
    formatted_name = formatted_name.split(":")[0]
    formatted_name = formatted_name.split("(")[0]
    return "https://www.goodreads.com/book/show/" + str(id) + "." + formatted_name


def read_goodreads_export(csv_path):
    goodreads_data = pd.read_csv(csv_path)
    urls = goodreads_data.apply(lambda x: create_url(x["Book Id"], x["Title"]), axis=1)

    return urls, goodreads_data


def apply_added_by(urls):
    raw_stats = []
    missing = []
    for i, url in enumerate(urls):
        stat = get_stats(url)
        raw_stats.append(stat)
        missing.append(stat is None)
        if len(missing) > 3 and all(missing[-3:]):
            time.sleep(45)
        if i % 20 == 0:
            print(i)
    #raw_stats = [get_stats(url, i/100) for i, url in enumerate(urls)]
    not_missing = [i for i, m in enumerate(missing) if m == False]
    stats = [stat for stat in raw_stats if stat is not None ]
    goodreads_data = pd.DataFrame(stats)
    goodreads_data['i'] = not_missing
    goodreads_data['Added_by'] = goodreads_data['Added_by'].str.extract("(\d+)") 
    goodreads_data['To_reads'] = goodreads_data['To_reads'].str.extract("(\d+)") 
    goodreads_data['Publish_info'] = goodreads_data['Publish_info'].str.replace('Published', '').str.strip()
    goodreads_data['Publisher'] = goodreads_data['Publish_info'].str.split('by ').apply(lambda x: x[1] if x is not None and len(x)>1 else None)  
    goodreads_data['date_published'] = goodreads_data['Publish_info'].str.split('by ').apply(lambda x: x[0] if x is not None  else None) 
    # gender
    d = gender.Detector()
    goodreads_data['first_name'] = [name[0] if name is not None else "" for name in goodreads_data['Author'].str.split(' ')]   
    goodreads_data['gender'] = [d.get_gender(name) for name in goodreads_data['first_name']] 
    # shelves
    return goodreads_data

def generate_random_urls(max, n, seed):
    random.seed(seed)
    start = 5
    random_samples = random.sample(list(np.arange(start, start + max)), n)
    urls = ["https://www.goodreads.com/book/show/" + str(r) for r in random_samples]

    return urls

if __name__ == "__main__":
    urls = generate_random_urls(int(sys.argv[1]), int(sys.argv[2]), int(sys.argv[3]))
    goodreads_data = apply_added_by(urls)
    try:
        existing = pd.read_csv(sys.argv[-1])
        goodreads_data = pd.concat([existing, goodreads_data], axis=0)
    except:
        pass
    goodreads_data.to_csv(sys.argv[-1], index=False)


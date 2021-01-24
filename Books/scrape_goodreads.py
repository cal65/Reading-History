from bs4 import BeautifulSoup
import requests
import pandas as pd
import numpy as np
import json
import random
import time

def get_stats(url, wait=0):
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

    time.sleep(wait)

    return {
        "added_by": added_by,
        "to_reads": to_reads,
        "title": title,
        "author": author,
        "publish_info": publish_info,
        "language": language,
        "rating": rating,
    }


def create_url(id, name):
    formatted_name = name.replace(" ", "_")
    formatted_name = formatted_name.split(":")[0]
    formatted_name = formatted_name.split("(")[0]
    return "https://www.goodreads.com/book/show/" + str(id) + "." + formatted_name


def read_goodreads_export(csv_path):
    goodreads_data = pd.read_csv(csv_path)
    urls = goodreads_data.apply(lambda x: create_url(x["Book Id"], x["Title"]), axis=1)

    return urls


def apply_added_by(urls):
    raw_stats = []
    missing = []
    for i, url in enumerate(urls):
        stat = get_stats(url)
        raw_stats.append(stat)
        missing.append(stat is None)
        if len(missing) > 3 and all(missing[-3:]):
            time.sleep(45)
        if i % 100 == 0:
            print(i)
    #raw_stats = [get_stats(url, i/100) for i, url in enumerate(urls)]
    #missing = [i for i, stat in enumerate(raw_stats) if stat is None]
    stats = [stat for stat in raw_stats if stat is not None ]
    goodreads_data = pd.DataFrame(stats)
    goodreads_data['added_by'] = goodreads_data['added_by'].str.extract("(\d+)") 
    goodreads_data['to_reads'] = goodreads_data['to_reads'].str.extract("(\d+)") 
    goodreads_data['publish_info'] = goodreads_data['publish_info'].str.replace('Published', '').str.strip()
    goodreads_data['publisher'] = goodreads_data['publish_info'].str.split('by ').apply(lambda x: x[1] if x is not None and len(x)>1 else None)  
    goodreads_data['date_published'] = goodreads_data['publish_info'].str.split('by ').apply(lambda x: x[0] if x is not None  else None) 
    
    return goodreads_data

def generate_random_urls(max, n, seed):
    random.seed(seed)
    random_samples = random.sample(list(np.arange(0, max)), n)
    urls = ["https://www.goodreads.com/book/show/" + str(r) for r in random_samples]

    return urls

if __name__ == "__main__":
    urls = generate_random_urls(40000000, 5000, 1000)
    goodreads_data = apply_added_by(urls)
    goodreads_data.to_csv('export_goodreads.csv', index=False)


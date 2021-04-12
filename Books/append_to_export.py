import scrape_goodreads
import sys
import pandas as pd
import re

def append_scraping(file_path):
	urls, goodreads_data = scrape_goodreads.read_goodreads_export(file_path)
	scraped_df = scrape_goodreads.apply_added_by(urls)
	scraped_df.drop(columns = ['Title', 'Author', 'Publish_info'], inplace=True)
	goodreads_data['i'] = goodreads_data.index
	goodreads_data_merged = pd.merge(goodreads_data, scraped_df, on='i', how='left')
	return goodreads_data_merged

if __name__ == "__main__":
	file_path = sys.argv[1]
	export_path = re.sub('.csv|.xlsx', '_appended.csv', file_path)
	append_scraping(file_path).to_csv(export_path, index=False)
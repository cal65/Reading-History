import scrape_goodreads
import sys
import pandas as pd

def append_scraping(csv_path):
	urls, goodreads_data = scrape_goodreads.read_goodreads_export(csv_path)
	scraped_df = scrape_goodreads.apply_added_by(urls)
	scraped_df.drop(columns = ['Title', 'Author', 'Publish_info'], inplace=True)
	goodreads_data['i'] = goodreads_data.index
	goodreads_data_merged = pd.merge(goodreads_data, scraped_df, on='i', how='left')
	return goodreads_data_merged

if __name__ == "__main__":
	csv_path = sys.argv[1]
	export_path = csv_path.replace('.csv', '_appended.csv')
	append_scraping(csv_path).to_csv(export_path, index=False)
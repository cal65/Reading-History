import scrape_goodreads
import sys
import pandas as pd
import re

def append_scraping(goodreads_data):
	urls = scrape_goodreads.return_urls(goodreads_data)
	scraped_df = scrape_goodreads.apply_added_by(urls)
	scraped_df.drop(columns = ['Title', 'Author', 'Publish_info'], inplace=True)
	goodreads_data['i'] = goodreads_data.index
	goodreads_data_merged = pd.merge(goodreads_data, scraped_df, on='i', how='left')
	return goodreads_data_merged

def apply_append(file_path):
	goodreads_data = scrape_goodreads.read_goodreads_export(file_path)
	return append_scraping(goodreads_data)

def add_to_existing_export(file_path_existing, file_path_new):
	existing = scrape_goodreads.read_goodreads_export(file_path_existing)
	new = scrape_goodreads.read_goodreads_export(file_path_new)
	diff_titles = set(new['Title']).difference(existing['Title']).tolist()
	diff = new[new['Title'].isin(diff_titles)]
	diff_scraped = append_scraping(diff)
	return diff_scraped

def compare_dfs(df1, df2, index_column):
	for t in df2[index_column]:
		df1_sub = df1[df1[index_column] == t]
		df1_sub = df1_sub.head(1) # in case of duplicates, just keep the first match
		df2_sub = df2[df2[index_column] == t]
		common_cols = [c for c in df2_sub.columns if c in df1_sub.columns]
		for c in common_cols:
			if df1_sub.iloc[0][c] != df2_sub.iloc[0][c]:
				df1_sub[c] = df2_sub[c]
				df1.loc[df1[index_column]==t, c] = df2_sub[c]
	return df1

def update_goodreads(df1, df2, index_column):
	df2.columns = [c.replace(' ', '.') for c in df2.columns]
	return compare_dfs(df1, df2, index_column)

if __name__ == "__main__":
	file_path = sys.argv[1]
	export_path = re.sub('.csv|.xlsx', '_appended.csv', file_path)
	apply_append(file_path).to_csv(export_path, index=False)

1. **python append_to_export file.csv** ## This will scrape additional fields from goodreads and append them as new columns to the file, and save a new file called file_appended.csv
2. Within goodreads.R, I read in the data and compare the authors in the data to authors in authors_database.csv. Any authors in the data that are not in the database csv are saved to a new file called new_authors_data.csv.
3. **python google_answer.py new_authors_data.csv**
4. **python choose_nationality.py new_authors_data.csv**
5. **python wikipedia.py new_authors_data.csv**
6. Then row bind this latest csv file with authors_database.csv and save
7. Later functions will read from authors_database and use that to generate graphs
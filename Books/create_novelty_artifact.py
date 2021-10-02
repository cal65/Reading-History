import pandas as pd
authors_db = pd.read_csv('authors_database.csv')
export_db = authors_db[['Author', 'gender_fixed', 'nationality1', 'nationality2', 'Country.Chosen']]
export_db.columns = ['author_name', 'gender', 'nationality1', 'nationality2', 'nationality_chosen']
export_db.to_csv('../Novelty/artifacts/authors_sql_database.csv', index=False) 
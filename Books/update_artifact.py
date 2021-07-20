import pandas as pd
import sys
from google_answer import append_nationalities
from wikipedia import search_person_for_gender
import gender_guesser.detector as gender


def guess_gender(author):
    d = gender.Detector()
    names = author.split(" ")
    first_name = names[0] if names is not None else ""
    gender_return = d.get_gender(first_name)  # this returns male, female, unknown or andy
    if gender_return not in ["male", "female"]:
        gender_return = search_person_for_gender(author)
    return gender_return


def update_artifact(df, author_col="Author", artifact_path="authors_database.csv"):
    df.drop_duplicates(author_col, inplace=True)
    authors_database = pd.read_csv(artifact_path)
    df_new = df[~df[author_col].isin(authors_database[author_col])]
    df_new = append_nationalities(df_new)
    df_new["gender_fixed"] = df_new[author_col].apply(guess_gender)

    return df_new

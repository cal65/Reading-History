import pandas as pd
import sys
from google_answer import lookup_unfound
from wikipedia import search_person_for_gender
import gender_guesser.detector as gender


def guess_gender(author):
    d = gender.Detector()
    first_name = [name[0] if name is not None else "" for name in author.str.split(" ")]
    gender = d.get_gender(first_name)  # this returns male, female, unknown or andy
    if gender not in ["male", "female"]:
        gender = search_person_for_gender(author)
    return gender


def update_artifact(df, author_col="Author", artifact_path="authors_database.csv"):
    authors_database = pd.read_csv(artifact_path)
    df_new = df[~df[author_col].isin(authors_database[author_col])]
    df_new = append_nationalities(df_new)
    df_new["gender_fixed"] = df_new[author_col].apply(guess_gender)

    return df_new

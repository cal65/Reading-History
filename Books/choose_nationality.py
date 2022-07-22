import pandas as pd
import numpy as np
import sys
import functools


def nationality_counts(df):
    nationality_cols = [n for n in df.columns if "nationality" in n]
    nationality_series = [df[c] for c in nationality_cols]
    if len(nationality_series) >0: # function fails with empty list
        nationalities_all = functools.reduce(lambda a, b: a.append(b), nationality_series)
    else:
        return {}
    counts_df = pd.DataFrame(nationalities_all.value_counts())
    counts_df.columns = ["Count"]
    count_dict = counts_df.to_dict()["Count"]
    return count_dict


def choose_nationality(df, regions_file="world_regions_dict.csv"):
    count_dict = nationality_counts(df)
    nationality_cols = [n for n in df.columns if "nationality" in n]
    regions = pd.read_csv(regions_file, encoding="latin-1")
    def _choose_row(row):
        if pd.isnull(
            row.get("nationality2")
        ):  # shortcut, if nationality 2 is NA, they are all NA
            return row["nationality1"]
        else:
            nationalities = [n for n in row[nationality_cols] if pd.notnull(n)]
            counts = [
                count_dict.get(n) if n in regions["nationality"].values else np.nan
                for n in nationalities
            ]
            if (len(counts) > 0) and (not all(pd.isnull(counts))):
                return nationalities[np.nanargmin(counts)]
            else:
                return None

    countries_chosen = df.apply(_choose_row, axis=1)
    if len(countries_chosen) > 0:
        df["Country.Chosen"] = countries_chosen
    else:
        df["Country.Chosen"] = [None] * len(df)
    return df


if __name__ == "__main__":
    file_path = sys.argv[1]
    df = pd.read_csv(file_path)
    if len(df) > 0:
        choose_nationality(df).to_csv(file_path, index=False)

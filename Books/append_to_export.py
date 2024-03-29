import scrape_goodreads
import sys
import pandas as pd
import re
import logging
import argparse

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


def append_scraping(goodreads_data, standard_export=True):
    """
    Take data meant to be in the Goodreads export format
    Scrape additional fields and add them as columns
    """
    goodreads_data.columns = [c.replace(" ", ".") for c in goodreads_data.columns]
    urls = scrape_goodreads.return_urls(goodreads_data)
    scraped_df = scrape_goodreads.apply_added_by(urls)
    # if the dataset coming in is from a standard Goodreads library export, there are scraped columns that we don't need
    if standard_export:
        scraped_df.drop(columns=["Title", "Author", "Publish_info"], inplace=True)
    goodreads_data_merged = pd.concat([goodreads_data, scraped_df], axis=1)
    return goodreads_data_merged


def apply_append(file_path, standard_export=True):
    goodreads_data = scrape_goodreads.read_goodreads_export(file_path)
    return append_scraping(goodreads_data, standard_export)


def add_to_existing_export(file_path_existing, file_path_new):
    existing = scrape_goodreads.read_goodreads_export(file_path_existing)
    new = scrape_goodreads.read_goodreads_export(file_path_new)
    diff_titles = set(new["Title"]).difference(existing["Title"]).tolist()
    diff = new[new["Title"].isin(diff_titles)]
    diff_scraped = append_scraping(diff)
    return diff_scraped


def update_goodreads(df1, df2, index_column):
    """
    Takes df1, an existing dataframe, and df2, a new dataframe.
    The idea is df1 already has appended fields, but df2 may be more recent.
    Any changes in df2 should be reflected in df1
    Updates df1 on new df2 values
    """
    df2.columns = [c.replace(" ", ".") for c in df2.columns]
    # save all new books
    df2_new = df2[~df2[index_column].isin(df1[index_column])]
    logger.info("Adding " + str(len(df2_new)) + " new rows of data")
    # go over the old books that are in common with existing dataset
    df1.set_index(index_column, inplace=True)
    df2.set_index(index_column, inplace=True)
    df1.update(df2)
    df1.reset_index(inplace=True)
    df2_updated = append_scraping(df2_new)
    df_updated = pd.concat([df1, df2_updated])
    return df_updated


def update_missing_data(df, wait=4, standard_export=True):
    """
    This function is for incomplete appends, when rows failed due to timeouts
    """
    df_missing = df[
        pd.isnull(df["Added_by"])
    ]  # this is a scraped field that is often missing
    if len(df_missing) > 0:
        logger.info("Updating " + str(len(df_missing)) + " missing rows of data")
        urls = scrape_goodreads.return_urls(df_missing)
        scraped_missing = scrape_goodreads.apply_added_by(urls, wait=wait)
        if standard_export == False:
            scraped_missing["Number.of.Pages"] = pd.to_numeric(
                scraped_missing["Number.of.Pages"].str.replace(" pages", ""),
                errors="coerce",
            )
            scraped_missing["Title"] = scraped_missing["Title"].str.strip()
        scraped_missing.index = df_missing.index
        diff_columns = set(scraped_missing.columns).difference(set(df.columns))
        for col in diff_columns:
            df[col] = None
        df.update(scraped_missing)
    else:
        logger.info("No data needs updating")
    return df


def fix_date(file_path):
    """
    In passing csvs back and forth between R and Python, different defaults in reading date columns can be problematic
    This function ensures that the csv in a file path has the Date.Added and Date.Read columns in datetime
    """
    df = pd.read_csv(file_path)
    date_columns = [c for c in df.columns if "date" in c.lower()]
    for c in date_columns:
        if c != 'date_published':
            df[c] = pd.to_datetime(df[c])
    df.to_csv(file_path, index=False)
    # return just for proof
    return df[["Title"] + date_columns]


def merge_with_existing(
    df, db, id_col_df="Book.Id", id_col_db="Book.Id", standard_export=True
):
    """
    df is a dataframe of a goodreads export (not yet appended)
    db is a dataframe of an existing library of goodreads books with only Book.Id and scraped columns (and unique)
    Merge the db fields into df, so as to save scraping time
    """
    # if the dataset coming in is from a standard Goodreads library export, there are scraped columns that we don't need
    if standard_export is True:
        db.drop(
            columns=[
                "Title",
                "Author",
                "Original.Publication.Year",
                "Number.of.Pages",
                "Average.Rating",
            ],
            inplace=True,
        )
    else:
        db.drop(columns=["Title"], inplace=True)
        if "Author" not in db.columns:
            db["Author"] = ""
    df = pd.merge(df, db, left_on=id_col_df, right_on=id_col_db, how="left")
    return df


if __name__ == "__main__":
    """
    Usage: python append_to_export.py filepath.csv [--update] [wait] [--nonstandard]
    """
    parser = argparse.ArgumentParser()
    parser.add_argument("file_path")
    parser.add_argument("wait")
    parser.add_argument(
        "--update",
        dest="update",
        action="store_true",
        help="Mode - default none = apply append",
    )
    parser.add_argument(
        "--nonstandard",
        dest="nonstandard",
        action="store_false",
        help="Include if not appending to a standard Goodreads library export",
    )
    args = parser.parse_args()
    db = pd.read_csv("artifacts/books_database.csv")
    file_path = args.file_path
    update = args.update
    wait = int(args.wait)
    nonstandard = args.nonstandard

    export_path = re.sub(".csv|.xlsx", "_appended.csv", file_path)
    if update is False:
        df = pd.read_csv(file_path)
        df.columns = [c.replace(" ", ".") for c in df.columns]
        df = merge_with_existing(df, db, standard_export=nonstandard)
        df = update_missing_data(df, wait, standard_export=nonstandard)
        df.to_csv(export_path, index=False)
        df = scrape_goodreads.guess_gender(df)
        df.to_csv(export_path, index=False)
        if nonstandard is False:
            fix_date(export_path)
    elif update is True:
        df = pd.read_csv(file_path)
        df = update_missing_data(df, wait, standard_export=nonstandard)
        df.to_csv(file_path, index=False)

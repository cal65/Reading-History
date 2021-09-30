import pandas as pd
import sys
import logging
import argparse
import google_answer as ga
from scrape_goodreads import guess_gender
from wikipedia import search_person_for_gender
from choose_nationality import choose_nationality

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


def remove_nulls(file_path, drop_col="Title", secondary_col="Author"):
    books_db = pd.read_csv(file_path)
    start_rows = len(books_db)
    books_db = books_db[pd.notnull(books_db[drop_col])]
    books_db.drop_duplicates(subset=[drop_col, secondary_col], inplace=True)
    logger.info(
        "Reduced books db size from "
        + str(start_rows)
        + " rows to "
        + str(len(books_db))
    )
    return books_db


def lookup_nationalities(
    books_db,
    authors_db,
    book_author_col="Author",
    authors_author_col="Author",
    source="Randomly Scraped",
):
    books_addon = books_db[
        ~books_db[book_author_col].isin(authors_db[authors_author_col])
    ]
    books_addon = ga.append_nationalities(books_addon)
    books_addon["Source"] = source
    return books_addon


def sync(
    books_addon, authors_db, book_author_col="Author", authors_author_col="Author"
):
    books_addon = books_addon[pd.notnull(books_addon["nationality1"])]
    books_addon = guess_gender(books_addon, gender_col="gender_guessed")
    books_addon["gender_fixed"] = [
        search_person_for_gender(author)
        if gender not in ["male", "female", "non-binary"]
        else author
        for author, gender in zip(books_addon[book_author_col], books_addon["gender_guessed"])
    ]
    books_addon = choose_nationality(books_addon)
    common_cols = list(set(books_addon.columns).intersection(set(authors_db.columns)))
    authors_db = pd.concat([authors_db, books_addon[common_cols]])
    return authors_db


if __name__ == "__main__":
    """
    python title_author_sync.py export_goodreads.csv --limit 100
    """
    parser = argparse.ArgumentParser()
    parser.add_argument("books_file_path", help="The path to a books csv file")
    parser.add_argument(
        "--limit",
        dest="limit",
        required=False,
        type=int,
        nargs=None,
        help="The number of rows to consider from the end of books file path",
    )
    parser.add_argument(
        "--source",
        dest="source",
        required=False,
        type=str,
        nargs=None,
        help="Optional, specifically cite the source. Default is Randomly Scraped",
    )
    args = parser.parse_args()
    print(args)
    books_file_path = args.books_file_path
    source = args.source if args.source else "Randomly Scraped"
    authors_file_path = "authors_database.csv"
    books_db = remove_nulls(books_file_path)
    limit = args.limit
    if limit:
        books_db = books_db.tail(int(limit))
    authors_db = pd.read_csv(authors_file_path)
    books_addon = lookup_nationalities(books_db, authors_db, source=source)
    authors_db = sync(books_addon, authors_db)
    authors_db.to_csv(authors_file_path, index=False)

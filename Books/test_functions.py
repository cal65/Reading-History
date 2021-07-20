import pandas as pd
import numpy as np
from pandas._testing import assert_frame_equal

from google_answer import lookup_unfound
from wikipedia import search_person_for_gender
from update_artifact import create_artifact_addition
from append_to_export import update_missing_data


test_df = pd.DataFrame(
    {
        "Author": ["J.K. Rowling", "LeBron James"],
        "Title": ["Harry Potter and the 8th Book", "How to Build a Superteam"],
    }
)


def test_create_artifact_addition(artifact_path="authors_database.csv"):
    artifact_addition = create_artifact_addition(df=test_df, artifact_path=artifact_path)
    # J.K. Rowling should be in Authors database. LeBron James should not
    assert artifact_addition.shape[0] == 1
    # we should have one row of addition, LeBron James
    assert artifact_addition["gender_fixed"].values == ["male"]
    # LeBron is male
    assert artifact_addition["country_chosen"].values == ["American"]
    # LeBron is American

def test_update_missing_data():
	no_missing_data = pd.DataFrame({'Title': ['A', 'B', 'C'],
		'Author': ['La La', 'Ba Ba', 'Ra Ra'],
		'Added_by': [99, 1029405, 3332859]})
	updated = update_missing_data(no_missing_data)
	assert_frame_equal(no_missing_data, updated)

import pandas as pd
import numpy as np
from google_answer import lookup_unfound
from wikipedia import search_person_for_gender
from update_artifact import update_artifact


test_df = pd.DataFrame(
    {
        "Author": ["J.K. Rowling", "LeBron James"],
        "Title": ["Harry Potter and the 8th Book", "How to Build a Superteam"],
    }
)


def test_update_artifact(artifact_path="authors_database.csv"):
    artifact_addition = update_artifact(df=test_df, artifact_path=artifact_path)
    # J.K. Rowling should be in Authors database. LeBron James should not
    assert artifact_addition.shape[0] == 1
    # we should have one row of addition
    assert artifact_addition["gender_fixed"].values == ["male"]

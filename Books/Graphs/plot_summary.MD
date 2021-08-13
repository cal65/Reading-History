# Plot Summaries:

*finish_plot*: This shows the books that you have read that are least commonly marked as "read" on Goodreads. This may mean they are hard to read, or perhaps some people lost interest, or perhaps they were recently published

*genre_comparison_plot*: This takes a look at the top "shelves", which are crowd-sourced on Goodreads and which I call genres, for each book. A ratio of genre/total read books is calculated for the user and for overall (all users in the database) to create an average comparison

*monthly_pages_read*: This shows the books that you have read and the month you read them. The color is the gender of the author (this is automated, so there are often mistakes), the height is the number of pages of each book. The month comes from the date read as marked on Goodreads.

*nationality_map*: A map of the nationalities of the authors you have read. This comes from automated Google responses + manual research, and is subject to some error and interpretation.

*nationality_barplot*: This is the same data but spelled out, for those who have trouble recognizing countries on a map.

*popularity_spectrum*: A facetted plot dividing books into the number of other users who have shelved this book on Goodreads.

*summary_plot*: 4 basic plots, 1) showing gender split and fiction vs non-fiction split. 2) histogram of the original publication dates of your books. 3) the most popular-rated books amongst your library, colored in by your personal rating (0 means not read) 4) the most common genres that you have read (crowd-sourced from Goodreads shelves)

*yearly_pages_read*: Similar to the monthly_pages_read plot, but on a year scale. Non-fiction books are above the 0 line, fiction books below it, hence there are negative values on the y-axis.
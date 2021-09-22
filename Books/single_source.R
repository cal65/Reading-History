getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

books_db <- books_combined[, .(n_readers=.N, Title=head(Title.Simple,1),
                               Author=head(Author, 1),
                               Original.Publication.Year=mean(Original.Publication.Year, na.rm=T),
                               Shelf1=head(Shelf1, 1),
                               Shelf2=head(Shelf2, 1),
                               Shelf3=head(Shelf3, 1),
                               Shelf4=head(Shelf4, 1),
                               Shelf5=head(Shelf5, 1),
                               Shelf6=head(Shelf6, 1),
                               Shelf7=head(Shelf7, 1),
                               Added_by=max(Added_by,na.rm=T),
                               To_reads=max(To_reads,na.rm=T),
                               Narrative=getmode(Narrative),
                               Number.of.Pages=as.integer(mean(Number.of.Pages, na.rm=T)),
                               Average.Rating = mean(Average.Rating, na.rm=T),
                               shelf_count = length(unique(Shelf1)),
                               Source=head(Source, 1)), by=Book.Id]
authors_from_books <- books_combined[, .(n=.N, Source=head(Source,1)),
                                     by = Author]
books_db[!is.finite(Added_by)]$Added_by <- NA
books_db[!is.finite(To_reads)]$To_reads <- NA

authors_db <- merge(authors_database, authors_from_books, by='Author', all.x=T)
write.csv(books_db, 'artifacts/books_database.csv', row.names=F)


unique_authors <- data.frame(table(authors_from_books[n==1]$Source))
names(unique_authors) <- c('name', 'unique_n')
source_authors <- books_combined[, .(n = length(unique(Author))), by='Source']
names(source_authors) <- c('name', 'n')
unique_authors_df <- merge(source_authors, unique_authors)
unique_authors_df$percent <- with(unique_authors_df, round(unique_n/n, 3))

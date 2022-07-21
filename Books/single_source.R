getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
source('utils.R')
transform_to_db <- function(dt){
  return (dt[, .(n_readers=.N, Title=head(Title.Simple,1),
              Author=head(Author, 1),
              Original.Publication.Year=getmode(Original.Publication.Year),
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
              Number.of.Pages=as.integer(getmode(Number.of.Pages)),
              Average.Rating = mean(Average.Rating, na.rm=T),
              shelf_count = length(unique(Shelf1)),
              Source=head(Source, 1)), by=Book.Id])
}
artifacts <- vector('list')

art_files <- list.files('artifacts/', pattern = '*appended.csv') 
for (file in art_files) {
  artifacts[[file]] <- fread(paste0('artifacts/', file))
}
artifacts_df <- setDT(do.call(rbind.fill, artifacts))
artifacts_df[Source =='']$Source <- 'List'
lapply(artifacts, function(x) length(which(is.na(x$Number.of.Pages))))
books_all <- setDT(rbind.fill(books_combined, artifacts_df))
books_all$Number.of.Pages <- as.numeric(books_all$Number.of.Pages)
books_db <- transform_to_db(books_all)
authors_from_books <- books_all[, .(n=.N, Source=head(Source,1)),
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

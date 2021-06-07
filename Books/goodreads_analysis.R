library(data.table)
library(ggplot2)
library(stringr)
library(scales)
library(ggrepel)
library(ggthemes)
setwd('~/Documents/CAL/Real_Life/Repository/Books/')
source('utils.R')

file_start <- 'data/goodreads_library_export_'
paths <- list('Cal' = paste0(file_start, 'cal_appended.csv'), 
              'Andrea' = paste0(file_start, 'andrea_appended.csv'),
              'Glen'  = 'data/goodreads_library_export_glen_appended.csv',
              'Sarah' = 'data/goodreads_library_export_sarahgrey_appended.csv',
              'Adam' = 'data/goodreads_library_export_adam_appended.csv',
              'Ruby' = 'data/goodreads_library_export_ruby_appended.csv',
              'Liz' = paste0(file_start, 'liz_appended.csv'),
              'Charlotte' = paste0(file_start, 'charlotte_appended.csv'),
              'Corinne' = paste0(file_start, 'corinne_appended.csv'),
              'Sarah_McNabb' = paste0(file_start, 'sarahmcnabb_appended.csv'),
              'Bernadette' = paste0(file_start, 'bernadette_appended.csv'),
              'Elena' = paste0(file_start, 'elena2.csv'),
              'Bev' = paste0(file_start, 'bev_appended.csv'),
              'Mery' = paste0(file_start, 'mery_appended.csv'))
goodreads_list <- lapply(paths, run_all)
for (name in names(paths)){
  goodreads_list[[name]]$Source <- name
  dir.create(paste0('Graphs/', name), showWarnings = F)
}
author_genders_fixed <- read.csv('author_genders_fixed.csv')
author_genders_fixed$gender_fixed <- ifelse(author_genders_fixed$gender_fixed=='', 
                                            author_genders_fixed$gender_guessed,
                                            author_genders_fixed$gender_fixed)
for (name in names(paths)){
  goodreads_list[[name]][!gender %in% c('female', 'male')]$gender <- mapvalues(
    goodreads_list[[name]][!gender %in% c('female', 'male')]$Author, 
    author_genders_fixed$Author, 
    author_genders_fixed$gender_fixed, warn_missing = F)
  write.csv(goodreads_list[[name]], paths[[name]], row.names=F)
}


sample <- read.csv('export_goodreads.csv')
# data cleaning
setDT(sample)
sample <- sample[Title != '']
sample$Added_by[is.na(sample$Added_by)] <- 0
sample$To_reads[is.na(sample$To_reads)] <- 0
sample$Edition_published <- unlist(lapply(str_extract_all(sample$date_published, '[0-9]{4}') , function(x) x[1]))
sample$Edition_published <- as.numeric(sample$Edition_published)
sample$Original.Publication.Year <- unlist(lapply(str_extract_all(sample$Publish_info, 'first published .*') , 
                                               function(x) x[1]))
sample$Original.Publication.Year <- unlist(lapply(str_extract_all(sample$Original.Publication.Year, '[0-9]{4}') , 
                                                function(x) x[1]))
sample$Original.Publication.Year <- as.numeric(sample$Original.Publication.Year)
sample$Original.Publication.Year <- ifelse(!is.na(sample$Original.Publication.Year), 
                                sample$Original.Publication.Year,
                                sample$Edition_published)

sample <- unique(sample)

sample$Source <- 'Random Scraped'
sample$Exclusive.Shelf <- 'read'
ggplot(sample) + geom_histogram(aes(Added_by), bins=100, fill='coral', color='black') + 
  scale_x_log10(label=comma) + 
  ggtitle('Reading Distribution of Goodreads Sample') +
  theme_fivethirtyeight() +
  xlab('Number of Readers') +
  theme(axis.title = element_text())
ggsave('Sample_distribution.jpeg', width=11, height=8)

median(sample[Added_by > 0]$Added_by)
books_combined <- setDT(do.call('rbind.fill', goodreads_list))
books_w_sample <- setDT(rbind.fill(books_combined, sample))

# complete author genders pipeline
author_genders <- books_combined[! gender %in% c('female', 'male'), .(Title=head(Title,1)), by = c('Author', 'gender')]
names(author_genders) <- mapvalues(names(author_genders),
                                   from = 'gender', to = 'gender_guessed')
author_genders <- author_genders[]
author_genders_new <- rbind.fill(author_genders_fixed, author_genders)
write.csv(author_genders_new, 'author_genders_fixed.csv', row.names=F)

# comparison plot df
ggplot(books_combined[order(Date.Read, decreasing = T)][, .SD[1:25], Source]) + 
  geom_text_repel(aes(x=narrative, y=Read, color=gender, label=Title.Simple), 
                  alpha=0.75, size=3) +
  facet_grid(. ~ Source) +
  scale_color_brewer(palette = 'Set1') +
  scale_y_log10(label=comma) +
  ggtitle('Comparison Plot') + ylab('Readers') +
  theme_wsj() +
  theme(plot.title=element_text(hjust=0.5))
ggsave('Graphs/Comparison_Named3.jpeg', width=16, height=10)

books_w_sample$Exclusive.Shelf <- factor(books_w_sample$Exclusive.Shelf,
                                         levels = c('unread', 'read'))
ggplot(books_w_sample[Date.Read > '2010-01-01' | is.na(Date.Read)]) + 
  geom_bar(aes(x=Original.Publication.Year, fill=Source, alpha=Exclusive.Shelf), 
           color='black') +
  facet_grid(Source ~., scales='free') +
  scale_fill_brewer(palette = 'Set3') + xlim(1840, 2020) +
  scale_alpha_discrete(range = c(0.5, 1)) +
  xlab('Original Publication Year') +
  ggtitle('Comparison of Publication Years') +
  theme_solarized() +
  theme(legend.position = 'bottom', plot.title=element_text(hjust=0.5))
ggsave('Graphs/Density_Years3.jpeg', width=13, height=9)

for (name in names(paths)){
  read_plot(goodreads_list[[name]][Read.Count==1], name=name, 
            read_col='Read', title_col = 'Title.Simple', plot=T)
  finish_plot(goodreads_list[[name]], name = name, plot=T)
}

library(igraph)
# graph theory

# spider graph
library(fmsb)
spider_df <- books_combined[Exclusive.Shelf == 'read' & Date.Read > '2010-01-01' | is.na(Date.Read),
                            c('Source', grep('^Shelf', names(books_combined), value=T)),with=F]

spider_df.m <- melt(spider_df, 
                    id.var='Source', value.name = 'Shelf')
setDT(spider_df.m)
spider_df.m <- spider_df.m[!is.na(Shelf)]
top_table <- spider_df.m[!Shelf %in% c('Fiction', 'Nonfiction', ''), 
                         .(Freq = .N), by = c('Source', 'Shelf')][order(Freq, decreasing = T),]
users <- c('Cal', 'Bev', 'Liz', 'Ruby', 'Sarah_McNabb')

top_genres <- unique(top_table[Source %in% users, .SD[1:15], Source ]$Shelf)
#radar_table <- dcast(top_table, Source ~ Shelf, value.var = 'Freq')
#colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
#radarchart(radar_table[,-1], pfcol=colors_in)
top_table$Shelf <- factor(top_table$Shelf, 
                          levels = rev(unique(top_table$Shelf)))
ggplot(top_table[Shelf %in% top_genres & Source %in% users]) + 
  geom_col(aes(x=Shelf, y=Freq, fill=Source), color='black') +
  facet_grid(. ~ Source, scales='free') + 
  scale_fill_brewer(palette = 'Set3') +
  coord_flip() +
  ggtitle('Genre Plot') +
  theme_wsj()
ggsave('Graphs/genre_plot3.jpeg', width=14, height=8)

overall_genre_distribution <- data.frame(table(spider_df.m$Shelf)/nrow(spider_df.m))
for (name in names(paths)){
  indiv_genre <- top_table[Source == name]
  indiv_genre$Freq_perc <- indiv_genre$Freq / sum(indiv_genre$Freq)
  genre_comparison_df <- merge(indiv_genre[,c('Shelf', 'Freq_perc')],
                               overall_genre_distribution,
                               by.x = 'Shelf', by.y = 'Var1',
                               all.y = T)
  genre_comparison_df$difference <- with(genre_comparison_df, Freq_perc - Freq)
  print(name)
  print(genre_comparison_df[which.max(genre_comparison_df$difference)])
}

# Medium post
top_table$Source_medium <- mapvalues(top_table$Source, 
                                     from = c('Adam', 'Liz', 'Ruby', 'Sarah', 'Andrea'),
                                     to = c('Friend 1', 'Friend 2', 'Friend 3', 'Friend 4', 'Friend 5'))
ggplot(top_table[Shelf %in% top_genres & Source != 'Glen']) + 
  geom_col(aes(x=Shelf, y=Freq, fill=Source_medium), color='black') +
  facet_grid(. ~ Source_medium, scales='free') + 
  scale_fill_brewer(palette = 'Set3', 'Source') +
  coord_flip() +
  ggtitle('Genre Plot') +
  theme_wsj()
ggsave('Graphs/genre_plot_medium.jpeg', width=14, height=8)
df_cal <- goodreads_list[['Cal']][Date.Read > '2011-01-01']


## Month plot
for (name in names(paths)){
  month_plot(goodreads_list[[name]], name=name, date_col='Date.Read', 
           page_col='Number.of.Pages', title_col='Title.Simple',
           author_gender_col='gender', lims=c(2010, 2022))
  ggsave(paste0('Graphs/', name, '/Monthly_pages_read_', name, '.jpeg'), width=15, height=9, dpi=180)
  year_plot(goodreads_list[[name]], name=name, fiction_col='narrative', 
            date_col='Date.Read', page_col='Number.of.Pages', 
            title_col='Title.Simple', author_gender_col='gender')
  ggsave(paste0('Graphs/', name, '/Yearly_pages_read_', name, '.jpeg'), width=15, height=9, dpi=180)
  
}
 
# gender analysis by unique authors
unique_authors <- unique(books_combined[,c('Author', 'gender', 'Source')])
gender_count <- unique_authors[, .(male=sum(gender=='male'), female=sum(gender=='female'), total=.N), by='Source']
gender_count$ratio <- with(gender_count, male/total)
gender_count$unknown <- with(gender_count, total - (male + female))
gender_count.m <- melt(gender_count[,c('Source', 'male', 'female', 'unknown')], id = 'Source',
                       value.name = 'count', variable.name='gender')
ggplot(gender_count.m) +
  geom_col(aes(x=Source, y=count, fill=gender), position = position_dodge()) +
  scale_fill_brewer(palette = 'Dark2') + 
  coord_flip() +
  ggtitle('Gender breakdown by user')

for (name in names(goodreads_list)){
  rating_gender <- goodreads_list[[name]][,c('Author', 'narrative', 'gender', 'My.Rating')]
  rating_count <- rating_gender[, .(Rating.Count = .N), by =c('narrative', 'My.Rating')]
  rating_gender <- merge(rating_gender, rating_count, by = c('narrative', 'My.Rating'))
  rating_gender$text_size <- pmin(10, 100/rating_gender$Rating.Count)
  ggplot(rating_gender) +
    geom_tile(aes(x=narrative, y=Author, fill=gender), color='black') +
    scale_fill_brewer(palette = 'Dark2') +
    geom_text(aes(x=narrative, y=Author, label=Author, size=text_size)) +
    scale_size_continuous(guide=F) +
    facet_wrap(narrative ~ My.Rating, scales='free', nrow=2) +
    theme(axis.text.y = element_blank(),
          plot.title = element_text(hjust=0.5),
          panel.background = element_blank())
  ggsave(paste0('Graphs/', name, '/gender_ratings_', name, '.jpeg'), width=15, height=9, dpi=180)
}

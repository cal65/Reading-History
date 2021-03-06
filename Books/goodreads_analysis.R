library(data.table)
library(ggplot2)
library(stringr)
library(scales)
library(ggrepel)
library(ggthemes)
setwd('~/Documents/CAL/Real_Life/Repository/Books/')
source('utils.R')

paths <- list('Cal' = 'data/goodreads_library_export_cal_appended.csv', 
              'Andrea' = 'data/goodreads_library_export_andrea_appended.csv',
              'Glen'  = 'data/goodreads_library_export_glen_appended.csv',
              'Sarah' = 'data/goodreads_library_export_sarahgrey_appended.csv',
              'Adam' = 'data/goodreads_library_export_adam_appended.csv',
              'Ruby' = 'data/goodreads_library_export_ruby_appended.csv',
              'Liz' = 'data/goodreads_library_export_liz_appended.csv')
goodreads_list <- lapply(paths, run_all)
for (name in names(paths)){
  goodreads_list[[name]]$Source <- name
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


books_combined <- setDT(do.call('rbind.fill', goodreads_list))
books_w_sample <- setDT(rbind.fill(books_combined, sample))

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
ggsave('Graphs/Comparison_Named2.jpeg', width=16, height=10)

books_w_sample$Exclusive.Shelf <- factor(books_w_sample$Exclusive.Shelf,
                                         levels = c('unread', 'read'))
ggplot(books_w_sample[Date.Read > '2010-01-01' | is.na(Date.Read)]) + 
  geom_bar(aes(x=Original.Publication.Year, fill=Source, alpha=Exclusive.Shelf), 
           color='black') +
  facet_grid(Source ~., scales='free') +
  scale_fill_brewer(palette = 'Set1') + xlim(1840, 2020) +
  scale_alpha_discrete(range = c(0.5, 1)) +
  xlab('Original Publication Year') +
  ggtitle('Comparison of Publication Years') +
  theme_solarized() +
  theme(legend.position = 'bottom', plot.title=element_text(hjust=0.5))
ggsave('Graphs/Density_Years3.jpeg', width=13, height=9)

## books added by
most_least_popular <- rbind(books_combined[Source != 'Random Scraped', .SD[which.max(Added_by)],
               by = 'Source'],
books_combined[Source != 'Random Scraped', .SD[which.min(Added_by)],
               by = 'Source'])

ggplot(books_w_sample[Added_by > 0 & Exclusive.Shelf == 'read']) + 
  geom_histogram(aes(x=Added_by, fill=Source), bins=60) +
  scale_fill_brewer(palette = 'Set1') + 
  xlab('Number of Readers Added') +
  scale_x_log10(label=comma) +
  facet_grid(Source ~ ., scales='free') + 
  geom_text(data=most_least_popular, 
            aes(x=Added_by, y=1, label=paste0(Title.Simple, ':\n', Added_by))) +
  ggtitle('Comparison of Readers') +
  theme_solarized() +
  theme(plot.title = element_text(hjust=0.5))
ggsave('Graphs/Density_Readers4.jpeg', width=10, height=9)

for (name in names(paths)){
  read_plot(goodreads_list[[name]][Read.Count==1], name=name, 
            read_col='Read', title_col = 'Title.Simple', plot=T)
}


books_read_df <- books_combined[Source != 'Random Scraped' & !is.na(Date.Read)] 

common <- intersect(books_read_df[Source == "Cal's Books"]$Title.Simple , 
                    books_read_df[Source == "Andrea's Books"]$Title.Simple)
books_read_df$In_common <- books_read_df$Title.Simple %in% common
common_df <- unique(books_read_df[In_common == T][,c('Title.Simple', 'Author', 'Average.Rating',
                                              'Number.of.Pages')])
common_df <- setDT(common_df)[, .(Average.Rating = max(Average.Rating),
                                  Number.of.Pages = max(Number.of.Pages)), 
      by =c('Title.Simple', 'Author')]
ggplot(books_read_df, aes(x=Average.Rating, y=Number.of.Pages)) + 
  geom_point(aes(size=log(added_by), fill=Source, color=In_common), 
             shape=21, alpha=0.4) +
  geom_text_repel(data=books_read_df[My.Review != ""], 
                  aes(label=Title.Simple, color=Source), size=3) +
  geom_text_repel(data=common_df, 
                  aes(label=Title.Simple), color='red', size=3) +
  scale_fill_brewer(palette='Dark2') +
  scale_color_brewer(palette='Dark2') +
  scale_size_continuous(breaks = seq(2, 12, 2),
                          labels = round(exp(seq(2, 12, 2))),
                        'Number of People Added') +
  ylim(0, 500) +
  ggtitle('Andrea + Cal book comparison') +
  theme_economist()
ggsave('Graphs/Comparison_plot.jpeg', width=12, height=9)

## reading %
quintiles <- books_combined[, .(q_low = quantile(Read.Percentage, .1, na.rm=T), 
                   q_high = quantile(Read.Percentage, .9, na.rm=T)),
               by = 'Source']
reading_highlows <- books_combined[Exclusive.Shelf == 'read']
reading_highlows[, read_percentage_rank_high := frank(Read.Percentage), by = Source]
reading_highlows[, read_percentage_rank_low := frank(-Read.Percentage), by = Source]
n <- 25
reading_highlows <- reading_highlows[read_percentage_rank_low < n | read_percentage_rank_high < n]
reading_highlows <- reading_highlows[order(Read.Percentage),]
reading_highlows$popularity <- with(reading_highlows, 
                                    ifelse(Added_by < 5000, 'low', 'high'))
reading_highlows$Title.Simple <- factor(reading_highlows$Title.Simple,
                                        levels = unique(reading_highlows$Title.Simple))

ggplot(reading_highlows[popularity == 'low']) +
  geom_col(aes(x=Title.Simple, y=Added_by), fill='blue') +
  geom_col(aes(x=Title.Simple, y=Read), fill='red') +
  facet_wrap(Source ~ ., scales = 'free', ncol=2) +
  coord_flip() + 
  ggtitle('Rarely Finished Reads') +
  theme_solarized() +
  theme(plot.title = element_text(hjust=0.5))
ggsave('Graphs/reading_perc_graph2.jpeg', width=12, height=9)

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
top_genres <- unique(top_table[, .SD[1:20], Source ]$Shelf)
#radar_table <- dcast(top_table, Source ~ Shelf, value.var = 'Freq')
#colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
#radarchart(radar_table[,-1], pfcol=colors_in)
top_table$Shelf <- factor(top_table$Shelf, 
                          levels = rev(unique(top_table$Shelf)))
ggplot(top_table[Shelf %in% top_genres]) + 
  geom_col(aes(x=Shelf, y=Freq, fill=Source), color='black') +
  facet_grid(. ~ Source, scales='free') + 
  scale_fill_brewer(palette = 'Dark2') +
  coord_flip() +
  ggtitle('Genre Plot') +
  theme_wsj()
ggsave('Graphs/genre_plot.jpeg', width=14, height=8)

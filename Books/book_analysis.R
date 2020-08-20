setwd('~/Documents/CAL/Real_Life/Repository/Books/')
options(stringsAsFactors = F)
library(ggplot2)
library(googlesheets)
library(ggrepel)
#read in from Google and environment variable
register_google(key = Sys.getenv(x='GOOGLE_API'))  

books <- gs_title('Books')
books_df <- gs_read(ss=books, ws = 'Sheet1')
books_df$Date.Read <- as.Date(books_df$Date.Read, format = '%m/%d/%Y')
names(books_df) <- gsub(' ', '.', names(books_df))
books_df$Biography[is.na(books_df$Biography)] <- 'Not Biography'
#initial eda graphs
ggplot(books_df) + geom_point(aes(x=Date.Read, y=Fiction, color=Author.Gender, shape=Biography))
ggplot(books_df) + geom_histogram(aes(Date.Read)) +
  scale_x_date(date_labels="%Y", date_breaks = '2 years')
  
#timeline graph
ggplot(books_df) + geom_line(aes(x=Date.Read, y=1:nrow(books_df)), color='white') +
  geom_point(aes(x=Date.Read, y=1:nrow(books_df), fill=Fiction), shape=24, size=4, alpha=0.8) +
  scale_color_brewer(palette='Pastel2') + ylab('Count') +
  scale_fill_manual(values = c('Dark Red', 'Dark Green')) +
  scale_x_date(date_breaks = "1 year", date_labels = '%Y') +
  geom_text_repel(aes(x=Date.Read, y=1:nrow(books_df), label=Title, color=Author.Gender), 
                  size=3, alpha=0.7) +
  ggtitle('Reading Chart') +
  theme(plot.title=element_text(hjust=0.5), panel.background = element_rect(fill='black'), 
        panel.grid = element_blank())
ggsave('Books_Timeline.jpeg', width=15, height=9.5, dpi=200)

ggplot(books_df) + geom_histogram(aes(x=format(Date.Read, '%m'), fill=Author.Gender), stat='count') +
  facet_grid(format(Date.Read, '%Y') ~ .) +
  ggtitle('Month Breakdown') + xlab('Month')


## rolling average
rolling_books_read <- function(df, date_col, interval = 180){
  #recursive function to calculate the number of books read in the last interval
  if (nrow(df) <= 1){
    return(1)
  }
  date <- df[[date_col]][nrow(df)]
  return(c(rolling_books_read(df[1:(nrow(df)-1),], date_col, interval),
              sum(df[[date_col]] - date > (interval * -1) & df[[date_col]] <= date)))
}

library(viridis)

books_df$Rolling.Read <- rolling_books_read(books_df, 'Date.Read')
ggplot(books_df) + 
  geom_line(aes(x=Date.Read, y=Rolling.Read), color='white') + 
  geom_point(aes(x=Date.Read, y=Rolling.Read), color='white') +
  geom_text_repel(aes(x=Date.Read, y=Rolling.Read, label=Title, color=Pages), size=4) +
  scale_x_date(date_breaks = "2 year", date_labels = '%Y') +
  scale_y_continuous('Number of Books Read') +
  scale_color_viridis() +
  theme(plot.title=element_text(hjust=0.5), panel.background = element_rect(fill='grey85'), 
        panel.grid = element_blank()) +
  ggtitle('180 Day Rolling Average of Books Read')
ggsave('Books_rolling_average.jpeg', width=16, height=8)

books_df$Date.Numeric <- as.numeric(books_df$Date.Read) 
interpolated_df <- data.frame(Date.Numeric = min(books_df$Date.Numeric):max(books_df$Date.Numeric))
interpolated_df$y_spl <- splinefun(x=books_df$Date.Numeric, y=books_df$Rolling.Read,
                               method = 'fmm')(interpolated_df$Date.Numeric)
# using a quintic regression instead of spline
lm8 <- lm(data=books_df, Rolling.Read ~ Date.Numeric + I(Date.Numeric^2) +
            I(Date.Numeric^3) + I(Date.Numeric^4) + I(Date.Numeric^5) + I(Date.Numeric^6) +
            I(Date.Numeric^7) + I(Date.Numeric^8))
interpolated_df$y_lm8 <- predict(lm8, newdata = interpolated_df)

interpolated_merged_df <- merge(interpolated_df, books_df, by = 'Date.Numeric', all.x=T)
interpolated_merged_df$Date <- as.Date(interpolated_merged_df$Date.Numeric, origin='1970-01-01')
ggplot(interpolated_merged_df) + 
  geom_line(aes(x=Date, y=y_spl), size=3, alpha=0.1) + 
  geom_point(aes(x=Date.Read, y=y_spl, color=Pages)) +
  geom_text_repel(aes(x=Date, y=y_spl, label=Title, color=Pages), size=3) +
  scale_x_date(date_breaks = "2 year", date_labels = '%Y') +
  scale_y_continuous('Number of Books Read') +
  scale_color_viridis() +
  theme(plot.title=element_text(hjust=0.5), panel.background = element_rect(fill='grey85'), 
        panel.grid = element_blank()) +
  ggtitle('180 Day Rolling Average of Books Read')
ggsave('Books_rolling_average_spline.jpeg', width=16, height=8)

ggplot(interpolated_merged_df) + 
  geom_line(aes(x=Date, y=y_lm8), size=3, alpha=0.1) + 
  geom_point(aes(x=Date.Read, y=Rolling.Read, color=Pages)) +
  geom_text_repel(aes(x=Date, y=Rolling.Read, label=Title, color=Pages), size=3) +
  scale_x_date(date_breaks = "2 year", date_labels = '%Y') +
  scale_y_continuous('Number of Books Read') +
  scale_color_viridis() +
  theme(plot.title=element_text(hjust=0.5), panel.background = element_rect(fill='grey85'), 
        panel.grid = element_blank()) +
  ggtitle('180 Day Rolling Average of Books Read')

###nationality
nation_df <- data.frame(table(books_df$Author.Nationality))
names(nation_df) <- c('Nationality', 'Count')
ggplot(nation_df) + geom_col(aes(x=Nationality, y=Count)) +
  coord_flip()

ggplot(books_df) + geom_point(aes(x=Date.Read, y=Year, color=Fiction, shape=Author.Gender), size=5) +
  scale_color_brewer(palette='Accent') + 
  geom_abline(aes(slope=1/365, intercept=1970), size=1, color='black', 
              alpha=0.5, linetype='dashed') +
  theme(panel.background = element_rect(fill=NA, color='black'), 
        plot.title = element_text(hjust=0.5)) +
  geom_text_repel(aes(x=Date.Read, y=Year, label = Title), alpha=0.8, color='Dark Red', 
                  size=2, box.padding=0.5, segment.alpha=0.3, min.segment.length = 2) +
  ggtitle('Date Read vs. Date Published') +
  xlab('Date Read') + ylab('Date Published') + ylim(1840, 2040) +
  scale_shape_manual('Author Gender', values = c("M" = "\u2642", "F" = "\u2640"))
ggsave('BooksPlot.jpeg', width=10, height=9.5, dpi=200)


ggplot(books_df) + 
  geom_point(aes(x=Date.Read, y=Pages, color=Fiction, shape=Author.Gender), size=5, alpha=0.6) +
  scale_color_brewer(palette='Set1') + 
  scale_shape_manual('Author Gender', values = c("M" = "\u2642", "F" = "\u2640")) +
  geom_text_repel(aes(x=Date.Read, y=Pages, label = Title, color=Fiction), alpha=0.8, 
                  size=2.5, box.padding=0.3, segment.alpha=0.3, min.segment.length = 10) +
  ggtitle("Cal's Reading List") +
  xlab('Date Read') +
  scale_x_date(date_labels="%Y", date_breaks = '2 years') +
  theme(panel.background = element_rect(fill=NA, color='black'), 
        plot.title = element_text(hjust=0.5)) 
ggsave('BooksPlot2.jpeg', width=14, height=9.5, dpi=200)


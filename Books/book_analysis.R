setwd('~/Documents/CAL/Real_Life/Repository/Books/')
options(stringsAsFactors = F)
library(ggplot2)
library(googlesheets4)
library(ggrepel)
library(forcats)
library(viridis)
library(data.table)
library(stringi)
library(ggthemes)
#read in from Google
books <- read_sheet('1x3c7xMfgKCdCg3D1wEqhCxchETzqNuv_arW2QMT4P0M')
books_df <- setDT(data.frame(books))
names(books_df) <- gsub(' ', '.', names(books_df))
books_df$Date.Read <-
  as.Date(books_df$Date.Read, format = '%m/%d/%Y')
books_df$Title <- as.character(unlist(books_df$Title))
books_df$Biography[is.na(books_df$Biography)] <- 'Not Biography'
books_df$Year.Read <- as.numeric(format(books_df$Date.Read, '%Y'))
books_df$Month.Read  <- as.numeric(format(books_df$Date.Read, '%m'))

#initial eda graphs
ggplot(books_df) + geom_histogram(aes(Date.Read), bins=60) +
  scale_x_date(date_labels = "%Y", date_breaks = '2 years') +
  theme_pander()

#timeline graph
ggplot(books_df) + geom_line(aes(x = Date.Read, y = 1:nrow(books_df)), color =
                               'white') +
  geom_point(
    aes(
      x = Date.Read,
      y = 1:nrow(books_df),
      fill = Fiction
    ),
    shape = 24,
    size = 4,
    alpha = 0.8
  ) +
  scale_color_brewer(palette = 'Pastel2') + ylab('Count') +
  scale_fill_manual(values = c('Dark Red', 'Dark Green')) +
  scale_x_date(date_breaks = "1 year", date_labels = '%Y') +
  geom_text_repel(
    aes(
      x = Date.Read,
      y = 1:nrow(books_df),
      label = Title,
      color = Author.Gender
    ),
    size = 3,
    alpha = 0.7
  ) +
  ggtitle('Reading Chart') +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.background = element_rect(fill = 'black'),
    panel.grid = element_blank()
  )
# ggsave(
#   'Books_Timeline.jpeg',
#   width = 15,
#   height = 9.5,
#   dpi = 200
# )

# month plotter
ggplot(books_df[Year.Read > 2010],
       aes(
         x = Month.Read,
         y = Pages,
         group = fct_rev(fct_inorder(Title))
       )) +
  geom_col(
    aes(
      fill = Author.Gender,
      color = Fiction,
      linetype = Fiction
    ),
    width = 1,
    alpha = 0.65
  ) +
  facet_grid(Year.Read ~ .) +
  scale_fill_manual(values = c('grey40', 'pink1', 'royalblue1'),
                    'Author Gender',
                    guide = F) +
  scale_color_brewer('Type', palette = 'Dark2') +
  scale_linetype_discrete('Type') +
  geom_text(
    aes(label = Author),
    position = position_stack(0.5),
    size = 2.4,
    fontface = 'bold'
  ) +
  scale_x_continuous(breaks = 1:12, labels = 1:12) +
  ggtitle('Month Breakdown') + xlab('Month') +
  theme(
    strip.text.y = element_text(angle = 0),
    plot.title = element_text(hjust = 0.5),
    panel.background = element_rect(color = 'black', fill = NA),
    legend.position = 'bottom'
  )
ggsave('Monthly_pages_read.jpeg',
       width = 16,
       height = 10, 
       dpi=350)

## rolling average
rolling_books_read <- function(df, date_col, interval = 180) {
  #recursive function to calculate the number of books read in the last interval
  if (nrow(df) <= 1) {
    return(1)
  }
  date <- df[[date_col]][nrow(df)]
  return(c(
    rolling_books_read(df[1:(nrow(df) - 1), ], date_col, interval),
    sum(df[[date_col]] - date > (interval * -1) &
          df[[date_col]] <= date)
  ))
}

books_df$Rolling.Read <-
  rolling_books_read(books_df, 'Date.Read', interval = 180)
ggplot(books_df) +
  geom_line(aes(x = Date.Read, y = Rolling.Read), color = 'white') +
  geom_point(aes(x = Date.Read, y = Rolling.Read), color = 'white') +
  geom_text_repel(aes(
    x = Date.Read,
    y = Rolling.Read,
    label = Title,
    color = Pages
  ),
  size = 4) +
  scale_x_date(date_breaks = "2 year", date_labels = '%Y') +
  scale_y_continuous('Number of Books Read') +
  scale_color_viridis() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.background = element_rect(fill = 'grey85'),
    panel.grid = element_blank()
  ) +
  ggtitle('180 Day Rolling Average of Books Read')
ggsave('Books_rolling_average.jpeg',
       width = 16,
       height = 8)

books_df$Date.Numeric <- as.numeric(books_df$Date.Read)
interpolated_df <-
  data.frame(Date.Numeric = min(books_df$Date.Numeric):max(books_df$Date.Numeric))
interpolated_df$y_spl <-
  splinefun(
    x = books_df$Date.Numeric,
    y = books_df$Rolling.Read,
    method = 'fmm'
  )(interpolated_df$Date.Numeric)

interpolated_merged_df <-
  merge(interpolated_df, books_df, by = 'Date.Numeric', all.x = T)
interpolated_merged_df$y_interpolated <-
  approx(books_df$Date.Numeric,
         books_df$Rolling.Read,
         xout = interpolated_merged_df$Date.Numeric)$y
interpolated_merged_df$Date <-
  as.Date(interpolated_merged_df$Date.Numeric, origin = '1970-01-01')


ss <- smooth.spline(books_df$Date.Numeric,
                    books_df$Rolling.Read)
interpolated_merged_df$smooth_spline <-
  predict(ss, interpolated_merged_df$Date.Numeric)$y

ggplot(interpolated_merged_df[which(interpolated_merged_df$Date > '2014-01-01'),]) +
  geom_line(aes(x = Date, y = y_spl), size = 3, alpha = 0.1) +
  geom_point(aes(x = Date.Read, y = y_spl, color = Pages)) +
  geom_text_repel(aes(
    x = Date,
    y = y_spl,
    label = Title,
    color = Pages
  ), size = 3) +
  scale_x_date(date_breaks = "2 year", date_labels = '%Y') +
  scale_y_continuous('Number of Books Read') +
  scale_color_viridis() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.background = element_rect(fill = 'grey85'),
    panel.grid = element_blank()
  ) +
  ggtitle('180 Day Rolling Average of Books Read')
ggsave('Books_rolling_average_spline.jpeg',
       width = 16,
       height = 8)


ggplot(interpolated_merged_df[which(interpolated_merged_df$Date > '2013-01-01'),]) +
  geom_line(aes(x = Date, y = smooth_spline),
            size = 3,
            alpha = 0.1) +
  geom_point(aes(x = Date.Read, y = Rolling.Read, color = Pages)) +
  geom_text_repel(aes(
    x = Date,
    y = Rolling.Read,
    label = Title,
    color = Pages
  ), size = 3) +
  scale_x_date(date_breaks = "2 year", date_labels = '%Y') +
  scale_y_continuous('Number of Books Read') +
  scale_color_viridis() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.background = element_rect(fill = 'grey85'),
    panel.grid = element_blank()
  ) +
  ggtitle('180 Day Rolling Average of Books Read')
ggsave('Books_rolling_smoothspline.jpeg',
       width = 16,
       height = 8)

###nationality
nation_df <- data.frame(table(books_df$Author.Nationality))
names(nation_df) <- c('Nationality', 'Count')
setDT(nation_df)
nation_df <- nation_df[order(Count)]
nation_df$Nationality <-
  factor(nation_df$Nationality, levels = nation_df$Nationality)
ggplot(nation_df) + geom_col(aes(x = Nationality, y = Count), fill = 'steelblue3') +
  coord_flip() + ggtitle("Nationalities Read")

ggplot(books_df) + geom_point(aes(
  x = Date.Read,
  y = Year,
  color = Fiction,
  shape = Author.Gender
),
size = 5) +
  scale_color_brewer(palette = 'Accent') +
  geom_abline(
    aes(slope = 1 / 365, intercept = 1970),
    size = 1,
    color = 'black',
    alpha = 0.5,
    linetype = 'dashed'
  ) +
  theme(
    panel.background = element_rect(fill = NA, color = 'black'),
    plot.title = element_text(hjust = 0.5)
  ) +
  geom_text_repel(
    aes(x = Date.Read, y = Year, label = Title),
    alpha = 0.8,
    color = 'Dark Red',
    size = 2,
    box.padding = 0.5,
    segment.alpha = 0.3,
    min.segment.length = 2
  ) +
  ggtitle('Date Read vs. Date Published') +
  xlab('Date Read') + ylab('Date Published') + ylim(1840, 2040) +
  scale_shape_manual('Author Gender', 
                     values = c("M" = "\u2642", "F" = "\u2640", "B" = "\u2660"))
ggsave('BooksPlot.jpeg',
       width = 10,
       height = 9.5,
       dpi = 200)


ggplot(books_df) +
  geom_point(
    aes(
      x = Date.Read,
      y = Pages,
      color = Fiction,
      shape = Author.Gender
    ),
    size = 5,
    alpha = 0.6
  ) +
  scale_color_brewer(palette = 'Set1') +
  scale_shape_manual('Author Gender', 
                     values = c("M" = "\u2642", "F" = "\u2640", "B" = "\u2660")) +
  geom_text_repel(
    aes(
      x = Date.Read,
      y = Pages,
      label = Title,
      color = Fiction
    ),
    alpha = 0.8,
    size = 2.5,
    box.padding = 0.3,
    segment.alpha = 0.3,
    min.segment.length = 10
  ) +
  ggtitle("Cal's Reading List") +
  xlab('Date Read') +
  scale_x_date(date_labels = "%Y", date_breaks = '2 years') +
  theme(
    panel.background = element_rect(fill = NA, color = 'black'),
    plot.title = element_text(hjust = 0.5)
  )
ggsave('BooksPlot2.jpeg',
       width = 14,
       height = 9.5,
       dpi = 200)

books_df$Decade <- round(books_df$Year,-1)
books_df$Era <-
  cut(
    books_df$Year,
    breaks = c(0, 1900, 1950, 1970, 1990, 2000, 2010, 2015, 2020, 2025),
    c(
      '1800s',
      'early 1900s',
      '1950-1970',
      '1970-1990',
      '1990-2000',
      '2000-2010',
      '2010-2015',
      '2015-2020',
      '2020-2025'
    )
  )
years_tile <-
  books_df[, .(n = .N), by = c('Year', 'Year.Read', 'Era')]
ggplot(years_tile) +
  geom_tile(aes(x = Year.Read, y = Year, fill = n)) +
  geom_text(
    data = books_df,
    aes(x = Year.Read, y = Year, label = Author),
    size = 2,
    color = 'orange'
  ) +
  geom_abline(aes(slope = 1, intercept = 0)) +
  scale_fill_gradient(low = 'light blue', high = 'dark blue') +
  facet_grid(Era ~ ., scales = 'free', drop = T) +
  scale_y_reverse() +
  xlab('Year Read') + ylab('Year Published') +
  ggtitle('Year Tile Plot')



# divergent chart
divergent_df <- books_df
divergent_df$Pages <-
  with(divergent_df, ifelse(Fiction == 'Fiction',-1 * Pages, Pages))
str_len <- 30

divergent_df$Title <- sapply(divergent_df$Title,
                             function(x)
                               paste(stri_wrap(x, width = str_len), collapse = '\n'))
ggplot(divergent_df[Year.Read > 2011], aes(
  x = Year.Read,
  y = Pages,
  group = fct_rev(fct_inorder(Title))
)) +
  geom_col(
    aes(color = Fiction, fill = Author.Gender),
    size = 0.7,
    width = .7,
    alpha = 0.7
  ) +
  geom_hline(
    yintercept = 0,
    linetype = 'solid',
    size = 3,
    color = 'white'
  ) +
  geom_text(aes(label = Title), position = position_stack(0.5), size = 2) +
  scale_color_manual(values = c('chartreuse', 'Grey')) +
  scale_fill_manual(values = c('grey10', 'pink1', 'royalblue1'), 'Author Gender') +
  scale_x_continuous(breaks = c(min(divergent_df$Year.Read) : max(divergent_df$Year.Read)),
    labels = c(min(divergent_df$Year.Read) : max(divergent_df$Year.Read))) +
  xlab('Year Read') +
  ggtitle('Reading History') +
  theme_pander() + theme(plot.title = element_text(hjust = 0.5),
                         legend.position = 'bottom')
ggsave(
  'Annual_Summary.jpeg',
  width = 16,
  height = 10,
  dpi = 450
)

table(books_df[, c('Author.Gender', 'Fiction')])
sort(table(books_df$Author.Nationality))


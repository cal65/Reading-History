options(stringsAsFactors = F)
library(ggplot2)
library(data.table)
library(ggrepel)
library(forcats)
library(plyr)
library(stringi)
library(rworldmap)
library(RColorBrewer)
setwd('~/Documents/CAL/Real_Life/Repository/Books/')

preprocess <- function(dt){
  dt$gender <- mapvalues(dt$gender,
                                       from = c('mostly_male', 'mostly_female'),
                                       to = c('male', 'female'), warn_missing = FALSE)
  names(dt) <- gsub(' ', '.', names(dt))
  dt$Date.Read <- as.Date(dt$Date.Read, format = '%Y/%m/%d')
  dt$Title.Simple <- gsub(':.*', '', dt$Title)
  dt$Title.Simple <- gsub('\\(.*\\)', '', dt$Title.Simple)
  dt$Exclusive.Shelf <- mapvalues(dt$Exclusive.Shelf,
                                  from = c('2020-books', 'currently-reading', 'to-read'),
                                  to=c('unread', 'unread', 'unread'))
  
  return(dt)
}

narrative <- function(dt){
  dt$narrative <- apply(
    dt, 1, function(x) ifelse(any(
    grepl('Nonfiction|Memoir', x[c('Shelf1', 'Shelf2', 'Shelf3', 'Shelf4')])
    ), 'Nonfiction', 'Fiction')
  )
  return(dt)
}

read_percentage <- function(dt){
  # It seems sometimes the "added by" value is off by a factor of 10. When the 
  # number of people listing the book as "to read" is larger than total added by, multiply by 10
  dt[To_reads > Added_by, Added_by := Added_by * 10] 
  dt$Read <- with(dt, Added_by - To_reads)
  dt$Read.Percentage <- with(dt, Read / Added_by)
  
  return(dt)
}

run_all <- function(csv_path){
  dt <- fread(csv_path)
  dt <- preprocess(dt)
  dt <- narrative(dt)
  dt <- read_percentage(dt)
  return(dt)
}

preprocess_dt <- function(dt){
  dt <- dt[Title != '']
  dt$Added_by[is.na(dt$Added_by)] <- 0
  dt$To_reads[is.na(dt$To_reads)] <- 0
  dt$Edition_published <- unlist(lapply(str_extract_all(dt$date_published, '[0-9]{4}') , function(x) x[1]))
  dt$Edition_published <- as.numeric(dt$Edition_published)
  dt$Original.Publication.Year <- unlist(lapply(str_extract_all(dt$Publish_info, 'first published .*') , 
                                                    function(x) x[1]))
  dt$Original.Publication.Year <- unlist(lapply(str_extract_all(dt$Original.Publication.Year, '[0-9]{4}') , 
                                                    function(x) x[1]))
  dt$Original.Publication.Year <- as.numeric(dt$Original.Publication.Year)
  dt$Original.Publication.Year <- ifelse(!is.na(dt$Original.Publication.Year), 
                                             dt$Original.Publication.Year,
                                             dt$Edition_published)
  
  dt <- unique(dt)
  
  dt$Source <- 'Random Scraped'
  dt$Exclusive.Shelf <- 'read'
}

month_plot <- function(df, name, date_col, page_col, title_col,
                       author_gender_col, lims=NULL){
  setDT(df)
  df$Year.Read <- as.numeric(format(df[[date_col]], '%Y'))
  df$Month.Read  <- as.numeric(format(df[[date_col]], '%m'))
  df <- df[!is.na(Year.Read)]
  if (!is.null(lims)){
    df <- df[Year.Read > lims[1] & Year.Read < lims[2]]
  }
  ggplot(df, 
         aes(x=Month.Read, y=get(page_col), group=fct_rev(fct_inorder(get(title_col))))) + 
    geom_col(aes(fill=get(author_gender_col)), width=1, alpha=0.65, color='black') + 
    facet_grid(Year.Read ~ .) +
    scale_fill_brewer('Author Gender', palette = 'Pastel1', guide=F) +
    scale_color_brewer('Type', palette = 'Dark2') +
    scale_linetype_discrete('Type') +
    geom_text(aes(label=Author), position = position_stack(0.5), 
              size=2.5, fontface='bold') +
    scale_x_continuous(breaks=1:12, labels=1:12) +
    ggtitle(paste0('Month Breakdown - ', name)) + 
    xlab('Month') + ylab('Number of Pages') + 
    theme(strip.text.y=element_text(angle=0), plot.title = element_text(hjust=0.5),
          panel.background = element_rect(color='black', fill=NA))
}

year_plot <- function(df, name, fiction_col, date_col, page_col, 
                      title_col, author_gender_col, str_len=30, start_year = 2010){
  divergent_df <- df
  divergent_df$Pages <- with(divergent_df, ifelse(get(fiction_col) == 'Fiction', -1*get(page_col), get(page_col)))
  str_len <- str_len
  divergent_df$Year.Read <- format(divergent_df[[date_col]], '%Y')
  
  divergent_df[[title_col]] <- sapply(divergent_df[[title_col]],
                               function(x) paste(stri_wrap(x, width=str_len), collapse='\n'))
  ggplot(divergent_df[Year.Read >= start_year], aes(x=Year.Read, y=Pages, group=fct_rev(fct_inorder(get(title_col))))) +
    geom_col(aes(color=get(fiction_col), fill=get(author_gender_col)), 
             size=0.7, width = .7, alpha=0.7) +
    geom_hline(yintercept=0, linetype='solid', size=1, color='white') +
    geom_text(aes(label=get(title_col)), position = position_stack(0.5), size=1.5) +
    scale_color_manual(values=c('chartreuse', 'Grey'), 'Narrative') + 
    scale_fill_brewer(palette='Dark2', 'Author Gender') +
    xlab('Year Read') +
    ggtitle(paste0(name, ' - Reading History')) +
    theme_pander() + theme(plot.title=element_text(hjust=0.5), 
                           legend.position = 'bottom') 
}

generate_labels <- function(breaks){
  if (length(breaks) == 2){
    return (paste0(breaks[1], ' - ', breaks[2]))
  }
  else {
    return (c(paste0(breaks[1], ' - ', breaks[2]), generate_labels(breaks[-1])))
  }
}

read_plot <- function(df, 
                      name, 
                      read_col, 
                      title_col, 
                      min_break = 3, 
                      plot=F, 
                      plot_name = 'popularity_spectrum_',
                      date_col='Date.Read',
                      start_year=NA){
  max_read <- as.integer(max(df[[read_col]], na.rm=T))
  df <- df[!is.na(get(read_col))]
  if (!is.na(start_year)){
    start_date <- as.Date(paste0(start_year, '-01-01'))
    df <- df[date_col > start_date]
  }
  # control text sizes
  max_digits <- nchar(as.character(max_read)) + 1
  breaks <- c(0, 10^c(min_break : max_digits))
  df[[read_col]] <- as.numeric(df[[read_col]])
  df$title_length <- nchar(df[[title_col]])
  df$text_size <- pmax(3, 70/df$title_length)
  # order title text by popularity
  df <- df[order(get(read_col))]
  df[[title_col]] <- factor(df[[title_col]], levels = unique(df[[title_col]]))
  labels = generate_labels(prettyNum(breaks, big.mark = ',', scientific=F))
  df$strats <- cut(df[[read_col]], breaks = breaks, 
                   labels = labels)
  ggplot(df, aes(x=strats, y=get(title_col))) +
    geom_tile(aes(fill=narrative), color='black') +
    geom_text(aes(label = get(title_col), size = text_size)) +
    facet_wrap(strats ~ ., scales='free', nrow=1) +
    scale_fill_manual(values = c('hotpink2', 'darkolivegreen')) +
    scale_size_continuous(guide=F, range=c(3, 10)) +
    xlab('Number of Readers') + 
    ylab('Title') +
    ggtitle(paste0('Readership Spectrum - ', name)) +
    theme(axis.text.y = element_blank(),
          plot.title = element_text(hjust=0.5),
          panel.background = element_blank())
  if (plot){
    ggsave(paste0('Graphs/', name, '/', plot_name, name, '.jpeg'), width = 16, height=9)
  }
}

### % finish plot
finish_plot <- function(df, 
                      name, 
                      read_col = 'Read.Percentage',
                      n = 5, 
                      plot=F, 
                      plot_name = 'finish_plot_'){
  df_read <- df[order(get(read_col)),]
  df_read <- df_read[!is.na(get(read_col))]
  # keep only top and bottom n
  df_read <- rbind(head(df_read, n), tail(df_read, n))
  df_read$Title.Simple <- factor(df_read$Title.Simple,
                                 levels = unique(df_read$Title.Simple))
  df_read$Popularity <- c(rep('Least', n), rep('Most', n))
  df_read$Popularity <- factor(df_read$Popularity, levels = c('Most', 'Least'))
  ggplot(df_read, aes(x=Title.Simple)) +
    geom_col(aes( y=1), fill='Dark Blue') +
       geom_col(aes( y=get(read_col)), fill='red') +
    geom_text(aes(y=get(read_col)/2, label = paste0(Read, ' / ', Added_by)), 
              size=n * 3/5, color='white') +
    facet_grid(Popularity ~ ., scales='free', space='free') +
    ylim(0, 1) +
    xlab('Title') +
    ylab('Reading Percentage') +
       coord_flip() + 
       ggtitle('Most and Least Finished Reads') +
       theme_solarized() +
       theme(plot.title = element_text(hjust=0.5))
  if (plot == T){
    ggsave(paste0('Graphs/', name, '/', plot_name, name, '.jpeg'), width=12, height=8)
  }
}

year_comparison <- function(l, year_col, year_start, user, plot=T) {
  l_df <- setDT(do.call('rbind.fill', l))
  year_max <- max(l_df[,get(year_col)], na.rm=T)
  den_all <- density(l_df[!is.na(get(year_col)),][,get(year_col)], 
          from = year_start, to = year_max)
  den_user <- density(l[[user]][!is.na(get(year_col)),][,get(year_col)], 
                      from = year_start, to = year_max)
  density_df <- data.frame(x=den_all$x, y_all=den_all$y, y_user= den_user$y)
  names(density_df) <- mapvalues(names(density_df), 
                                 from = 'y_user', to = paste0('y_', user))
  density_df$y_diff <- with(density_df, get(paste0('y_', user)) - y_all)
  density_df.m <- reshape2::melt(density_df, id = 'x', variable.name = 'Source')
  density_df.m$Source <- gsub('y_', '', density_df.m$Source)
  density_df.m$Source <- factor(density_df.m$Source, levels = unique(density_df.m$Source))
  ggplot(density_df.m) +
    geom_area(aes(x=x, y=value, fill=Source)) +
    facet_grid(Source ~ .) +
    scale_fill_brewer(palette = 'Pastel1') +
    xlab(gsub('.', ' ', year_col)) + 
    theme_dark() +
    ggtitle('Publication Year Comparison')
  if (plot == T){
    ggsave(paste0('Graphs/', user, '/publication_year_', user, '.jpeg'), width=12, height=8)
  }
}

update_authors_artifact <- function(artifact, df_new, id_col='Author'){
  authors_new = setdiff(df_new[,get(id_col)], artifact[,get(id_col)])
  
}

merge_nationalities <- function(df, authors_db, country_col = 'country_chosen'){
  df <- merge(df, authors_db[,c('Author', country_col)], by='Author', all.x=T)
  return (df)
}

plot_map_data <- function(df, region_dict, world_df, user, country_col = 'country_chosen'){
  country_df <- merge(df, region_dict, by.x='country_chosen', by.y='nationality', all.x=T)
  regions_count <- data.frame(table(country_df$region))
  names(regions_count) <- c('region', 'count')
  world_df <- merge(world_df, regions_count, all.x=T)
  max_count = max(regions_count$count)
  my_breaks <- c(1, rep(2^(1:round(log2(max_count)))))
  ggplot(world_df) + 
    geom_polygon((aes(x=long, y=lat, group=group, fill=count))) +
    scale_fill_gradientn(name = "count", trans = "log", breaks=my_breaks,
                        colors=rev(brewer.pal(8, 'RdBu'))) +
    ggtitle(paste0('Author Nationality Map - ', user)) +
    theme_pander() + theme(plot.title=element_text(hjust=0.5), 
                           legend.position = 'bottom', legend.key.width = unit(1.5, 'cm')) 
  ggsave(paste0('Graphs/', user, '/nationality_map_', user, '.jpeg'), width=12, height=8)
}

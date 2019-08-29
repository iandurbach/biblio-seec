## Makes a data frame containing *guaranteed* accredited outputs for SEEC
## Data obtained from the UCT Research Office for Dept StatSci review (2014-2017, no 2018 yet)
## Filtered by SEEC core membership in each year, so a paper is only counted if it included at least one core 
## member. However, publication units can be earned by any UCT staff, so the number of units is properly interpreted 
## as "units/subsidies earned for publications where at least one SEEC core member was an author".

library(readxl)
library(dplyr)
library(tidytext)
library(stringr)
library(ggplot2)
library(tidyr)

# get SEEC core members surnames
source("get_seec_core.R")

# read in Science Faculty pub counts
x0 <- read_excel("data/SEEC_data_2014_2017.xlsx", sheet = "2014")
x1 <- read_excel("data/SEEC_data_2014_2017.xlsx", sheet = "2015")
x2 <- read_excel("data/SEEC_data_2014_2017.xlsx", sheet = "2016")
names(x2) <- str_remove(names(x2), "\r\n") # weird column names in this df
x3 <- read_excel("data/SEEC_data_2014_2017.xlsx", sheet = "2017")

# no journal name in 2017 so just add a NA
x3 <- x3 %>% mutate(journal = NA)

# extract desired columns and standardize column names
x0 <- x0 %>% select(year, n_authors = `Total number of authors`, author = Author, credits = `DoE score`, title = Title, journal = `Journal name`)
x1 <- x1 %>% select(year = `DoE reporting year`, n_authors = `Total number of authors`, author = `Author`, credits = `Claimed units`, title = Title, journal = `Journal name`)
x2 <- x2 %>% select(year = `Publication Year`, n_authors = `Total Number of Authors`, author = Author, credits = `Units claimed for author`, title = `Article Title`, journal = `Title of Journal`)
x3 <- x3 %>% select(year = `Publication Year`, n_authors = `Total Number of Authors`, author = `UCT Author`, credits = `Units claimed for author`, title = `Article Title`, journal)

# combine
x <- rbind.data.frame(x0,x1,x2,x3)
rm(x0,x1,x2,x3)

# make variables numeric if they should be
x$year <- as.numeric(x$year)
x$n_authors <- as.numeric(x$n_authors)

# extract author surnames
x <- x %>% mutate(author = str_to_lower(str_remove(author, "[, ].+" )))

# reorder variables same as other (Sulaiman) data
x <- x %>% mutate(cites = 0, 
                  n_seec = 0,
                  name_year = paste(author, year, sep = "_"))
x <- x %>% dplyr::select(year, title, journal, cites, author,  n_authors, n_seec, credits, name_year)

# only keep papers by seec core members
seec_pubs_urc <- x %>% 
  filter(name_year %in% seec_core) %>% 
  mutate(title = str_to_lower(title)) %>%
  mutate(title = str_squish(str_replace_all(title, '[[:punct:]]',' '))) %>%
  mutate(short_title = word(title, start = 1, end = 5))

credited_papers <- seec_pubs_urc %>% group_by(short_title) %>% summarize(credits_earned = sum(credits),
                                                                   title = first(title),
                                                                   year = first(year))



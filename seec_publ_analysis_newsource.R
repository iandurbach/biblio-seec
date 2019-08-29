library(readxl)
library(dplyr)
library(tidytext)
library(stringr)
library(ggplot2)
library(tidyr)
library(stringdist)
library(igraph)

# get SEEC core members surnames
source("get_seec_core.R")

# get accredited list of papers with at least one SEEC core author
source("make_accredited_papers.R")

# read in Science Faculty pub counts
x0 <- read_excel("data/SEEC Publications.xlsx", sheet = "Included")
x1 <- read_excel("data/SEEC Publications.xlsx", sheet = "Toinclude")

## process Included pubs
x0 <- x0 %>% mutate(`SEEC name(s)` = str_remove_all(`SEEC name(s)`, "NA; "))
x0 <- x0 %>% mutate(n_authors = str_count(x0$`author(s)`, ",") + 1,
                    n_seec = str_count(x0$`SEEC name(s)`, "[,;]") + 1)
max_authors <- max(x0$n_authors)
max_seec <- max(x0$n_seec)
newnames <- paste0("auth",1:max_seec)

x0 <- x0 %>% separate(`SEEC name(s)`, into = newnames, sep = "[;,]", extra = "drop", fill = "right")
x0 <- x0 %>% gather(author_order, author, newnames)
x0 <- x0 %>% mutate(author = str_trim(author))
x0 <- x0 %>% filter(!is.na(author)) %>% filter(is.na(`exclude?  (1=yes)`))
x0 <- x0 %>% distinct(title, author, .keep_all = TRUE)
x0 <- x0 %>% dplyr::select(year, title, journal, cites, author, n_authors, n_seec)
# few authors are duplicated in raw data so recalculate n_seec
x0 <- x0 %>% add_count(title) %>% mutate(n_seec = n) %>% select(-n)


# process Toinclude pubs (manually inserted "Any other" in here)
x1 <- x1 %>% mutate(`SEEC name(s)` = str_remove_all(`SEEC name(s)`, "NA; "))
x1 <- x1 %>% mutate(n_authors = str_count(x1$`author(s)`, ",") + 1,
                    n_seec = str_count(x1$`SEEC name(s)`, "[,;]") + 1)
x1 <- x1 %>% filter(!is.na(n_seec)) # remove one pub with no SEEC authors
max_authors <- max(x1$n_authors)
max_seec <- max(x1$n_seec)
newnames <- paste0("auth",1:max_seec)

x1 <- x1 %>% separate(`SEEC name(s)`, into = newnames, sep = "[;,]", extra = "drop", fill = "right")
x1 <- x1 %>% gather(author_order, author, newnames)
x1 <- x1 %>% mutate(author = str_trim(author))
x1 <- x1 %>% filter(!is.na(author)) %>% filter(str_detect(`Toinclude? (1=yes)`, "1"))
x1 <- x1 %>% distinct(title, author, .keep_all = TRUE)
x1 <- x1 %>% dplyr::select(year, title, journal, cites, author, n_authors, n_seec)
# few authors are duplicated in raw data so recalculate n_seec
x1 <- x1 %>% add_count(title) %>% mutate(n_seec = n) %>% select(-n)

pubs <- rbind(x0, x1)

# extract author surnames
pubs <- pubs %>% mutate(author = str_to_lower(word(author, -1)),
                        title = str_to_lower(title))

# only keep papers by seec core members
seec_pubs <- pubs %>% 
  mutate(name_year = paste(author, year, sep = "_")) %>%
  filter(name_year %in% seec_core) %>%
  mutate(title = str_to_lower(title)) %>%
  mutate(title = str_squish(str_replace_all(title, '[[:punct:]]',' '))) %>%
  mutate(short_title = word(title, start = 1, end = 5))

# add in credits earned and verified by UCT
seec_pubs <- seec_pubs %>% 
  left_join(seec_pubs_urc %>% select(short_title, author, credits), by = c("short_title", "author"))

# as a check, add in the closest matching title from the list of accredited papers
closest_match = function(string, stringVector){
  stringVector[amatch(string, stringVector, maxDist=Inf)]
}
seec_pubs <- seec_pubs %>%
  mutate(credited = (short_title %in% unique(seec_pubs_urc$short_title)),
         best_match = closest_match(title, unique(seec_pubs_urc$title)),
         match = (title == best_match))
seec_pubs <- seec_pubs %>% select(title, best_match, match, credited, credits, everything()) %>% arrange(desc(year), title)

# there's one error, 2018 paper by Henning with a very similar title ("Sharks.... 13" vs "Sharks.... 14")
# nothing in 2018 has been accredited yet, so just manually change
seec_pubs$credited[seec_pubs$year == 2018] <- FALSE
seec_pubs$credits[seec_pubs$year == 2018] <- NA

## summaries

# publications over time

all_by_year <- seec_pubs %>% 
  group_by(year) %>% 
  summarize(unique = n_distinct(title), 
            all = n() - unique,
            units = sum(credits, na.rm = TRUE)) %>% 
  filter(year < 2019) %>%
  mutate(units = ifelse(units == 0, mean(units[1:4]), units),
         value = ifelse(year < 2018, "actual", "projected")) 

subsidy <- data.frame(year = 2014:2018,
                      rand_per_unit = c(108693, 107222, 102520, 129145, 129145))

all_by_year <- all_by_year %>%
  left_join(subsidy, by = "year") %>%
  mutate(subs_earned = rand_per_unit * units)

p1 <- all_by_year %>%
  gather(unique, all, key = "papers", value = "n") %>%
  ggplot(aes(x = year, y = n, fill = papers)) + 
  geom_bar(stat = "identity") +
  theme_bw(base_size=12) + 
  xlab("Year") + ylab("Number of papers") + 
  theme(axis.text=element_text(size=12), axis.title=element_text(size=12)) +
  theme(legend.position = "bottom", legend.text=element_text(size=12),
        legend.title=element_blank())

p2 <- all_by_year %>%
  filter(year < 2019) %>%
  mutate(units = ifelse(units == 0, mean(units[1:4]), units),
         value = ifelse(year < 2018, "actual", "projected")) %>%
  ggplot(aes(x = year, y = units)) + 
  geom_line(alpha = 0.4) +
  geom_point(aes(colour = value, shape = value), size = 4) +
  ylim(c(0,6)) +
  theme_bw(base_size=12) + 
  theme(legend.title = element_blank()) +
  xlab("Year") + ylab("Number of publication units") 

options(scipen=1000000)

p2a <- all_by_year %>%
  ggplot(aes(x = year, y = subs_earned)) + 
  geom_line(alpha = 0.4) +
  geom_point(aes(colour = value, shape = value), size = 4) +
  theme_bw(base_size=12) + 
  theme(legend.title = element_blank()) +
  xlab("Year") + ylab("Value of publication subsidies (R)") 

p3 <- seec_pubs %>% 
  filter(!is.na(journal), !str_detect(journal, "University")) %>%
  mutate(journal = str_to_lower(journal)) %>%
  count(journal) %>%
  arrange(desc(n)) %>%
  ggplot(aes(reorder(journal,n),n)) + geom_col() + coord_flip() + xlab("") + ylab("Number of publications")

ggsave("output/sul_pub_counts.png", p1, width=6, height=4, dpi = 300)
ggsave("output/sul_units_counts.png", p2, width=6, height=4, dpi = 300)
ggsave("output/sul_units_subs.png", p2a, width=6, height=4, dpi = 300)
ggsave("output/sul_journal_counts.png", p3, width=6, height=12, dpi = 300)

edgelist <- seec_pubs %>% 
  select(title, author) %>%
  filter(!is.na(title)) %>%
  mutate(title = str_replace_all(title, "[^[A-za-z]]", "")) 

g <- graph.edgelist(as.matrix(edgelist))
V(g)$type <- FALSE
V(g)$label.cex <- 2
V(g)$type[V(g)$name %in% edgelist$author] <- TRUE
author_projection <- bipartite.projection(g)$proj2
set.seed(100)
png("output/sul_collab_network.png", width=12, height=8, units = "in", res = 300)
plot.igraph(author_projection, edge.width = 5, edge.color = "black")
dev.off()

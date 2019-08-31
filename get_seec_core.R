# get SEEC core members surnames each year
library(readxl)
library(dplyr)
library(stringr)
library(tidyr)

seec <- read_excel("data/SEEC_data_2014_2017.xlsx", sheet = "List")

seec <- seec %>% mutate(core2014 = str_detect(`2014`, "Core"),
                        core2015 = str_detect(`2015`, "Core"),
                        core2016 = str_detect(`2016`, "Core"),
                        core2017 = str_detect(`2017`, "Core"),
                        core2018 = str_detect(`2018`, "Core"),
                        core2019 = (!is.na(since) & is.na(until)),
                        allaff2014 = !is.na(`2014`),
                        allaff2015 = !is.na(`2015`),
                        allaff2016 = !is.na(`2016`),
                        allaff2017 = !is.na(`2017`),
                        allaff2018 = !is.na(`2018`),
                        allaff2019 = !is.na(`2018`)) # assume same as 2018

seec_core <- seec %>% gather(year, core, core2014:core2019) %>%
  dplyr::select(name = Name, year, core) %>%
  mutate(year = as.numeric(str_remove(year, "core"))) %>%
  mutate(name = str_to_lower(word(name, -1))) %>%
  mutate(name_year = paste(name, year, sep = "_"))

seec_core <- seec_core %>% filter(core) %>% 
  select(name_year) %>% unlist() %>% as.character()

seec_allaff <- seec %>% gather(year, allaff, allaff2014:allaff2019) %>%
  dplyr::select(name = Name, year, allaff) %>%
  mutate(year = as.numeric(str_remove(year, "allaff"))) %>%
  mutate(name = str_to_lower(word(name, -1))) %>%
  mutate(name_year = paste(name, year, sep = "_"))

seec_allaff <- seec_allaff %>% filter(allaff) %>% 
  select(name_year) %>% unlist() %>% as.character() 

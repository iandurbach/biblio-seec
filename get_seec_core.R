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
                        core2019 = (!is.na(since) & is.na(until)))

seec <- seec %>% gather(year, core, core2014:core2019) %>%
  dplyr::select(name = Name, year, core) %>%
  mutate(year = as.numeric(str_remove(year, "core"))) %>%
  mutate(name = str_to_lower(word(name, -1))) %>%
  mutate(name_year = paste(name, year, sep = "_"))

seec_core <- seec %>% filter(core) %>% 
  select(name_year) %>% unlist() %>% as.character()

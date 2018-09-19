library(tidyverse)
library(here)

# generate vector of unique serial numbers for household rosters
# Load training data, add filenames as column for selection later
all_files <- c(list.files(here("data", "Uncoded"), full.names = T), 
                   list.files(here("data", "Coded"), full.names = T))


files <- list()
for(a in 1:length(all_files)){
  files[[a]] <- read_csv(all_files[a])
}
#names(uncoded) <- uncoded_files

all_serials <- unique(combine(unlist(map(files, `[[`, "serial1")), unlist(map(files, `[[`, "serial2"))))
  combine() %>% 
  unique()


# Load Philly data for household rosters
# Two filters: needed columns, and match the serial numbers in the training data
bind_rows(select(read.csv(here("data", "Phl10.csv"), stringsAsFactors = F), 
                                namefrst, namelast, age, sex, relate, bpl, year, serial, pernum),
                         select(read.csv(here("data", "Phl20.csv"), stringsAsFactors = F), 
                                namefrst, namelast, age, sex, relate, bpl, year, serial, pernum),
                         select(read.csv(here("data", "Phl30.csv"), stringsAsFactors = F), 
                                namefrst, namelast, age, sex, relate, bpl, year, serial, pernum),
                         select(read.csv(here("data", "Phl40.csv"), stringsAsFactors = F), 
                                namefrst, namelast, age, sex, relate, bpl, year, serial, pernum)) %>% 
  filter(serial %in% all_serials) %>% 
  mutate(namefrst = str_to_upper(namefrst),
         namelast = str_to_upper(namelast)) %>% 
  write_csv(here("data", "for_rosters.csv"))

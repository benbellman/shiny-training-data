library(tidyverse)
library(here)
library(RecordLinkage)


# load years
phl30 <- read.csv(here("data", "Phl30.csv"), stringsAsFactors = F) %>% as_tibble()
phl40 <- read.csv(here("data", "Phl40.csv"), stringsAsFactors = F) %>% as_tibble()

# replace age values for integer conversion
phl30$age <- recode(phl30$age, 
                    `90 (90+ in 1980 and 1990)` = "90",
                    `100 (100+ in 1960-1970)` = "100",
                    `112 (112+ in the 1980 internal data)` = "112",
                    `Less than 1 year old` = "0")
phl40$age <- recode(phl40$age, 
                    `90 (90+ in 1980 and 1990)` = "90",
                    `100 (100+ in 1960-1970)` = "100",
                    `112 (112+ in the 1980 internal data)` = "112",
                    `Less than 1 year old` = "0")

# keep only vars needed for matching
phl30 <- phl30 %>% 
  select(serial, pernum, namefrst, namelast, bpl, age, sex, relate, year, marst) %>% 
  mutate(uniqueid = paste(serial, pernum, year, sep = "_"),
         byear = year - as.integer(age),
         namefrst = str_to_upper(namefrst),
         namefrst = if_else(namefrst == "WM", "WILLIAM", namefrst),
         namelast = str_to_upper(namelast),
         mi = str_replace(str_extract(namefrst, " [A-Z] ?[A-Z]?$"), " ", ""),
         namefrst = str_replace(namefrst, " [A-Z] ?[A-Z]?$", ""))
phl40 <- phl40 %>% 
  select(serial, pernum, namefrst, namelast, bpl, age, sex, relate, year, marst, race) %>% 
  mutate(uniqueid = paste(serial, pernum, year, sep = "_"),
         byear = year - as.integer(age),
         namefrst = str_to_upper(namefrst),
         namefrst = if_else(namefrst == "WM", "WILLIAM", namefrst),
         namelast = str_to_upper(namelast),
         mi = str_replace(str_extract(namefrst, " [A-Z] ?[A-Z]?$"), " ", ""),
         namefrst = str_replace(namefrst, " [A-Z] ?[A-Z]?$", "")) %>% 
  filter(race == "Black/Negro") %>% 
  select(-race)

names(phl30) <- paste0(names(phl30), "1")
names(phl40) <- paste0(names(phl40), "2")


# Create 10 sets of training data drawing from 100 random household heads in 1940

set.seed(123)

for(x in 0:5){
  # create a training dataset
  chunk <- phl40 %>% 
    filter(relate2 == "Head/householder") %>% 
    sample_n(100)
  
  # set app ID for focal records
  chunk$focal_app_id <- 1:100
  
  for(a in 1:nrow(chunk)){
    # define vars for record being considered
    byearmin <- chunk$byear2[a] - 2
    byearmax <- chunk$byear2[a] + 2
    sex <- chunk$sex2[a]
    bpl <- chunk$bpl2[a]
    namefrst <- chunk$namefrst2[a]
    namelast <- chunk$namelast2[a]
    
    # create initial block of possible matches
    block <- filter(phl30, byear1 <= byearmax & byear1 >= byearmin & sex1 == sex & bpl1 == bpl)
    
    # calculate string similarities
    block$jw_frst <- jarowinkler(namefrst,block$namefrst1)
    block$jw_last <- jarowinkler(namelast,block$namelast1)
    
    # limit block based on JW scores
    block <- filter(block, jw_frst >= 0.75 & jw_last >= 0.8) %>%
      # does changing jw_first limit to 0.6 change # cases with potential matches?
      # 0.6 adds a lot of junk (~1300 potential matches for 86 of 100 records from X1)
      ### 0.7 has some potential (649 potential matches for 80 of 100 records from X1)
      # 0.75 gives 460 potential matches for 74 of 100 records from X1
      # 0.8 gives 400 potential matches for 73 of 100 records from X1
      arrange(desc(jw_frst), desc(jw_last))
    
    # combine and add to training set
    if(nrow(block) > 0){
      
      # set app ID for comparison records
      block$compare_app_id <- 1:nrow(block)
      
      if(exists("training_set") == F){
        training_set <- cbind(chunk[a,], block, row.names = NULL)
      } else {
        training_set <- bind_rows(training_set, cbind(chunk[a,], block, row.names = NULL))
      }
    }
  }
  
  # add match variable for manual coding
  training_set$match <- 0
  
  # Make sure that there are no duplicate rows (no clue hoe it happened)
  training_set <- unique(training_set)
  
  # output the file
  write_csv(training_set, here("data", "Uncoded", paste0("blackover_40_30_", x, ".csv")))
  
  # delete files from this set of training data
  rm(chunk, block, training_set)
}






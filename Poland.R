## load nec. packages

library(tidyverse)
library(readxl)
library(rio)

## load ParlGov-sheets

ParlGov <- "/Users/jacmatbue/Library/Mobile Documents/com~apple~CloudDocs/Uni/BA-Arbeit/Daten/Datensets/parlgov.xlsx"

ParlGov_party <- read_excel(ParlGov, sheet = "party")
ParlGov_election <- read_excel(ParlGov, sheet = "election")
ParlGov_cabinet <- read_excel(ParlGov, sheet = "cabinet")

# filter election_AUT and cabinet_AUT: year >= 1998, election: parliament, seats != 0

election_POL <- ParlGov_election %>%
  filter(country_name == "Poland", as.numeric(substr(election_date, 1, 4)) >= 1998, election_type == "parliament", seats != 0)

cabinet_POL <- ParlGov_cabinet %>%
  filter(country_name == "Poland", as.numeric(substr(election_date, 1, 4)) >= 1998)


## merge election_AUT and cabinet_AUT 

merged_POL <- election_POL %>%
  left_join(cabinet_POL[, c("election_id", "cabinet_party", "party_name", "cabinet_id", "prime_minister")], 
            by = c("election_id", "party_name"))



## add cabinet_party variable (government, 1, 0) and cabinet_id (show all cabients one party has been part of in the respective election)

POL_1 <- merged_POL %>%
  group_by(election_id, party_id) %>%
  mutate(cabinet_party = as.integer(any(cabinet_party == 1)),
         prime_minister = as.integer(any(prime_minister == 1)),
         cabinet_id = paste(cabinet_id[cabinet_party == 1], collapse = ",")) %>%
  ungroup()

# every party must show up only onnce in an election period even though it has been part of multiple cabinets

POL_2 <- POL_1 %>% 
  distinct(election_id, party_id, .keep_all = TRUE) 


## add election_costs 

POL_3 <- POL_2 %>%
  group_by(party_name) %>%
  mutate(election_cost = vote_share - lag(vote_share)) %>%
  ungroup()




## add populist trough PopuList dataset


populist <- import("https://popu-list.org/wp-content/uploads/2020/06/populist-2.0.xlsx") 

selected_variables <- populist %>% 
  select(parlgov_id, populist) 

POL_pop <- left_join(POL_3, selected_variables, by=c("party_id"="parlgov_id")) %>%
  mutate(populist = ifelse(is.na(populist), 0, populist))

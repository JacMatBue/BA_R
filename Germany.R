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

election_GER <- ParlGov_election %>%
  filter(country_name == "Germany", as.numeric(substr(election_date, 1, 4)) >= 1998, election_type == "parliament", seats != 0)

# Bc. CSU and CDU are separate -> transform to Union

election_GER1 <- election_GER %>%
  mutate(party_name_short = ifelse(party_name_short %in% c("CDU", "CSU"), "CDU+CSU", party_name_short),
         party_name = ifelse(party_name %in% c("Christlich Demokratische Union", "Christlich Soziale Union"), "Christlich Demokratische Union / Christlich Soziale Union", party_name),
         party_name_english = ifelse(party_name_english %in% c("Christian Democratic Union", "Christian Social Union"), "Christian Democratic Union / Christian Social Union", party_name_english)
  )%>%
  group_by(country_name_short, country_name, election_type, election_date, party_name_short, party_name, party_name_english) %>%
  summarise(vote_share = sum(vote_share),
            seats = sum(seats),
            seats_total = first(seats_total),
            left_right = first(left_right),
            country_id = first(country_id),
            election_id = first(election_id),
            previous_parliament_election_id = first(previous_parliament_election_id),
            previous_cabinet_id = first(previous_cabinet_id),
            party_id = first(party_id)) %>%
  ungroup() %>%
  relocate(country_name_short, country_name, election_type, election_date, vote_share, 
           seats, seats_total, party_name_short, party_name, party_name_english, left_right, 
           country_id, election_id, previous_parliament_election_id, previous_cabinet_id, party_id)
  





cabinet_GER <- ParlGov_cabinet %>%
  filter(country_name == "Germany", as.numeric(substr(election_date, 1, 4)) >= 1998)


## merge election_AUT and cabinet_AUT -> Problem: sorge dafür dass CDU/CSU in cabinet nicht getrennt sind?

merged_GER <- election_GER1 %>%
  left_join(cabinet_GER[, c("election_id", "cabinet_party", "party_name", "cabinet_id", "prime_minister")], 
            by = c("election_id", "party_name"))


## add cabinet_party variable (government, 1, 0) and cabinet_id (show all cabients one party has been part of in the respective election)

GER_1 <- merged_GER %>%
  group_by(election_id, party_id) %>%
  mutate(cabinet_party = as.integer(any(cabinet_party == 1)),
         prime_minister = as.integer(any(prime_minister == 1)),
         cabinet_id = paste(cabinet_id[cabinet_party == 1], collapse = ",")) %>%
  ungroup()

# every party must show up only onnce in an election period even though it has been part of multiple cabinets

GER_2 <- GER_1 %>% 
  distinct(election_id, party_id, .keep_all = TRUE) 


## add election_costs 

GER_3 <- GER_2 %>%
  group_by(party_name) %>%
  mutate(election_cost = vote_share - lag(vote_share)) %>%
  ungroup()




## add populist trough PopuList dataset


populist <- import("https://popu-list.org/wp-content/uploads/2020/06/populist-2.0.xlsx") 

selected_variables <- populist %>% 
  select(parlgov_id, populist) 

GER_pop <- left_join(GER_3, selected_variables, by=c("party_id"="parlgov_id")) %>%
  mutate(populist = ifelse(is.na(populist), 0, populist))
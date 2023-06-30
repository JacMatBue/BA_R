## load nec. packages

library(tidyverse)
library(readxl)
library(rio)

## load ParlGov-sheets

ParlGov <- "/Users/jacmatbue/Library/Mobile Documents/com~apple~CloudDocs/Uni/BA-Arbeit/Daten/Datensets/parlgov.xlsx"

ParlGov_party <- read_excel(ParlGov, sheet = "party")
ParlGov_election <- read_excel(ParlGov, sheet = "election")
ParlGov_cabinet <- read_excel(ParlGov, sheet = "cabinet")

# filter election_AUT and cabinet_AUT: year >= 1998 (election before 1998), election: parliament, seats != 0

election_ESP <- ParlGov_election %>%
  filter(country_name == "Spain", as.numeric(substr(election_date, 1, 4)) >= 1996, election_type == "parliament", seats != 0)

cabinet_ESP <- ParlGov_cabinet %>%
  filter(country_name == "Spain", as.numeric(substr(election_date, 1, 4)) >= 1996)



## merge election_AUT and cabinet_AUT 

merged_ESP <- election_ESP %>%
  left_join(cabinet_ESP[, c("election_id", "cabinet_party", "party_name", "cabinet_id", "prime_minister")], 
            by = c("election_id", "party_name"))

## add cabinet_party variable (government, 1, 0) and cabinet_id (show all cabients one party has been part of in the respective election)

ESP_1 <- merged_ESP %>%
  group_by(election_id, party_id) %>%
  mutate(cabinet_party = as.integer(any(cabinet_party == 1)),
         prime_minister = as.integer(any(prime_minister == 1)),
         cabinet_id = paste(cabinet_id[cabinet_party == 1], collapse = ",")) %>%
  ungroup()

# every party must show up only onnce in an election period even though it has been part of multiple cabinets

ESP_2 <- ESP_1 %>% 
  distinct(election_id, party_id, .keep_all = TRUE) 


## add election_costs 

ESP_3 <- ESP_2 %>%
  group_by(party_name) %>%
  mutate(election_cost = vote_share - lag(vote_share)) %>%
  ungroup()



## adjust due to party name irregularities: United Podemos -> United Left (IU), Communist Party of Spain 

# Sanchez 3: Podemos ist nicht gelistet

ESP_4 <- ESP_3 %>%
  mutate(cabinet_party = if_else(election_id == 1085 & party_name == "Podemos", 1L, cabinet_party),
         cabinet_id = if_else(election_id == 1085 & party_name == "Podemos", "1605", cabinet_id))





## add populist trough PopuList dataset


populist <- import("https://popu-list.org/wp-content/uploads/2020/06/populist-2.0.xlsx") 

selected_variables <- populist %>% 
  select(parlgov_id, populist) 

ESP_pop <- left_join(ESP_4, selected_variables, by=c("party_id"="parlgov_id")) %>%
  mutate(populist = ifelse(is.na(populist), 0, populist))

  













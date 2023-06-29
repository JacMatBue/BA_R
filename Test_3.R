library(tidyverse)
library(readxl)
library(rio)


### Anmerkungen: BZÖ wird nicht als Regierungspartei gecodet weil sie vorher nicht durch eine Wahl in das Amt gekommen ist


## ParlGov laden, drei Sheets importieren und nach Land und Datum filtern

ParlGov <- "/Users/jacmatbue/Library/Mobile Documents/com~apple~CloudDocs/Uni/BA-Arbeit/Daten/Datensets/parlgov.xlsx"

ParlGov_party <- read_excel(ParlGov, sheet = "party")
ParlGov_election <- read_excel(ParlGov, sheet = "election")
ParlGov_cabinet <- read_excel(ParlGov, sheet = "cabinet")


election_AUT <- ParlGov_election %>%
  filter(country_name == "Austria", as.numeric(substr(election_date, 1, 4)) >= 1998, election_type == "parliament", seats != 0)

cabinet_AUT <- ParlGov_cabinet %>%
  filter(country_name == "Austria", as.numeric(substr(election_date, 1, 4)) >= 1998)




## election_AUT und cabinet_AUT verbinden

merged_AUT <- election_AUT %>%
  left_join(cabinet_AUT[, c("election_id", "cabinet_party", "party_name", "cabinet_id")], 
            by = c("election_id", "party_name"))



## government variable hinzufügen + additional cabinets (um zu zeigen welche Cabinets eine Partei innerhalb einer Wahl zugehörig war)

AUT_1 <- merged_AUT %>%
  group_by(election_id, party_id) %>%
  mutate(cabinet_party = as.integer(any(cabinet_party == 1)),
         cabinet_id = paste(cabinet_id[cabinet_party == 1], collapse = ",")) %>%
  ungroup()


## Filtern der Daten, um jede Partei nur einmal pro Wahl aufzuführen

AUT_2 <- AUT_1 %>% 
  distinct(election_id, party_id, .keep_all = TRUE) 


## Wahlkosten hinzufügen

AUT_final<- AUT_2 %>%
  group_by(party_name) %>%
  mutate(election_cost = vote_share - lag(vote_share)) %>%
  ungroup()



## PopuList laden und verbinden

populist <- import("https://popu-list.org/wp-content/uploads/2020/06/populist-2.0.xlsx") 

selected_variables <- populist %>% 
  select(parlgov_id, populist) 

AUT_pop <- left_join(AUT_final, selected_variables, by=c("party_id"="parlgov_id")) %>%
  mutate(populist = ifelse(is.na(populist), 0, populist))


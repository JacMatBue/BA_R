library(tidyverse)
library(readxl)

## ParlGov laden, drei Sheets importieren und nach Land und Datum filtern

ParlGov <- "/Users/jacmatbue/Library/Mobile Documents/com~apple~CloudDocs/Uni/BA-Arbeit/Daten/Datensets/parlgov.xlsx"

ParlGov_party <- read_excel(ParlGov, sheet = "party")
ParlGov_election <- read_excel(ParlGov, sheet = "election")
ParlGov_cabinet <- read_excel(ParlGov, sheet = "cabinet")


election_AUT <- ParlGov_election %>%
  filter(country_name == "Austria", as.numeric(substr(election_date, 1, 4)) >= 1998, election_type == "parliament", seats != 0)

cabinet_AUT <- ParlGov_cabinet %>%
  filter(country_name == "Austria", as.numeric(substr(election_date, 1, 4)) >= 1998)


## government variable in election_AUT einfügen

merged_AUT <- election_AUT %>%
  left_join(cabinet_AUT[, c("election_id", "cabinet_party", "party_name", "cabinet_id")], by = c("election_id", "party_name"))




merged_AUT_1 <- election_AUT %>% ## alternative
  left_join(cabinet_AUT %>% select(election_id, party_name, cabinet_party, cabinet_id),
            by = c("election_id", "party_name")) 
  


## election_costs hinzufügen -> Problem: zeigt in manchen Fällen 0 wegen Doppelungen

AUT <- merged_AUT %>%
  arrange(party_id, election_date) %>%
  group_by(party_id) %>%
  mutate(row_num = row_number(),
         election_cost = ifelse(row_num == 1, vote_share - lag(vote_share, default = first(vote_share)), NA)) %>%
  select(-row_num)



AUT_1 <- merged_AUT %>%
  group_by(election_id, party_id) %>%
  mutate(government = as.integer(any(cabinet_party == 1))) %>%
  ungroup()

AUT_final <- AUT_1 %>%
  distinct(election_id, party_id, .keep_all = TRUE)




### richtiger COde

merged_AUT <- election_AUT %>%
  left_join(cabinet_AUT[, c("election_id", "cabinet_party", "party_name", "cabinet_id")], by = c("election_id", "party_name"))

AUT_1 <- merged_AUT %>%
  group_by(election_id, party_id) %>%
  mutate(government = as.integer(any(cabinet_party == 1)),
         additional_cabinets = paste(cabinet_id[cabinet_party == 1], collapse = ",")) %>%
  ungroup()

AUT_final <- AUT_1 %>% ### Filtern der Daten, um jede Partei nur einmal pro Wahl aufzuführen
  distinct(election_id, party_id, .keep_all = TRUE) 

AUT_final1 <- AUT_final %>%
  group_by(party_name) %>%
  mutate(election_cost = vote_share - lag(vote_share)) %>%
  ungroup()


regression_model <- lm(election_cost ~ cabinet_party, data = AUT_final1)

summary(regression_model)

## zu klären: warum BZÖ nicht in Regierung -> erklärung: weil 2002 Wahl und da noch nicht aufgenbommen -> lösung: alle Parteien unabhöngig davon ob sie in Parlament sind aufnehmen?

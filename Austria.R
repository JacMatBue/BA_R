library(tidyverse)
library(readxl)
library(lubridate)



Populism_Data <- read_excel("/Users/jacmatbue/Library/Mobile Documents/com~apple~CloudDocs/Uni/BA-Arbeit/Daten/Datensets/xlsx_votes_for_populists_data.xlsx", skip = 1)
MP_Data <- read_excel("/Users/jacmatbue/Library/Mobile Documents/com~apple~CloudDocs/Uni/BA-Arbeit/Daten/Datensets/Manifesto/MPDataset_MPDS2023a.xlsx")
DPI_Data <- read_excel("/Users/jacmatbue/Library/Mobile Documents/com~apple~CloudDocs/Uni/BA-Arbeit/Daten/Datensets/DPI2020/DPI2020.xlsx")
Partyfacts_linked <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/Uni/BA-Arbeit/Daten/BA_R/partyfacts-linked.csv")

## Partyfacts_linked nach Österreich (AUT) filtern und Mapping_Objekt mit Zuordnung erstellen

Partyfacts_linked_AUT <- Partyfacts_linked %>%
  subset(country.x == "AUT" & country.y == "AUT")

party_mapping <- setNames(Partyfacts_linked_AUT$name_short.x, Partyfacts_linked_AUT$name_short.y)


## Manifesto AUT

MP_Austria_1998 <- MP_Data %>%
  subset(countryname == "Austria") %>% 
  mutate(year = substr(edate, 1, 4)) %>%
  subset(as.numeric(substr(edate, 1, 4)) >= 1998) 
  
## DPI AUT

DPI_Austria_1998 <- DPI_Data %>%
  subset(countryname == "Austria") %>%
  subset(as.numeric(substr(year, 1, 4)) >= 1998) %>%
  
  
DPI_Austria_1998_test <- DPI_Austria_1998 %>%
  mutate(
    gov1me = ifelse(gov1me %in% names(party_mapping), party_mapping[gov1me], gov1me),
    gov2me = ifelse(gov2me %in% names(party_mapping), party_mapping[gov2me], gov2me),
    gov3me = ifelse(gov3me %in% names(party_mapping), party_mapping[gov3me], gov3me),
    opp1me = ifelse(opp1me %in% names(party_mapping), party_mapping[opp1me], opp1me),
    opp2me = ifelse(opp2me %in% names(party_mapping), party_mapping[opp2me], opp2me),
    opp3me = ifelse(opp3me %in% names(party_mapping), party_mapping[opp3me], opp3me),
  )


## Variable government (0/1) in MP_Austria_1998 hinzufügen


MP_DPI_Austria_1998 <- MP_Austria_1998 %>%
  left_join(select(DPI_Austria_1998_test, year, gov1me, gov2me, gov3me), by = c("year" = "year")) %>% ## weil DPI zu groß nur bestimmte Variablen von Interesse joinen
  mutate(government = case_when(
    partyabbrev == gov1me | partyabbrev == gov2me | partyabbrev == gov3me ~ 1,
    TRUE ~ 0
  )) %>%
  
  select(-gov1me, -gov2me, -gov3me)  %>% ## gov1me, gov2me, gov3me rausschmeißen
  relocate(government, .before = 10)


















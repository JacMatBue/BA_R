library(tidyverse)
library(readxl)


Populism_Data <- read_excel("/Users/jacmatbue/Library/Mobile Documents/com~apple~CloudDocs/Uni/BA-Arbeit/Daten/Datensets/xlsx_votes_for_populists_data.xlsx", skip = 1)
MP_Data <- read_excel("/Users/jacmatbue/Library/Mobile Documents/com~apple~CloudDocs/Uni/BA-Arbeit/Daten/Datensets/Manifesto/MPDataset_MPDS2023a.xlsx")


Populism_Data <- Populism_Data %>%
  rename(partyname = populist1)

MP_Data_1 <- MP_Data %>%
  mutate(populist = ifelse(partyname %in% Populism_Data$partyname, 1, 0)) %>%
  relocate(populist, .before = 10)


MP_Data_Populist <- MP_Data_1 %>%
  subset(populist == 1)
  

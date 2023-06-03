library(tidyverse)
library(readxl)


Populism_Data <- read_excel("/Users/jacmatbue/Library/Mobile Documents/com~apple~CloudDocs/Uni/BA-Arbeit/Daten/Datensets/xlsx_votes_for_populists_data.xlsx", skip = 1)

## inspect Populism_Data

head(Populism_Data)
summary(Populism_Data)

## filtern nach: populist in power -> unvollständig!

Populism_Power <- Populism_Data %>%
  filter(populistpres == 1 | coalition == 1)


## Load Polity5

Polity5 <- read_excel("/Users/jacmatbue/Library/Mobile Documents/com~apple~CloudDocs/Uni/BA-Arbeit/Daten/Datensets/Polity5.xls", col_names = TRUE)

Polity5_1960 <- Polity5 %>%
  filter(year >= 1960)



## Polity5 und Populismus_Data mergen

merged_data <- merge(Populism_Data, Polity5[, c("country", "year", "polity", "polity2")], by = c("country", "year"), all.x = TRUE)


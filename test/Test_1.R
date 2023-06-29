library(tidyverse)
library(readxl)


Populism_Data <- read_excel("/Users/jacmatbue/Library/Mobile Documents/com~apple~CloudDocs/Uni/BA-Arbeit/Daten/Datensets/xlsx_votes_for_populists_data.xlsx", skip = 1)
DPI_Data <- read_excel("/Users/jacmatbue/Library/Mobile Documents/com~apple~CloudDocs/Uni/BA-Arbeit/Daten/Datensets/DPI2020/DPI2020.xlsx")

DPI_Data <- DPI_Data %>%
  mutate(year = substr(year, 1, 4)) %>%
  rename(country = "countryname")


## inspect Populism_Data

head(Populism_Data)
summary(Populism_Data)

## filtern nach: populist in power -> unvollständig!

Populism_Power <- Populism_Data %>%
  filter(coalition == 1)

Populism_Cleaned <- Populism_Data %>%
  filter(preselection == 1, enteredgov == 1, year >= 1980)
  


## Load Polity5

Polity5 <- read_excel("/Users/jacmatbue/Library/Mobile Documents/com~apple~CloudDocs/Uni/BA-Arbeit/Daten/Datensets/Polity5.xls", col_names = TRUE)

Polity5_1960 <- Polity5 %>%
  filter(year >= 1989) 



## Polity5 und Populismus_Data / DPI mergen

merged_data <- merge(Populism_Data, Polity5[, c("country", "year", "polity", "polity2")], by = c("country", "year"), all.x = TRUE)

merged_data_1 <- merge(DPI_Data, Polity5[, c("country", "year", "polity", "polity2")], by = c("country", "year"), all.x = TRUE)


## Filtern nach: Demokratie (>6=)


Populism <- merged_data %>%
  filter(polity2 >= 6)


DPI_democracy <- merged_data_1 %>%
  filter(polity2 >= 6)
  


## Populism_Europe: nach europäischen Ländern gefiltert und danach nach Power (coalition)


Populism_Europe <- Populism %>%
  filter(country %in% c("Albania", "Austria", "Belgium", "Bulgaria", "Croatia", "Czech Republic", "Denmark", 
                        "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland", "Italy", 
                        "Latvia", "Lithuania", "Moldova", "Netherlands", "Poland", "Portugal", "Romania", "Serbia", 
                        "Slovenia", "Spain", "Sweden", "Switzerland", "Ukraine", "United Kingdom"), 
         year >= 1980, 
         preselection == 0)





## Populism als Excel exportieren um Power-Status zu überarbeiten

install.packages("writexl")
library(writexl)
write_xlsx(Populism, "/Users/jacmatbue/Library/Mobile Documents/com~apple~CloudDocs/Uni/BA-Arbeit/Daten/Datensets/Populismus.xlsx")

write_xlsx(Populism_Europe, "/Users/jacmatbue/Library/Mobile Documents/com~apple~CloudDocs/Uni/BA-Arbeit/Daten/Datensets/Populism_Europe_1.xlsx")
write_xlsx(Populism_Cleaned, "/Users/jacmatbue/Library/Mobile Documents/com~apple~CloudDocs/Uni/BA-Arbeit/Daten/Datensets/Populism_cleaned.xlsx")




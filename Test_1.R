library(tidyverse)
library("readxl")


Populism_Data <- read_excel("/Users/jacmatbue/Library/Mobile Documents/com~apple~CloudDocs/Uni/BA-Arbeit/Daten/Datensets/xlsx_votes_for_populists_data.xlsx")

## inspect Populism_Data

head(Populism_Data)
summary(Populism_Data)
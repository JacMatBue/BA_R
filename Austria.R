library(tidyverse)
library(readxl)
library(lubridate)



Populism_Data <- read_excel("/Users/jacmatbue/Library/Mobile Documents/com~apple~CloudDocs/Uni/BA-Arbeit/Daten/Datensets/xlsx_votes_for_populists_data.xlsx", skip = 1)
MP_Data <- read_excel("/Users/jacmatbue/Library/Mobile Documents/com~apple~CloudDocs/Uni/BA-Arbeit/Daten/Datensets/Manifesto/MPDataset_MPDS2023a.xlsx")
DPI_Data <- read_excel("/Users/jacmatbue/Library/Mobile Documents/com~apple~CloudDocs/Uni/BA-Arbeit/Daten/Datensets/DPI2020/DPI2020.xlsx")


## PartyFacts dataset für einheitliche Parteiennamen

# download and read Party Facts mapping table
file_name <- "partyfacts-mapping.csv"
if( ! file_name %in% list.files()) {
  url <- "https://partyfacts.herokuapp.com/download/external-parties-csv/"
  download.file(url, file_name)
}
partyfacts_raw <- read_csv(file_name, guess_max = 50000)
partyfacts <- partyfacts_raw |> filter(! is.na(partyfacts_id))

# link datasets (select only linked parties)
dataset_1 <- partyfacts |> filter(dataset_key == "manifesto")
dataset_2 <- partyfacts |> filter(dataset_key == "dpi")
link_table <-
  dataset_1 |>
  inner_join(dataset_2, by = c("partyfacts_id" = "partyfacts_id"))

# write results into file with dataset names in file name
file_out <- "partyfacts-linked.csv"
write_csv(link_table, file_out)


Partyfacts_linked <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/Uni/BA-Arbeit/Daten/BA_R/partyfacts-linked.csv")

# Partyfacts_linked nach Österreich (AUT) filtern und Mapping_Objekt mit Zuordnung erstellen

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



## Variable Populist (0/1) hinzufügen (in populist Dataset sind die Namen ungleich der Namen im MP_DPI_Austria_1998 Datenset und müssen daher verändert werden)

Populism_Data <- Populism_Data %>%
  rename(partyname = populist1, partyabbrev = populist3) %>%
  mutate(partyname = case_when(country == "Austria" & partyname == "Freedom Party of Austria" ~ "Austrian Freedom Party", 
                               TRUE ~ partyname),
         partyabbrev = case_when(country == "Austria" & partyabbrev == "FPO" ~ "FPÖ", 
                                 TRUE ~ partyabbrev))
  

MP_DPI_Austria_1998 <- MP_DPI_Austria_1998 %>%
  mutate(populist = ifelse(partyname %in% Populism_Data$partyname, 1, 0))





## MP_DPI_Austria_1998 cleanen und Variablen hinzufügen

# Nur die wichtigen Variablen behalten
AUT_1998 <- MP_DPI_Austria_1998 %>%
  subset(select = c(countryname, year, edate, partyname, partyabbrev, populist, government, pervote, absseat, totseats, rile))
  

# Variable "Wahlkosten" erstellen

AUT_1998_X1 <- AUT_1998 %>%
  group_by(partyname) %>%
  mutate(Wahlkosten = pervote - lag(pervote)) %>%
  ungroup()




## Regressionsanalyse

regression_model1 <- lm(Wahlkosten ~ government + populist, data = AUT_1998_X1)

summary(regression_model1)

## Welche Fälle waren in der Regierung und sind populistisch

populistisch_regierung <- AUT_1998_X1 %>%
  subset(government == 1 & populist == 1) %>%
  print()
























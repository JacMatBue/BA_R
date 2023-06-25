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

# Partyfacts_linked nach Bulgarien (BGR) filtern und Mapping_Objekt mit Zuordnung erstellen

Partyfacts_linked_BGR <- Partyfacts_linked %>%
  subset(country.x == "BGR" & country.y == "BGR")

party_mapping <- setNames(Partyfacts_linked_BGR$name_short.x, Partyfacts_linked_BGR$name_short.y)


## MP BRG

MP_Bulgaria_1998 <- MP_Data %>%
  subset(countryname == "Bulgaria") %>% 
  mutate(year = substr(edate, 1, 4)) %>%
  subset(as.numeric(substr(edate, 1, 4)) >= 1998) 

## DPI AUT

DPI_Bulgaria_1998 <- DPI_Data %>%
  subset(countryname == "Bulgaria") %>%
  mutate(year = substr(year, 1, 4)) %>%
  subset(as.numeric(substr(year, 1, 4)) >= 1998)
  
  
DPI_Bulgaria_1998_test <- DPI_Bulgaria_1998 %>%
  mutate(
    gov1me = ifelse(gov1me %in% names(party_mapping), party_mapping[gov1me], gov1me),
    gov2me = ifelse(gov2me %in% names(party_mapping), party_mapping[gov2me], gov2me),
    gov3me = ifelse(gov3me %in% names(party_mapping), party_mapping[gov3me], gov3me),
    opp1me = ifelse(opp1me %in% names(party_mapping), party_mapping[opp1me], opp1me),
    opp2me = ifelse(opp2me %in% names(party_mapping), party_mapping[opp2me], opp2me),
    opp3me = ifelse(opp3me %in% names(party_mapping), party_mapping[opp3me], opp3me),
  ) 

# zur besseren visualisierung von DPI_Bulgaria

DPI_BGR_visual <- DPI_Bulgaria_1998 %>%
  subset(select = c(year, gov1me, gov1vote, gov2me, gov2vote, gov3me, gov3vote, opp1me, opp1vote, opp2me, opp2vote, opp3me, opp3vote))
  
  
## Variable government (0/1) in MP_Bulgaria_1998 hinzufügen

MP_DPI_Bulgaria_1998 <- MP_Bulgaria_1998 %>%
  left_join(select(DPI_Bulgaria_1998_test, year, gov1me, gov2me, gov3me), by = c("year" = "year")) %>% ## weil DPI zu groß nur bestimmte Variablen von Interesse joinen
  mutate(government = case_when(
    partyabbrev == gov1me | partyabbrev == gov2me | partyabbrev == gov3me ~ 1,
    TRUE ~ 0
  )) %>%
  
  select(-gov1me, -gov2me, -gov3me)  %>% ## gov1me, gov2me, gov3me rausschmeißen
  relocate(government, .before = 10)


## MP_DPI_Bulgaria_1998 cleanen und Variablen hinzufügen

# Nur die wichtigen Variablen behalten
BGR_1998 <- MP_DPI_Bulgaria_1998 %>%
  subset(select = c(countryname, year, edate, partyname, partyabbrev, government, pervote, absseat, totseats, rile))


# Variable "Wahlkosten" erstellen

BGR_1998_X1 <- BGR_1998 %>%
  arrange(partyname, year) %>%
  group_by(partyname) %>%
  mutate(Wahlkosten = pervote - lag(pervote)) %>%
  ungroup()



## Datenset muss nochmal überprüft und nachgebessert werden: 2017/18 war z.B. OP in Regierung
  
  
  
  

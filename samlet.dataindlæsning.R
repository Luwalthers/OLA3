#### Pakker ####
library(tidyverse)
library(dkstat)

#### Realvækst NKHC021 ####
## Hent forbrugsdata via DST API
NKHC021.meta <- dst_meta(table="NKHC021", lang="da")

NKHC021.filter <- list(
  FORMAAAL = "I alt",
  PRISENHED = "2020-priser, kædede værdier",
  SÆSON = "Sæsonkorrigeret",
  Tid = "*")

NKHC021 <- dst_get_data(table = "NKHC021", query = NKHC021.filter)
NKHC021 <- NKHC021[,-c(1:3)]

## Udregner den kvartalvise årlige realvæskt
NKHC021$Realvækst <- (NKHC021$value / dplyr::lag(NKHC021$value, 4) - 1) * 100

#Ændrer fra yyyy-mm-xx til yyyy-Q1
NKHC021$TID <- as.Date(NKHC021$TID)  # Sørg for, at datoen er i Date format

#Udtræk år og kvartal
NKHC021$TID <- paste0(year(NKHC021$TID), "-Q", quarter(NKHC021$TID))


## Skær til fra 2000Q1
NKHC021 <- NKHC021[-1:-40, -2] #tager for den udvalgte periode
rownames(NKHC021) <- NULL



########### Forbrugerforvetninger FORV1 #############
FORV1.meta <- dst_meta(table = "FORV1", lang = "da")

## Liste med relevante filter-variabler.
FORV1.filter <- list(
  INDIKATOR = "*",
  Tid = "*")

FORV1 <- dst_get_data(table = "FORV1", query = FORV1.filter, lang = "da")

## Bredt format
FORV1.samlet <- pivot_wider(FORV1, names_from = INDIKATOR, values_from = value)

## Gruppering og opsummering med udregning af mean
FORV1.samlet$TID <- paste(year(FORV1.samlet$TID), " Q", quarter(FORV1.samlet$TID), sep = "")


FORV1 <- FORV1.samlet %>%
  group_by(TID) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

## Skær til fra 2000Q1
FORV1 <- FORV1[-1:-101,]


#### Dataframe med FTI og Realvækst i lige lange mulige perioder ####
FTI <- data.frame(NKHC021)
FTI <- cbind(FTI, FORV1[1:nrow(FTI),-1])
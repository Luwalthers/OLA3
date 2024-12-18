#Eksamen - stabilitet i indikator

#Indhentning af data
library(tidyverse)
library(dkstat) # henter DKstat pakke

#### Realvækst NKHC021 ####
## Hent forbrugsdata via DST API

NKHC021.meta <- dst_meta(table="NKHC021", lang="da")

NKHC021.filter <- list(
  FORMAAAL = "I alt",
  PRISENHED = "2020-priser, kædede værdier",
  SÆSON = "Sæsonkorrigeret",
  Tid = "*")

NKHC021 <- dst_get_data(table = "NKHC021", query = NKHC021.filter)


NKHC021 <- NKHC021[,-c(1:3)] #fjerner irrelevante kolonner

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

## Ændrer tidskolonne
FORV1.samlet$TID <- paste(year(FORV1.samlet$TID), " Q", quarter(FORV1.samlet$TID), sep = "")

## Gruppering på kvartal og opsummering med udregning af mean
FORV1 <- FORV1.samlet %>%
  group_by(TID) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

## Skær til fra 2000Q1
FORV1 <- FORV1[-1:-101,]


#### OLA 3 - opg 1 ####
#Finder alle mulige kombinationer
FORV1.2 <- FORV1[,-c(1:2)] #ny dataframe uden fti og realvækst

#test af korrelation - testet på alle - negativ korrelation på to nedenstående
cor(NKHC021$Realvækst, FORV1.2$`Priser i dag, sammenlignet med for et år siden`[-100])
cor(NKHC021$Realvækst, FORV1.2$`Arbejdsløsheden om et år, sammenlignet med i dag`[-100])

#Ganger med -1 på spørgsmål 6 og 8, for at ændre værdierne i de to spørgsmål (de er negativ korrelerede til realvæksten)
FORV1.2$`Priser i dag, sammenlignet med for et år siden`= FORV1.2$`Priser i dag, sammenlignet med for et år siden`*-1
FORV1.2$`Arbejdsløsheden om et år, sammenlignet med i dag` = FORV1.2$`Arbejdsløsheden om et år, sammenlignet med i dag`*-1


# Liste til at gemme alle kombinationer
kombinationer_liste <- list()

for (k in 1:12) {
  # Brug combn til at finde alle kombinationer af 'k' spørgsmål
  comb_result <- combn(FORV1.2, k, simplify = FALSE)
  
  # Gem hver kombination som en data frame
  kombinationer_liste[[paste0("komb_", k)]] <- comb_result
}

#Der er i alt 4095 kombinationer af de 12 spørgsmål

##### Opgave 1.2 – R2 og forbrugertillidsindikatorer ####
#Danner samlet indikator af gns på alle rækker 

# Opret en tom liste til at samle gennemsnittene
gennemsnit_liste <- list()

# Iterér gennem hver kombinationsliste (komb_1 til komb_12)
for (k in 1:length(kombinationer_liste)) {
  # Hent hver liste af kombinationer af data frames
  comb_list <- kombinationer_liste[[k]]
  
  # Loop gennem hver kombination i den aktuelle liste
  for (i in 1:length(comb_list)) {
    # Hent den aktuelle kombination (som er en matrix af data)
    current_comb <- comb_list[[i]]
    
    # Konverter data til en matrix, hvis den ikke allerede er det
    current_comb_matrix <- as.matrix(current_comb)
    
    # Beregn række-gennemsnittet
    row_avg <- rowMeans(current_comb_matrix, na.rm = TRUE)
    
    # Tilføj gennemsnittet til vores gennemsnit_liste
    gennemsnit_liste[[paste0("Komb_", k, "_", i)]] <- row_avg
  }
}

# Konverter listen med gennemsnittene til en data frame
gennemsnit_df <- as.data.frame(do.call(cbind, gennemsnit_liste))

# Tilføj kolonnenavne for at identificere hver kombination
colnames(gennemsnit_df) <- names(gennemsnit_liste)


##### Lin.reg på alle kombinationer #####
# Kombiner realvækst fra perioden med gennemsnittene
data_for_regression <- cbind(Realvækst = NKHC021$Realvækst, gennemsnit_df[-nrow(gennemsnit_df),])

# Fjern eventuelle rækker med NA-værdier, da de kan påvirke regressionen
data_for_regression <- na.omit(data_for_regression)

####### Udfør lineær regression på alle kombinationer #######

# Opret en liste til at gemme regression resultater
regression_resultater <- list()

# Loop gennem hver kombination (hver kolonne i gennemsnit_df)
for (kombination in colnames(gennemsnit_df)) {
  # Udfør lineær regression mellem Realvækst og den aktuelle kombination
  model <- lm(Realvækst ~ data_for_regression[[kombination]], data = data_for_regression)
  
  # Gem resultaterne (du kan gemme hele modelobjektet eller bare resuméet)
  regression_resultater[[kombination]] <- summary(model)
}

# Se resultaterne for en specifik kombination
print(regression_resultater[["Komb_1_1"]])  # Skift "Komb_1_1" til en anden kombination efter behov


##### Beregning af R2 for alle kombinationer #####

# Opret en tom liste til at gemme R2-værdierne
r_squared_values <- list()

# Loop gennem regression resultaterne og træk R2 værdien ud for hver model
for (kombination in names(regression_resultater)) {
  # Træk R2 værdien fra modelresuméet
  r_squared_values[[kombination]] <- regression_resultater[[kombination]]$r.squared
}

# Konverter listen med R2 værdier til en data frame for lettere visualisering/analyse
r_squared_df <- as.data.frame(do.call(rbind, r_squared_values))

# Tilføj kolonnenavne (hvis nødvendigt)
colnames(r_squared_df) <- c("R2")

# Print dataframen med R2 værdierne
print(r_squared_df)

####top 10 kombinationer####

# Sorter R2 værdierne for at finde de højeste forklaringsgrader
r_squared_sorted <- r_squared_df[order(-r_squared_df$R2), , drop = FALSE]

# Konverter rownames til en ny kolonne
r_squared_sorted$Kombination <- rownames(r_squared_sorted)

# 10 øverste rækker baseret på R2
top_10_r2 <- r_squared_sorted[1:10, c("Kombination", "R2")]


#Resultat: Ud fra dette resultat kan vi se, at kombinationen "Komb_5_660" har den højeste R2-værdi på 0.4417049

#Beregn korrelationen for alle top 10:

top_10_r2$Correlation <- NA  # tom kolonne

# Loop gennem hver kombination og beregn korrelationen
for (i in seq_len(nrow(top_10_r2))) {
  kombination <- top_10_r2$Kombination[i]  # 
  
  # Beregn korrelationen mellem Realvækst og den aktuelle kombination
  cor_value <- cor(data_for_regression$Realvækst, data_for_regression[[kombination]])
  
  # Gem korrelationen i top_10_r2
  top_10_r2$Correlation[i] <- cor_value
}

#p-værdi for top 10

# Initialiser en liste til at gemme p-værdier
p_values <- list()

# Loop gennem hver kombination
for (kombination in top_10_r2$Kombination) {
  # Hent lm
  model_summary <- regression_resultater[[kombination]]
  
  #  p-værdier fra coefficients
  p_values[[kombination]] <- model_summary$coefficients[2,4]
}

# Konverter listen til en data frame
p_values_df <- do.call(rbind, lapply(names(p_values), function(komb) {
  data.frame(Kombination = komb, t(p_values[[komb]]))
}))

# Vis resultatet
print(p_values_df)

top_10_r2$P.værdi <- p_values_df[,2]


#Spørgsmål i den bedste indikatoren

# Hent kombination nummer 551 i listen over 7-spørgsmålskombinationer (Komb_5_660)
Komb_5_660 <- kombinationer_liste[["komb_5"]][[660]]

# Udskriv navnene på de spørgsmål i kombinationen
sporgsmaal_Komb_5_660 <- colnames(Komb_5_660)

# Print spørgsmålene
sporgsmaal_Komb_5_660



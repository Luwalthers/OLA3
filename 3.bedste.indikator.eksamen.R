## fra opg 1.1 ola 3
print(top_10_r2)

#### Komb_5_660 ####
# Hent kombination nummer 660 i listen over 5-spørgsmålskombinationer (Komb_5_660)
Komb_5_660 <- kombinationer_liste[["komb_5"]][[660]]

# Udskriv navnene på de spørgsmål i kombinationen
sporgsmaal_Komb_5_660 <- colnames(Komb_5_660)

# Print spørgsmålene
sporgsmaal_Komb_5_660

#indeholder følgende spørgmål
        #[1] "Danmarks økonomiske situation i dag, sammenlignet med for et år siden"                       
        #[2] "Priser om et år, sammenlignet med i dag"                                                     
        #[3] "Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr."                             
        #[4] "Regner med at kunne spare op i de kommende 12 måneder"                                       
        #[5] "Familiens økonomiske situation lige nu: kan spare/penge slår til/ bruger mere end man tjener"

#### Graf 1 #####
#får alle R2 værdier for bedste indikator i begge perioder
top_1_p1 <- data.frame(Antal_fjernet = 1:40, Værdi = as.numeric(stab_df["Komb_5_660", 1:40]))
top_1_p1$Periode <- "Periode 1"

top_1_p2 <- data.frame(Antal_fjernet = 1:40, Værdi = as.numeric(stab_df2["Komb_5_660", 1:40]))
top_1_p2$Periode <- "Periode 2"

# Kombiner perioderne
top_1_combined <- rbind(top_1_p1, top_1_p2)

#### graf over perioder ####
library(ggplot2)

ggplot(top_1_combined, aes(x = Antal_fjernet, y = Værdi, color = Periode)) +
  geom_line(size = 1) +  # Linjeplot
  geom_point(size = 2) +  # Punkter på linjen
  geom_text(aes(label = round(Værdi, 3)), vjust = -0.5, size = 3, check_overlap = TRUE) +  # Tilføj værdier
  labs(title = "Udvikling af Komb_5_660 i Periode 1 og 2",
       x = "Antal Kvartaler Fjernet",
       y = "R² Værdi",
       color = "Periode") +
  theme_minimal()

#### Graf 2 #####
# data for begge perioder i dataframe
Komb_5_660_data <- data.frame(
  Periode = c("Periode 1", "Periode 2"),
  Min = c(0.3792827, 0.2264198),
  Max = c(0.4827550, 0.2891540),
  Gns = c(0.4323820, 0.2690247),
  Score = c(4.178721, 4.288324)
)

library(ggplot2)
library(tidyr)

# Konverter data fra wide til long format for lettere plotting
Komb_5_660_data <- pivot_longer(Komb_5_660_data, cols = Min:Gns)

#### barplot ####
ggplot(Komb_5_660_data, aes(x = Periode, y = value, fill = name)) +
  geom_bar(stat = "identity", position = "dodge") +  # Søjlediagram
  geom_text(aes(label = round(value, 3)),  # Tilføj værdierne
            position = position_dodge(width = 0.9),  # Juster for grupperede søjler
            vjust = -0.3,  # Placér værdierne lige over søjlerne
            size = 3) + 
  scale_fill_viridis_d(option = "G") +  # Brug fill i stedet for color
  labs(
    title = "Højest R²-værdier for den bedste indikator i periode 1",
    x = NULL,
    y = "R²-værdi",
    fill = NULL
  ) +
  theme(plot.title = element_text(hjust = 0.5,size = 16,face = "bold"))+
  theme_minimal()


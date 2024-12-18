#grafer 
library(tidyr)
# Beregn gennemsnit af gns for hver periode
mean_gns <- data.frame(Periode_1 = stab_df$Gns, Periode_2 = stab_df2$Gns)

p.gns.1 <- colMeans(stab_df[1:40])
p.gns.2 <- colMeans(stab_df2[1:40])
# Kombiner de to vektorer til en data frame
periode.gns <- data.frame(Periode_1 = p.gns.1, Periode_2 = p.gns.2)


#### Graf - begge perioder R2 ####
gns_kolonner_1 <- colMeans(stab_df_rounded[1:40],na.rm = TRUE)
gns_kolonner_2 <- colMeans(stab_df2_rounded[1:40],na.rm = TRUE)

# Skab en vektor med perioderne (for eksempel P99 til P60)
perioder <- 1:40

# Omdan gennemsnittene til en dataframe
gns_df <- data.frame(
  Antal_fjernet_kvartaler = rep(perioder, 2),  # Repeter perioderne for begge datasæt
  Gns = c(gns_kolonner_1, gns_kolonner_2),  # Sammenkæd de to vektorer
  Periode = rep(c("Periode1", "Periode2"), each = length(gns_kolonner_1))  # Angiv periode type (P1 og P2)
)

# Tjek dataen
head(gns_df)

library(ggplot2)

# Plot de gennemsnitlige R²-værdier for de to perioder
ggplot(gns_df, aes(x = Antal_fjernet_kvartaler, y = Gns, color = Periode)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Højere gennemsnitslig R²-værdi i periode 1",
       x = "Antal kvartaler fjernet fra hhv. 2024Q3 = Periode 1 og 2001Q1 = Periode 2",
       y = "Gennemsnitlig R²-værdi") +
  theme_minimal() +
  scale_color_viridis_d(option = "D") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


plot(gns_kolonner_1, 
     type = "o", # type = "o" giver både punkter og linjer
     main = "Gennemsnit af R²-værdier for hver periode", 
     ylab = "Gennemsnitlig R²-værdi", 
     xlab = "Periode", 
     col = "blue", 
     pch = 16)  # pch = 16 giver fyldte punkter


plot(gns_kolonner_2, 
     type = "o", # type = "o" giver både punkter og linjer
     main = "Gennemsnit af R²-værdier for hver periode", 
     ylab = "Gennemsnitlig R²-værdi", 
     xlab = "Periode", 
     col = "blue", 
     pch = 16)  # pch = 16 giver fyldte punkter




#### enkelte spg over periode ####

print(spg_1_12) #R2 for alle spg enkeltvis
spg_1_12$Spørgsmål <- 1:12 #tilføjer spørgssmåls nr

# Pivotér dataene til long format
spg_1_12_long <- spg_1_12 %>%
  pivot_longer(cols = starts_with("P"),   # Alle kolonner der starter med "P"
               names_to = "Antal_fjernet_kvartaler",      # Navnet på den nye kolonne, der indeholder perioderne (P99, P98, osv.)
               values_to = "R2")      # Navnet på den nye kolonne, der indeholder værdierne

# Gentag perioderne fra 1 til 40 over alle rækker
spg_1_12_long$Antal_fjernet_kvartaler <- rep(1:40, length.out = nrow(spg_1_12_long))

# Opdeler spørgsmålene i mikro og makro
spg_1_12_long$Type <- ifelse(spg_1_12_long$Spørgsmål %in% c(1, 2, 9, 11, 12), "Mikro", "Makro")
          #mikro og makrospg er tidligere defineret 
          #mikro = 1,2,9,11,12
          #makro = 3,4,5,6,7,8,10

library(ggplot2)
# Visualisering med farver for Mikro og Makro
ggplot(spg_1_12_long, aes(x = Antal_fjernet_kvartaler, y = R2, group = Spørgsmål, color = Type)) +
  geom_line(size = 0.5) +  # Plot linjerne
  geom_point(size = 0.5) +  # Punkter på linjerne
  # Tilføj etiketter kun ved slutningen af linjen (brug af max Periode)
  geom_text(data = subset(spg_1_12_long, Antal_fjernet_kvartaler == 40), aes(label = Spørgsmål), 
            size = 3, vjust = 0.5, colour = "black", fontface = "bold", hjust = -1 ) +  # Juster etiketterne så de er ved siden af linjen
  scale_color_manual(values = c("Mikro" = "red", "Makro" = "blue")) +  # Rød for Mikro og Blå for Makro
  labs(title = "Udvalgte makroøkonomiske spørgsmål kan trække R²-værdier ned i kombinationsindikatorerne",
       x = "Antal fjernet kvartaler fra 2024Q3",
       y = "R²-værdi",
       color = "Spørgsmålstype") +
  theme_minimal()+
  theme(legend.position = "top",
        legend.title = element_text(face = "bold"),  # Gør legenden titel fed
        axis.title = element_text(face = "bold"),
        title = element_text(face = "bold")) 



#####top 15 bedste indikator ift stabilitet ####

head(stab_results)
library(ggplot2)
library(reshape2)
udvalgte <- stab_results[stab_results$score_rank == 1 | stab_results$kombination == "Komb_4_354", ] #udvalgt nr 1 rank og en med 4 spørg

# Konverter data fra wide til long format for lettere plotting
udvalgte_long <- pivot_longer(udvalgte, cols = min:Gns)

# Plot med værdier
ggplot(udvalgte_long, aes(x = reorder(kombination, score_rank), y = value, fill = name)) +
  geom_bar(stat = "identity", position = "dodge") +  # Søjlediagram
  geom_text(aes(label = round(value, 2)),  # Tilføj værdierne
            position = position_dodge(width = 0.9),  # Juster for grupperede søjler
            vjust = -0.3,  # Placér værdierne lige over søjlerne
            size = 3, 
            colour = "black",
            fontface = "bold") +  # Størrelse på teksten
  geom_text(aes(label = ifelse(name == "max", paste("Stabilitetsscore Rank:", score_rank), "")),  # Vis rangeringen kun for min-værdien
            position = position_dodge(width = 1.5),  # Juster for grupperede søjler
            vjust = -1.5,  # Placér rangeringen lidt længere oppe
            size = 3.5, color = "darkred", fontface = "bold")  +  # Farve på rangeringen for at gøre det tydeligere
  labs(title = "Ubetydelig forskel i R²-værdier for højest rangerede ",
       x = NULL,
       y = "R²-værdi",
       fill = NULL) +
  scale_fill_viridis_d(option = "G") +
  theme(
    plot.title = element_text(
      hjust = 0.5,         
      size = 125,           
      face = "bold"     
    )) +
  theme_minimal()

#### udvalgte indikatorer barplot ####
#inklusiv gamle bedste indikator
udvalgte2 <- stab_results[stab_results$score_rank == 1 | stab_results$kombination == c("Komb_4_354", "Komb_5_660"), ] #udvalgt nr 1 rank, en med 4 spørgs og tidligere indikator

# Konverter data fra wide til long format for lettere plotting
udvalgte_long2 <- pivot_longer(udvalgte2, cols = min:Gns)

library(ggplot2)
ggplot(udvalgte_long2, aes(x = reorder(kombination, score_rank), y = value, fill = name)) +
  geom_bar(stat = "identity", position = "dodge") +  # Søjlediagram
  geom_text(aes(label = round(value, 2)),  # Tilføj værdierne
            position = position_dodge(width = 0.9),  # Juster for grupperede søjler
            vjust = 1.5,  # Placér værdierne lige over søjlerne
            size = 3, 
            colour = "black",
            fontface = "bold") +  # Størrelse på teksten
  geom_text(aes(label = ifelse(name == "max", paste("Stabilitetsscore Rank:", score_rank), "")),  # Vis rangeringen kun for min-værdien
            position = position_dodge(width = 1.5),  # Juster for grupperede søjler
            vjust = -1,  # Placér rangeringen lidt længere oppe
            size = 3, color = "red", fontface = "bold")  +  # Farve på rangeringen for at gøre det tydeligere
  labs(title = "Forholdet mellem min- og maxværdi er afgørende for stabilitetsrangeringen",
       x = NULL,
       y = "R²-værdi",
       fill = NULL) +
  theme_minimal()



#stabilitet i de enkelte spørgsmål
Spørgsmål <- data.frame(Spørgsmål = colnames(FORV1.2), 
                        NR = 1:12)
Spørgsmål$Type <- ifelse(Spørgsmål$NR %in% c(1, 2, 9, 11, 12), "Mikro", "Makro")
#mikro og makrospg er tidligere defineret 
#mikro = 1,2,9,11,12
#makro = 3,4,5,6,7,8,10

spg_1_12_stabilitet <- spg_1_12
spg_1_12_stabilitet$min <- apply(spg_1_12_stabilitet[, 1:40], 1, min)
spg_1_12_stabilitet$max <- apply(spg_1_12_stabilitet[, 1:40], 1, max)

spg_1_12_stabilitet$gns <- rowMeans(spg_1_12_stabilitet) # tilføjer gns R2 værdi som kolonne

# stabilitetsscore
spg_1_12_stabilitet <- spg_1_12_stabilitet %>% 
  mutate(Score = gns/(max - min)) %>% 
  arrange(desc(Score))  # Sortér efter højeste score først

spg_1_12_stabilitet <- spg_1_12_stabilitet[,41:45]
Spørgsmål$Type <- ifelse(Spørgsmål$NR %in% c(1, 2, 9, 11, 12), "Mikro", "Makro")
apply(FORV1.2, 2, var, na.rm = TRUE) #meget lille variation i nogle spørgsmål



### bedste alternativ indikator ####
#inklusiv gamle bedste indikator
stab_df$kombination <- rownames(stab_df)
Komb_4_354_data <- data.frame(Kvartaler_fjernet = 1:40)
Komb_4_354_data$R2 <- (t(stab_df[stab_df$kombination == c("Komb_4_354"),1:40 ]))


# Linjeplot af gennemsnittet for hver kolonne med y-akse fra 0 til 1
plot(Komb_4_354_data, 
     type = "o",  # type = "o" giver både punkter og linjer
     main = "Gennemsnit af R²-værdier for hver periode", 
     ylab = "Gennemsnitlig R²-værdi", 
     xlab = "Periode", 
     col = "blue", 
     pch = 16)  # pch = 16 giver fyldte punkter  # Angiv y-aksegrænser

library(ggplot2)

# Forbered data til ggplot
Komb_4_354_data <- data.frame(
  Kvartaler_fjernet = 1:40,
  R2 = as.numeric(t(stab_df[stab_df$kombination == "Komb_4_354", 1:40]))
)

# Definér linjeværdier
min_val <- 0.36091
max_val <- 0.42326
mean_val <- 0.3942645

# Lav plottet med ggplot
ggplot(Komb_4_354_data, aes(x = Kvartaler_fjernet, y = R2)) +
  geom_line(color = "blue", size = 0.5) +                  # Linje
  geom_point(color = "blue", size = 2) +                # Punkter
  geom_hline(yintercept = min_val, color = "red", linetype = "dashed", size = 1) +  # Min linje
  geom_hline(yintercept = max_val, color = "green", linetype = "dashed", size = 1) + # Max linje
  geom_hline(yintercept = mean_val, color = "purple", linetype = "dashed", size = 1) + # Gns linje
  labs(
    title = "Gennemsnit af R²-værdier for hver periode",
    x = "Antal fjernede kvartaler",
    y = "Gennemsnitlig R²-værdi"
  ) +
  scale_y_continuous(limits = c(0.30, 0.50)) +            # Sæt y-aksegrænser
  theme_minimal() +                                     # Pæn minimalistisk tema
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),  # Centrer titel og justér størrelse
    axis.title = element_text(size = 12),              # Størrelse på akse-titler
    axis.text = element_text(size = 10)                # Størrelse på akse-tekst
  ) +
  annotate("text", x = 35, y = min_val, label = "Min", color = "red", hjust = -0.1, size = 4) +
  annotate("text", x = 35, y = max_val, label = "Max", color = "green", hjust = -0.1, size = 4) +
  annotate("text", x = 35, y = mean_val, label = "Gns", color = "purple", hjust = -0.1, size = 4)



####12 højest rangeret ####
# Forbered data til ggplot- henter data fra stab_df

top_12 <- data.frame(
  stab_df[stab_df$score_rank %in% 1:12, 1:40]
)

top_12 <- as.data.frame(t(top_12))
top_12$Kvartaler_fjernet <- 1:40


# Konverter data fra wide til long format for lettere plotting
top_12_long <- pivot_longer(top_12, cols = 1:12, names_to = "Kombination", values_to = "R2")

# Lav plottet med ggplot
ggplot(top_12_long, aes(x = Kvartaler_fjernet, y = R2, color = Kombination)) +
  geom_line(size = 0.7) +             
  geom_point(size = 1.5) +             
  scale_color_viridis_d(option = "C") + 
  labs(
    title = "Minimal variation i R²-værdier for de 12 bedste indikatorer",
    x = "Antal fjernede kvartaler fra 2024Q3",
    y = "R²-værdi",
    color = "Kombination"              # Label til farve-legenden
  ) +
  scale_y_continuous(limits = c(0.35, 0.5)) +
  scale_x_continuous(breaks = seq(5, 40, 5)) +# Y-akse begrænsning
  theme_minimal() +                        
  theme(
    plot.title = element_text(
      hjust = 0.5,          
      size = 16,             
      face = "bold"        
    ),
    axis.title = element_text(
      size = 14,            
      face = "bold"          
    ),
    axis.text = element_text(
      size = 12              # Justér størrelsen på akse-tekster
    ),
    legend.title = element_text(
      size = 12,           
      face = "bold"      
    ),
    legend.text = element_text(
      size = 10              # Justér størrelsen på legend-tekster
    ),
    legend.position = "right", # Placér legend til højre
    legend.background = element_rect(
      fill = "white",    
      color = "black"        
    ),
    panel.grid.major = element_line(
      color = "grey80"       # Lys gitterlinje for klarere visning
    ),
    panel.grid.minor = element_blank(), # Fjern mindre gitterlinjer for et renere udseende
    panel.background = element_rect(
      fill = "white",        
      color = "grey80"       
    )
  )






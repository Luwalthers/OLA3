#Eksamen - stabilitet i indikator
library(tidyverse)
####1. nested-loop####
#Her fjernes kvartaler fra 2024Q3 og 40 kvartaler tilbage

#data frame for alle kombinationer 
stab_gns <- data_for_regression

stab_df <- data.frame() # tom data frame til at samle R2 resultaterne

for (j in 2:ncol(stab_gns)) {  #1. loop - iterer gennem alle kombinationer 
  for (i in 99:60) { #2. loop - fjerner kvartalsvis og udregner R2 for periode
    
    temp_df <- stab_gns[1:i, c(1, j)]  # #midlertidig dataframe med Realvækst og den j'te indikator-kolonne
    
    model <- lm(Realvækst ~ ., data = temp_df) #lineær regression med Realvækst som y og den valgte komb som x
    
    stab_df[100 - i, j - 1] <- summary(model)$r.squared #gemmer R2 værdi i stab_df
  }
}

stab_df <- data.frame(t(stab_df)) #Transponer data frame

colnames(stab_df) <- paste0("P", 99:60) # #Tilføj kolonnenavne (periode fra 99 til 60)


rownames(stab_df) <- colnames(stab_gns)[2:ncol(stab_gns)] # rækkenavne med indikator / kombinations-navnene

stab_df <- round(stab_df[,1:40], 5)

spg_1_12 <- stab_df[1:12,] #til senere analyse af spørgsmål enkeltvis

#finder min og max værdi
stab_df$min <- apply(stab_df[, 1:40], 1, min)
stab_df$max <- apply(stab_df[, 1:40], 1, max)

#finder gns
stab_df$Gns <- rowMeans(stab_df) # tilføjer gns R2 værdi som kolonne

# Opret en stabilitetsscore
stab_df <- stab_df %>% 
  mutate(Score = Gns/(max - min)) %>% 
  arrange(desc(Score))  # Sortér efter højeste score først

#opretter rangering efter score
stab_df$score_rank <- rank(-stab_df$Score)

#opretter rangering efter score
stab_df$gns_rank <- rank(-stab_df$Gns)

stab_df$kombination <- rownames(stab_df)

# Se data frame med resultaterne
View(stab_df)

stab_results <- stab_df[, 41:47]
stab_results$kombination <- rownames(stab_results)




####2. nested-loop#### 
#Her fjernes kvartaler fra 2001Q1 og 40 kvartaler frem
#data frame for alle kombinationer 
stab_gns2 <- data_for_regression 

# Opret en tom data frame til at samle R2 resultaterne

stab_df2 <- data.frame()

for (j in 2:ncol(stab_gns2)) {  #1. loop - iterer gennem alle kombinationer 
  for (i in 1:40) { #2. loop - fjerner kvartalsvis og udregner R2 for periode
    
    temp_df <- stab_gns2[i:99, c(1, j)]  
    
    model <- lm(Realvækst ~ ., data = temp_df)
    
    stab_df2[i, j - 1] <- summary(model)$r.squared
  }
}

stab_df2 <- data.frame(t(stab_df2)) #T data frame 

colnames(stab_df2) <- paste0("P", 1:40) # #Tilføj kolonnenavne (periode fra 1 til 40)

rownames(stab_df2) <- colnames(stab_gns2)[2:ncol(stab_gns2)] # rækkenavne med indikator / kombinations-navnene

stab_df2 <- round(stab_df2[,1:40], 5) #affrunder til 5 decimaler

#periode 2
stab_df2$min <- apply(stab_df2[, 1:40], 1, min)
stab_df2$max <- apply(stab_df2[, 1:40], 1, max)

stab_df2$Gns <- rowMeans(stab_df2) # tilføjer gns R2 værdi som kolonne

# stabilitetsscore
stab_df2 <- stab_df2 %>% 
  mutate(Score = Gns/(max - min)) %>% 
  arrange(desc(Score))  # Sortér efter højeste score først

#opretter rangering efter score
stab_df2$score_rank <- rank(-stab_df2$Score)

#opretter rangering efter gns
stab_df2$gns_rank <- rank(-stab_df2$Gns)

stab_df2$kombination <- rownames(stab_df2)

# Se data frame med resultaterne
View(stab_df2)

stab_results2 <- stab_df2[, 41:47]
stab_results2$kombination <- rownames(stab_results2)

#### stablitet af komb_5_660 ####
# Tilføj en ny kolonne 'Periode' til hvert datasæt
stab_results$Periode <- "Periode1"
stab_results2$Periode <- "Periode2"

# Kombiner de to datasæt
samlet_results <- bind_rows(stab_results, stab_results2)

# Konverter data til long format
samlet_results_long <- samlet_results %>%
  pivot_longer(cols = min:Score)
               
# Se resultatet
head(samlet_results_long)

#### alternative indikatorer ####

Komb_4_354 <- colnames(kombinationer_liste[["komb_4"]][[354]])

Komb_4_354_result <- Spørgsmål[match(Komb_4_354, Spørgsmål$Spørgsmål), ] 


#rank 1 = Komb_7_344

Komb_7_344 <- colnames(kombinationer_liste[["komb_7"]][[344]])
#[1] "Danmarks økonomiske situation i dag, sammenlignet med for et år siden"    
#[2] "Priser om et år, sammenlignet med i dag"                                  
#[3] "Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr."          
#[4] "Anser det som fornuftigt at spare op i den nuværende økonomiske situation"

Spørgsmål[match(Komb_7_344, Spørgsmål$Spørgsmål), ] #mange makro


#### bedste alternativ komb_4_354 ####
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
  geom_hline(yintercept = min_val, color = "darkred", linetype = "dashed", size = 0.7) +  # Min linje
  geom_hline(yintercept = max_val, color = "darkgreen", linetype = "dashed", size = 0.7) + # Max linje
  geom_hline(yintercept = mean_val, color = "black", linetype = "dashed", size = 0.5) + # Gns linje
  labs(
    title = "Komb_4_354 holder et stabilt niveau af R²-værdier",
    x = "Antal fjernede kvartaler fra 2024Q3",
    y = "R²-værdi"
  ) +
  scale_y_continuous(limits = c(0.30, 0.50)) +            # Sæt y-aksegrænser
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
    )) +
  annotate("text", x = 35, y = min_val, label = "Min", color = "darkred", vjust = -1, size = 4) +
  annotate("text", x = 35, y = max_val, label = "Max", color = "darkgreen", vjust = -1, size = 4) +
  annotate("text", x = 35, y = mean_val, label = "Gns", color = "black", vjust = -1, size = 4)












####begge perioder samlet ####

# Runde værdierne til 5 decimaler for overskuelighed
stab_df_rounded <- round(stab_df[,1:40], 5)
stab_df2_rounded <- round(stab_df2[,1:40], 5)

samlet_stab_df <- cbind(stab_df_rounded[,1:40], stab_df2_rounded[,1:40])

samlet_stab_df$Gns <- rowMeans(samlet_stab_df)

samlet_stab_df$min <- apply(samlet_stab_df[, 1:80], 1, min)
samlet_stab_df$max <- apply(samlet_stab_df[, 1:80], 1, max)

# Opret en stabilitetsscore
samlet_stab_df <- samlet_stab_df %>% 
  mutate(Score1 = Gns/(max - min)) %>% 
  arrange(desc(Score1))  # Sortér efter højeste score først

#opretter rangering efter score
samlet_stab_df$score_rank <- rank(-samlet_stab_df$Score1)

#### Kvartilstest ####

#Find 10%- og 90%-kvartiler for R2 for hver kombination
Kvartilstest <- t(apply(samlet_stab_df[, 1:80], 1, quantile, probs = c(0.1, 0.9)))

# Tilføj kvartilerne som nye kolonner i data framen
samlet_stab_df$Q10 <- Kvartilstest[, 1]
samlet_stab_df$Q90 <- Kvartilstest[, 2] 

#Opret en stabilitetsscore
samlet_stab_df <- samlet_stab_df %>%
  mutate(Score2 = Gns/(Q90 - Q10)) %>% 
  arrange(desc(Score2))  # Sortér efter højeste score først
samlet_stab_df$score_rank2 <- rank(-samlet_stab_df$Score2)


# Se data frame med resultaterne
samlet_stab_results <- samlet_stab_df[, 81:89]

samlet_stab_results$kombination <- rownames(samlet_stab_results)


top_5_score <- stab_results[1:5,]
top_5_score$Kombination <- rownames(top_5_score)






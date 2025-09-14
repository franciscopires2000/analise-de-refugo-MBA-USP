#ANALISE EXPLORATORIA DE SERIES TEMPORAIS

#Bibliotecas e Parametros Inciais ##############################################
library(tidyverse)
library(ggplot2)
library(gglinedensity)
library(dplyr)

#Agregar Dados #################################################################
bd_PRENSA_1$ID <- paste0(bd_PRENSA_1$Turno_ID, "_", bd_PRENSA_1$MAQUINA)
bd_PRENSA_2$ID <- paste0(bd_PRENSA_2$Turno_ID, "_", bd_PRENSA_2$MAQUINA)
bd_PRENSA_3$ID <- paste0(bd_PRENSA_3$Turno_ID, "_", bd_PRENSA_3$MAQUINA)

bd_PRENSAS <- bind_rows(bd_PRENSA_1, bd_PRENSA_2, bd_PRENSA_3)

#Padronizacao do Tempo com 1:n em cada série
df_padronizado <- bd_PRENSAS %>%
  arrange(ID, DateTime) %>%
  group_by(ID) %>%
  mutate(tempo_padrao = row_number()) %>%
  ungroup()

#Mapa de Calor - PLATO INFERIOR ################################################

#Definir limites do Gráfico
limites_y <- quantile(df_padronizado$Temp_PI, probs = c(0.025, 0.975), na.rm = TRUE)

#Gráfico de Calor
ggplot(df_padronizado, aes(x = tempo_padrao, y = Temp_PI, group = ID)) +
  stat_line_density(aes(fill = after_stat(ndensity)),
                    bins = 2500, na.rm = TRUE, drop = FALSE) +
  scale_fill_viridis_c(option = "plasma", name = "Densidade") +
  labs(title = "Gráfico de Densidade (DenseLines) da Temperatura do Platô Inferior das Prensas",
       x = "Tempo (Posição na Série)",
       y = "Temperatura ºC") +
  coord_cartesian(ylim = limites_y) +
  theme_minimal()

#Mapa de Calor - PLATO SUPERIOR ################################################

#Definir limites do Gráfico
limites_y <- quantile(df_padronizado$Temp_PS, probs = c(0.025, 0.975), na.rm = TRUE)

#Gráfico de Calor
ggplot(df_padronizado, aes(x = tempo_padrao, y = Temp_PS, group = ID)) +
  stat_line_density(aes(fill = after_stat(ndensity)),
                    bins = 2500, na.rm = TRUE, drop = FALSE) +
  scale_fill_viridis_c(option = "plasma", name = "Densidade") +
  labs(title = "Gráfico de Densidade (DenseLines) da Temperatura do Platô Superior das Prensas",
       x = "Tempo (Posição na Série)",
       y = "Temperatura ºC") +
  coord_cartesian(ylim = limites_y) +
  theme_minimal()

#Gráfico de Linha - Temperatura Média ##########################################

#Calcular médias ao longo do tempo (desconsiderar NA)
serie_media_dupla <- df_padronizado %>%
  group_by(tempo_padrao) %>%
  summarise(
    Temp_PI = mean(Temp_PI, na.rm = TRUE),
    Temp_PS = mean(Temp_PS, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  tidyr::pivot_longer(cols = c(Temp_PI, Temp_PS), 
                      names_to = "Plato", 
                      values_to = "Temperatura")

#Gráfico das Temperaturas Médias
ggplot(serie_media_dupla, aes(x = tempo_padrao, y = Temperatura, color = Plato)) +
  geom_line(size = 1.2) +
  scale_color_manual(
    values = c("Temp_PI" = "#007FFF", "Temp_PS" = "#e00000"),
    labels = c("Platô Inferior", "Platô Superior")
  ) +
  labs(
    title = "Médias das Temperaturas dos Platôs ao Longo do Tempo",
    x = "Tempo (Posição na Série)",
    y = "Temperatura Média (ºC)",
    color = "Platô"
  ) +
  theme_minimal()

rm(df_padronizado, serie_media_dupla, limites_y)
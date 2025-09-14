#SPLINE / SUAVIZAÇÃO DAS SERIES TEMPORAIS

#Bibliotecas e Parametros Inciais ##############################################
library(tidyverse)
library(ggplot2)
library(gglinedensity)
library(dplyr)
library(purrr)
library(tidyr)

#Agregar Dados #################################################################

#Padronizacao do Tempo com 1:n em cada série
df_padronizado <- bd_PRENSAS %>%
  arrange(ID, DateTime) %>%
  group_by(ID) %>%
  mutate(tempo_padrao = row_number()) %>%
  ungroup()

# Aplicar smooth.spline por ID - PLATO INFERIOR ################################
df_suavizado_PI <- df_padronizado %>%
  group_by(ID) %>%
  filter(!is.na(Temp_PI)) %>%
  group_modify(~ {
    dados <- .
    ajuste <- smooth.spline(x = dados$tempo_padrao,
                            y = dados$Temp_PI,
                            all.knots = TRUE,
                            cv = TRUE,
                            control.spar = list(low = 0.3, high = 0.7))
    tibble(
      tempo_padrao = dados$tempo_padrao,
      Temp_PI_suavizado = predict(ajuste, dados$tempo_padrao)$y
    )
  }) %>%
  ungroup()

# Aplicar smooth.spline por ID - PLATO SUPERIOR ################################
df_suavizado_PS <- df_padronizado %>%
  group_by(ID) %>%
  filter(!is.na(Temp_PS)) %>%
  group_modify(~ {
    dados <- .
    ajuste <- smooth.spline(x = dados$tempo_padrao,
                            y = dados$Temp_PS,
                            all.knots = TRUE,
                            cv = TRUE,
                            control.spar = list(low = 0.3, high = 0.7))
    tibble(
      tempo_padrao = dados$tempo_padrao,
      Temp_PS_suavizado = predict(ajuste, dados$tempo_padrao)$y
    )
  }) %>%
  ungroup()

#Grafico de Calor ##############################################################

#Plato Superior ################################################################

#Definir limites do Gráfico
limites_y <- quantile(df_suavizado_PS$Temp_PS_suavizado, probs = c(0.025, 0.975), na.rm = TRUE)

#Gráfico de Calor
ggplot(df_suavizado_PS, aes(x = tempo_padrao, y = Temp_PS_suavizado, group = ID)) +
  stat_line_density(aes(fill = after_stat(ndensity)),
                    bins = 2500, na.rm = TRUE, drop = FALSE) +
  scale_fill_viridis_c(option = "plasma", name = "Densidade") +
  labs(title = "Gráfico de Densidade (DenseLines) da Temperatura do Platô Superior das Prensas - Suavizado",
       x = "Tempo (Posição na Série)",
       y = "Temperatura ºC") +
  coord_cartesian(ylim = limites_y) +
  theme_minimal()

#Plato Inferior ################################################################

#Definir limites do Gráfico
limites_y <- quantile(df_suavizado_PI$Temp_PI_suavizado, probs = c(0.025, 0.975), na.rm = TRUE)

#Gráfico de Calor
ggplot(df_suavizado_PI, aes(x = tempo_padrao, y = Temp_PI_suavizado, group = ID)) +
  stat_line_density(aes(fill = after_stat(ndensity)),
                    bins = 2500, na.rm = TRUE, drop = FALSE) +
  scale_fill_viridis_c(option = "plasma", name = "Densidade") +
  labs(title = "Gráfico de Densidade (DenseLines) da Temperatura do Platô Inferior das Prensas - Suavizado",
       x = "Tempo (Posição na Série)",
       y = "Temperatura ºC") +
  coord_cartesian(ylim = limites_y) +
  theme_minimal()

#Gráfico de Linha - Temperatura Média ##########################################

#Juntar dataframes suavizados
df_suavizado_total <- full_join(df_suavizado_PI, df_suavizado_PS,
                                by = c("ID", "tempo_padrao"))

#Calcular a média suavizada por tempo_padrao
serie_media_suavizada <- df_suavizado_total %>%
  group_by(tempo_padrao) %>%
  summarise(
    Temp_PI = mean(Temp_PI_suavizado, na.rm = TRUE),
    Temp_PS = mean(Temp_PS_suavizado, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(Temp_PI, Temp_PS),
               names_to = "Plato",
               values_to = "Temperatura")

#Gráfico com as curvas suavizadas médias
ggplot(serie_media_suavizada, aes(x = tempo_padrao, y = Temperatura, color = Plato)) +
  geom_line(size = 1.2) +
  scale_color_manual(
    values = c("Temp_PI" = "#007FFF", "Temp_PS" = "#e00000"),
    labels = c("Platô Inferior", "Platô Superior")
  ) +
  labs(
    title = "Médias Suavizadas das Temperaturas dos Platôs ao Longo do Tempo - Suavizados",
    x = "Tempo (Posição na Série)",
    y = "Temperatura Média Suavizada (ºC)",
    color = "Platô"
  ) +
  theme_minimal()

bd_suavizado_padronizado = df_suavizado_total
rm(serie_media_suavizada, df_suavizado_PI, df_suavizado_PS, df_suavizado_total, df_padronizado, limites_y)
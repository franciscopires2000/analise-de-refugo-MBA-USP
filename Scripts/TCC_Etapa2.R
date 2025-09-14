#ANALISE EXPLORATORIA DE DADOS

#Bibliotecas e Parametros Inciais ##############################################
library(GGally)
library(ggplot2)
library(scales)
library(FSA)
library(dplyr)

################################################################################
#A Refugo é influenciado pela Prensa? ##########################################
################################################################################

#Verificar se a Prensa esta influenciando no Refugo

#Box-Plot
ggplot(bd_APONTAMENTO, aes(x = MAQUINA, y = Taxa_PERDA_POR_EMENDA_DE_MATERIAL)) +
  geom_boxplot(fill = "#007FFF", color = "black", alpha = 0.7, outlier.color = "blue") +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(
    title = "Taxa de Refugo por Emenda de Material por Prensa",
    x = "Prensas",
    y = "Taxa de Refugo"
  ) +
  theme_minimal(base_size = 13)

#Gráfico de Violino
ggplot(bd_APONTAMENTO, aes(x = MAQUINA, y = Taxa_PERDA_POR_EMENDA_DE_MATERIAL)) +
  geom_violin(trim = FALSE, alpha = 0.4, color = NA, fill = "#007FFF") +
  geom_boxplot(fill = "#007FFF", width = 0.2, alpha = 0.8, color = "black", outlier.color = "blue", outlier.size = 0.75) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(
    title = "Distribuição da Taxa de Refugo por Emenda de Material por Prensa",
    x = "Prensas",
    y = "Taxa de Refugo"
  ) +
  scale_fill_manual(values = rep("#007FFF", 10)) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

#Teste de normalidade por Prensa - SHAPIRO
shapiro_result <- bd_APONTAMENTO %>%
  group_by(MAQUINA) %>%
  summarise(p_shapiro = shapiro.test(Taxa_PERDA_POR_EMENDA_DE_MATERIAL)$p.value)

print(shapiro_result)

#Não há normalidade entre a distribuição das Prensas

#Teste de diferença entre Prensas - Kruskal-Wallis
teste_resultado <- kruskal.test(Taxa_PERDA_POR_EMENDA_DE_MATERIAL ~ MAQUINA, data = bd_APONTAMENTO)
print(teste_resultado)

# Há diferença estatística entre pelo menos tipos de MAQUIANA
# MAQUIANA influencia significativamente a taxa de refugo por emenda.

#Teste de Dunn
dunnTest(Taxa_PERDA_POR_EMENDA_DE_MATERIAL ~ factor(MAQUINA), data = bd_APONTAMENTO, method = "bonferroni")

medianas <- bd_APONTAMENTO %>%
  group_by(MAQUINA) %>%
  summarise(
    Mediana = median(Taxa_PERDA_POR_EMENDA_DE_MATERIAL, na.rm = TRUE),
    IQR = IQR(Taxa_PERDA_POR_EMENDA_DE_MATERIAL),
    n = n()
  ) %>%
  arrange(desc(Mediana))

print(medianas)

# Prensa 1 - A
# Prensa 2 - B
# Prensa 3 - B

rm(shapiro_result, teste_resultado, medianas)
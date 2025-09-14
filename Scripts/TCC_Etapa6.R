# Clusterização das Séries Temporais

#Bibliotecas e Parametros Inciais ##############################################
library(dtwclust)
library(cluster)
library(ggplot2)
library(dplyr)

#Temperatura Plato Superior ####################################################

#Transformação dos Dados #######################################################

series_list_ps <- bd_suavizado_padronizado %>% 
  arrange(ID, tempo_padrao) %>% group_by(ID) %>%
  summarise(serie = list(Temp_PS_suavizado), .groups = "drop") %>%
  pull(serie)

#Escolha do Tamanho do Cluster #################################################

#Range de Ks
range_k <- 2:10
wss_values <- numeric(length(range_k))

#Para cada valor do Range
for (i in seq_along(range_k)) {
  
  #Avisar Rodada
  cat("Rodando modelo para k =", range_k[i], "\n")
  
  #Executar Modelo para k=i
  model <- tsclust(
    series_list_ps,
    type = "partitional",
    k = range_k[i],
    distance = "dtw_basic",
    centroid = "pam",
    seed = 28,
    trace = FALSE
  )
  
  #Armazenar Variaveis
  centroids <- model@centroids
  clusters <- model@cluster
  wss <- 0
  #Para cada serie temporal
  for (j in seq_along(series_list_ps)) {
    #Calcular distancia dtw_basic entre serie (j) e seu centroide
    dist <- proxy::dist(list(series_list_ps[[j]]), list(centroids[[clusters[j]]]), method = "dtw_basic")
    #Calcular WSS
    wss <- wss + dist^2
  }
  
  #Armazenar valor de WSS
  wss_values[i] <- wss
}

#Plotar Gráfico de WSS
plot(range_k, wss_values, type = "n",
     xlab = "Número de Clusters (k)", ylab = "WSS", 
     main = "", xaxs = "i", yaxs = "i")

#Adicionar linha vertical no k ideal
abline(v = 4, col = "black", lty = 2, lwd = 2)

#Adiciona linhas conectando os pontos
lines(range_k, wss_values, col = "#007FFF", lwd = 2)

#Adiciona os pontos
points(range_k, wss_values, col = "#007FFF", pch = 19)

#Adiciona título alinhado à esquerda
title(main = "Método do Cotovelo para Escolha de Número de Clusters - Temperatura do Platô Superior", adj = 0)

################################################################################
# Através do Método do Cotovelo conclui-se que o número ideal de Clusters é 4
################################################################################

#Atribuir melhor k
best_k_ps <- 4

#Calcular Cluster Definitivo ###################################################
set.seed(28)
model <- tsclust(
  series_list_ps,
  type = "partitional",
  k = best_k_ps,
  distance = "dtw_basic",
  centroid = "pam",
  seed = 28,
  trace = FALSE
)

#Salvar Informações ############################################################

#Lista de IDs
ids <- bd_suavizado_padronizado %>%
  arrange(ID, tempo_padrao) %>%
  group_by(ID) %>%
  summarise(.groups = "drop") %>%
  pull(ID)

#Unir IDs com Clusters
df_clusters <- data.frame(ID = ids, Cluster_PS = model@cluster)

#Adicionar Informações bd_suavizado_padronizado 
bd_suavizado_padronizado <- bd_suavizado_padronizado %>%
  left_join(df_clusters, by = "ID")

#Avaliação de Clusters #########################################################

#Exibir Séries Temporais no Gráfico de Linhas
ggplot(bd_suavizado_padronizado, aes(x = tempo_padrao, y = Temp_PS_suavizado, group = ID, color = as.factor(Cluster_PS))) +
  geom_line(alpha = 0.7) +
  scale_color_manual(
    values = c("1" = "#007FFF", "2" = "#009132", "3" = "#e5050f", "4" = "#8c0051"),
    name = "Cluster"
  ) +
  labs(
    title = "Temperatura do Platô Superior por Cluster - Suavizado",
    x = "Tempo (Posição na Série)",
    y = "Temperatura ºC"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

#Agrupar bd_suavizado_padronizado por Cluster 
TESTES <- bd_suavizado_padronizado %>%
  group_by(Cluster_PS) %>%
  summarise(
    
    #Calcular Informações informações
    n = n_distinct(ID),
    media = mean(Temp_PS_suavizado),
    dp = sd(Temp_PS_suavizado),
    
    #IC - Média
    erro_media = dp / sqrt(n()),
    t_critico = qt(0.975, df = n() - 1),
    ic_media_inf = media - t_critico * erro_media,
    ic_media_sup = media + t_critico * erro_media,
    
    # IC - DP (qui-quadrado)
    chi2_inf = qchisq(0.975, df = n() - 1),
    chi2_sup = qchisq(0.025, df = n() - 1),
    dp_inf = sqrt((n() - 1) * dp^2 / chi2_inf),
    dp_sup = sqrt((n() - 1) * dp^2 / chi2_sup)
    
  ) %>%
  mutate(
    media_ic = sprintf("%.2f ± %.2f", media, t_critico * erro_media),
    dp_ic = sprintf("%.2f [%.2f, %.2f]", dp, dp_inf, dp_sup)
  ) %>%
  select(Cluster_PS, n, media_ic, dp_ic)

# Exibir resultado
print(TESTES)

#Aplicação dos Clusters ########################################################

#Salvar Informações em bd_APONTAMENTO
bd_APONTAMENTO$ID = paste0(bd_APONTAMENTO$Turno_ID,"_",bd_APONTAMENTO$MAQUINA)

bd_APONTAMENTO <- bd_APONTAMENTO %>%
  left_join(df_clusters %>% select(ID, Cluster_PS), by = "ID")

bd_APONTAMENTO$Cluster_PS <- as.factor(bd_APONTAMENTO$Cluster_PS)

#Verificar se Cluster_PS esta influenciando no Refugo

cores_cluster <- c("1" = "#007FFF", "2" = "#009132", "3" = "#e5050f", "4" = "#8c0051")

#Box-Plot
ggplot(bd_APONTAMENTO, aes(x = as.factor(Cluster_PS), y = Taxa_PERDA_POR_EMENDA_DE_MATERIAL, fill = as.factor(Cluster_PS))) +
  geom_boxplot(color = "black", alpha = 0.7, outlier.color = "black") +
  scale_fill_manual(values = cores_cluster) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(
    title = "Taxa de Refugo por Emenda de Material por Cluster de Temperatura do Platô Superior",
    x = "Clusters Temperatura do Platô Superior das Prensas",
    y = "Taxa de Refugo",
    fill = "Cluster"
  ) +
  theme_minimal(base_size = 13)

#Gráfico de Violino
ggplot(bd_APONTAMENTO, aes(x = as.factor(Cluster_PS), y = Taxa_PERDA_POR_EMENDA_DE_MATERIAL, fill = as.factor(Cluster_PS))) +
  geom_violin(trim = FALSE, alpha = 0.4, color = NA) +
  geom_boxplot(width = 0.2, alpha = 0.8, color = "black", outlier.color = "black", outlier.size = 0.75) +
  scale_fill_manual(values = cores_cluster) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(
    title = "Taxa de Refugo por Emenda de Material por Cluster de Temperatura do Platô Superior",
    x = "Clusters Temperatura do Platô Inferior das Superior",
    y = "Taxa de Refugo",
    fill = "Cluster"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

#Teste de normalidade por Prensa - SHAPIRO
shapiro_result <- bd_APONTAMENTO %>%
  group_by(Cluster_PS) %>%
  summarise(p_shapiro = shapiro.test(Taxa_PERDA_POR_EMENDA_DE_MATERIAL)$p.value)

print(shapiro_result)

#Não há normalidade entre a distribuição das Prensas

#Teste de diferença entre Prensas - Kruskal-Wallis
teste_resultado <- kruskal.test(Taxa_PERDA_POR_EMENDA_DE_MATERIAL ~ Cluster_PS, data = bd_APONTAMENTO)
print(teste_resultado)

# Segundo o teste de Kruskal-Wallis não há evidencia estaística de que os Clusters
# estão afetando a distribuição de Refugo por Emenda de Material #################
##################################################################################

#Limpar Variaveis ##############################################################

rm(series_list_ps, scores, model, df_clusters, shapiro_result, teste_resultado, TESTES, best_k_ps, i, ids)
rm(centroids, clusters, dist, j, wss, wss_values, cores_cluster, range_k)
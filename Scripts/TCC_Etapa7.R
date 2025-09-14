# Clusterização Conjunta das Séries Temporais

bd_APONTAMENTO$Cluster_PI_PS = as.factor(paste0(bd_APONTAMENTO$Cluster_PI,"_",bd_APONTAMENTO$Cluster_PS))

#Visualização Gráfica ##########################################################

unique(bd_APONTAMENTO$Cluster_PI_PS)
cores_cluster <- c("1_1" = "#007FFF", "1_3" = "#4c56af", "1_4" = "#4640a8", "2_2" = "#009132",
                   "3_1" = "#992e5f", "3_3"="#e5050f","3_4"="#b90330")

#Box-Plot
ggplot(bd_APONTAMENTO, aes(x = as.factor(Cluster_PI_PS), y = Taxa_PERDA_POR_EMENDA_DE_MATERIAL, fill = as.factor(Cluster_PI_PS))) +
  geom_boxplot(color = "black", alpha = 0.7, outlier.color = "black") +
  scale_fill_manual(values = cores_cluster) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(
    title = "Taxa de Refugo por Emenda de Material por Cluster de Temperatura do Combinados",
    x = "Clusters Temperatura das Prensas Combinados",
    y = "Taxa de Refugo",
    fill = "Cluster"
  ) +
  theme_minimal(base_size = 13)

#Gráfico de Violino
ggplot(bd_APONTAMENTO, aes(x = as.factor(Cluster_PI_PS), y = Taxa_PERDA_POR_EMENDA_DE_MATERIAL, fill = as.factor(Cluster_PI_PS))) +
  geom_violin(trim = FALSE, alpha = 0.4, color = NA) +
  geom_boxplot(width = 0.2, alpha = 0.8, color = "black", outlier.color = "black", outlier.size = 0.75) +
  scale_fill_manual(values = cores_cluster) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(
    title = "Taxa de Refugo por Emenda de Material por Cluster de Temperatura do Combinados",
    x = "Clusters Temperatura das Prensas Combinados",
    y = "Taxa de Refugo",
    fill = "Cluster"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

#Teste de diferença entre Prensas - Kruskal-Wallis
teste_resultado <- kruskal.test(Taxa_PERDA_POR_EMENDA_DE_MATERIAL ~ Cluster_PI_PS, data = bd_APONTAMENTO)
print(teste_resultado)

# Segundo o teste de Kruskal-Wallis não há evidencia estaística de que os Clusters
# combinados estão afetando a distribuição de Refugo por Emenda de Material ######
##################################################################################

rm(teste_resultado, cores_cluster)
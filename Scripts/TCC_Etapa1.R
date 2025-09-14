#CONSTRUÇÃO DA BASE DADOS E FORMATACAO DOS DADOS

################################################################################
#Bibliotecas e Parametros Inciais ##############################################
################################################################################
setwd("C:/")

library(readxl)
library(dplyr)
library(tidyr)
library(DBI)
library(odbc)
library(lubridate)

################################################################################
#Ler base de dados - Apontamento ###############################################
################################################################################
bd1 <- read_excel("Dados/DiarioDeBordo 1-6.xlsx")
bd2 <- read_excel("Dados/DiarioDeBordo 6-12.xlsx")
bd <- bind_rows(bd1, bd2)
rm(bd1,bd2)

#Mantar apenas Produção e Refugo
bd <- bd[bd$Tipo %in% c("Produção", "Refugo"), ]

#Remover colunas desnecessarias
bd <- bd %>% dplyr::select(-`T. Ciclo (min)`,
                           -Operação,
                           -`Data de Início`,
                           -`Data de Término`,
                           -Observações,
                           -`Tempo (min)`)

#Separar em duas bases de dados (refugo e boa)

#Qtd Boa
bd_boa = bd %>% filter(!is.na(`Qtd. boas`))

#Agrupar dados
bd_boa <- bd_boa %>%
  dplyr::select(-`Qtd. refugo`, -Motivo, -Tipo) %>%
  group_by(across(-`Qtd. boas`)) %>%
  summarise(`Qtd. boas` = sum(`Qtd. boas`, na.rm = TRUE), .groups = "drop")

#Qtd Refugo
bd_refugo = bd %>% filter(!is.na(`Qtd. refugo`))

#Agrupar dados
bd_refugo <- bd_refugo %>%
  dplyr::select(-`Qtd. boas`,-Tipo) %>%
  group_by(across(-`Qtd. refugo`)) %>%
  summarise(`Qtd. refugo` = sum(`Qtd. refugo`, na.rm = TRUE), .groups = "drop")

#Pivotar tipo de refugo
bd_refugo <- bd_refugo %>%
  pivot_wider(
    names_from = Motivo,
    values_from = `Qtd. refugo`,
    values_fill = 0
  )

#Renomear colunas de tipo de Refugo
bd_refugo <- bd_refugo %>%
  rename(
    PERDA_POR_EMENDA_DE_MATERIAL = `3 - PERDA POR EMENDA DE MATERIAL`,
    PERDA_POR_PROBLEMA_DE_ALIMENTACAO_NA_MAQUINA = `1 - PERDA POR PROBLEMA DE ALIMENTAÇÃO NA MÁQUINA`,
    PERDA_POR_RUPTURA_DE_ADERENCIA = `2 - PERDA POR RUPTURA DE ADERÊNCIA`,
    PERDA_POR_DEFEITO_NA_MATERIA_PRIMA = `7 - PERDA POR DEFEITO NA MATÉRIA PRIMA`,
    MAL_CONFORMADO = `8 - Mal conformado`,
    QUEIMADA = `6 - QUEIMADA`,
    MANCHADA = `4 - MANCHADA`
  )

#Criar Refugo Total
bd_refugo$REFUGO_TOTAL = bd_refugo$PERDA_POR_EMENDA_DE_MATERIAL +
  bd_refugo$PERDA_POR_PROBLEMA_DE_ALIMENTACAO_NA_MAQUINA +
  bd_refugo$PERDA_POR_RUPTURA_DE_ADERENCIA +
  bd_refugo$PERDA_POR_DEFEITO_NA_MATERIA_PRIMA +
  bd_refugo$MAL_CONFORMADO +
  bd_refugo$QUEIMADA +
  bd_refugo$MANCHADA

#Unir bases de dados
bd_APONTAMENTO <- left_join(bd_refugo, bd_boa, by = c("Máquina", "Data", "Turno", "Item"))

#Deletar variaveis desnecessarias
rm(bd,bd_boa,bd_refugo)

#Renomear colunas
bd_APONTAMENTO <- bd_APONTAMENTO %>%
  rename(
    MAQUINA = `Máquina`,
    Data = `Data`,
    TURNO = `Turno`,
    ITEM = `Item`,
    BOA_TOTAL = `Qtd. boas`)

#Criar incidencia de refugo
bd_APONTAMENTO$Taxa_Refugo <- bd_APONTAMENTO$REFUGO_TOTAL / (bd_APONTAMENTO$REFUGO_TOTAL + bd_APONTAMENTO$BOA_TOTAL)

#Criar ID de Turno
bd_APONTAMENTO$Turno_ID <- paste0(bd_APONTAMENTO$Data, "_", bd_APONTAMENTO$TURNO)

#Criar taxas individuais
bd_APONTAMENTO$Taxa_PERDA_POR_EMENDA_DE_MATERIAL <- bd_APONTAMENTO$PERDA_POR_EMENDA_DE_MATERIAL / (bd_APONTAMENTO$REFUGO_TOTAL + bd_APONTAMENTO$BOA_TOTAL)
bd_APONTAMENTO$Taxa_PERDA_POR_RUPTURA_DE_ADERENCIA <- bd_APONTAMENTO$PERDA_POR_RUPTURA_DE_ADERENCIA / (bd_APONTAMENTO$REFUGO_TOTAL + bd_APONTAMENTO$BOA_TOTAL)
bd_APONTAMENTO$Taxa_MAL_CONFORMADO <- bd_APONTAMENTO$MAL_CONFORMADO / (bd_APONTAMENTO$REFUGO_TOTAL + bd_APONTAMENTO$BOA_TOTAL)
bd_APONTAMENTO$Taxa_PERDA_POR_PROBLEMA_DE_ALIMENTACAO_NA_MAQUINA <- bd_APONTAMENTO$PERDA_POR_PROBLEMA_DE_ALIMENTACAO_NA_MAQUINA / (bd_APONTAMENTO$REFUGO_TOTAL + bd_APONTAMENTO$BOA_TOTAL)
bd_APONTAMENTO$Taxa_PERDA_POR_DEFEITO_NA_MATERIA_PRIMA <- bd_APONTAMENTO$PERDA_POR_DEFEITO_NA_MATERIA_PRIMA / (bd_APONTAMENTO$REFUGO_TOTAL + bd_APONTAMENTO$BOA_TOTAL)
bd_APONTAMENTO$Taxa_QUEIMADA <- bd_APONTAMENTO$QUEIMADA / (bd_APONTAMENTO$REFUGO_TOTAL + bd_APONTAMENTO$BOA_TOTAL)
bd_APONTAMENTO$Taxa_MANCHADA <- bd_APONTAMENTO$MANCHADA / (bd_APONTAMENTO$REFUGO_TOTAL + bd_APONTAMENTO$BOA_TOTAL)

#Criar Quantidade Total de Peças
bd_APONTAMENTO$TOTAL_PRODUZIDO = bd_APONTAMENTO$REFUGO_TOTAL + bd_APONTAMENTO$BOA_TOTAL

################################################################################
#Delimitar Itens ###############################################################
################################################################################
bd_APONTAMENTO %>%
  count(ITEM, name = "n_observacoes") %>%
  arrange(desc(n_observacoes))

itens_validos <- c("T4", "T3", "T2", "T5", "BB-240", "T1")

#Manter itens com mais de 50 ocorrencias
#bd_APONTAMENTO <- bd_APONTAMENTO %>%
#  filter(ITEM %in% itens_validos)

################################################################################
#Ler base de dados - Prensas ###################################################
################################################################################

#Conexao com SQL Server Express local
con <- dbConnect(odbc::odbc(),
                 Driver   = "SQL Server",
                 Server   = "localhost\\SQLEXPRESS",
                 Database = "PRENSAS_RS",
                 Trusted_Connection = "Yes")

#Ler base de dados
#PRENSA 1
bd_P1_TEMP_PI <- dbGetQuery(con, "SELECT * FROM PRENSAS_RS.dbo.PRENSAS_RS_TempPI001")
bd_P1_TEMP_PS <- dbGetQuery(con, "SELECT * FROM PRENSAS_RS.dbo.PRENSAS_RS_TempPS001")
bd_P1_SETP_PI <- dbGetQuery(con, "SELECT * FROM PRENSAS_RS.dbo.PRENSAS_RS_SetPointTempP1")
bd_P1_SETP_PS <- dbGetQuery(con, "SELECT * FROM PRENSAS_RS.dbo.PRENSAS_RS_SetPointTempP1S")

#PRENSA 2
bd_P2_TEMP_PI <- dbGetQuery(con, "SELECT * FROM PRENSAS_RS.dbo.PRENSAS_RS_TempPI002")
bd_P2_TEMP_PS <- dbGetQuery(con, "SELECT * FROM PRENSAS_RS.dbo.PRENSAS_RS_TempPS002")
bd_P2_SETP_PI <- dbGetQuery(con, "SELECT * FROM PRENSAS_RS.dbo.PRENSAS_RS_SetPointTempP2")
bd_P2_SETP_PS <- dbGetQuery(con, "SELECT * FROM PRENSAS_RS.dbo.PRENSAS_RS_SetPointTempP2S")

#PRENSA 3
bd_P3_TEMP_PI <- dbGetQuery(con, "SELECT * FROM PRENSAS_RS.dbo.PRENSAS_RS_TempPI003")
bd_P3_TEMP_PS <- dbGetQuery(con, "SELECT * FROM PRENSAS_RS.dbo.PRENSAS_RS_TempPS003")
bd_P3_SETP_PI <- dbGetQuery(con, "SELECT * FROM PRENSAS_RS.dbo.PRENSAS_RS_SetPointTempP3")
bd_P3_SETP_PS <- dbGetQuery(con, "SELECT * FROM PRENSAS_RS.dbo.PRENSAS_RS_SetPointTempP3S")

#Desligar conexao
dbDisconnect(con)
rm(con)

################################################################################
#PRENSA 1 ######################################################################
################################################################################

#Filtrar temperaturas erradas e remover colunas desnecessarias
bd_P1_TEMP_PI <- bd_P1_TEMP_PI %>%
  dplyr::select(-ID, -TimeStamp, -TimeStamp2, -Quality) %>%
  filter(DataValue <= 30000)

bd_P1_TEMP_PS <- bd_P1_TEMP_PS %>%
  dplyr::select(-ID, -TimeStamp, -TimeStamp2, -Quality) %>%
  filter(DataValue <= 30000)

bd_P1_SETP_PI <- bd_P1_SETP_PI %>%
  dplyr::select(-ID, -TimeStamp, -TimeStamp2, -Quality) %>%
  filter(DataValue <= 30000)

bd_P1_SETP_PS <- bd_P1_SETP_PS %>%
  dplyr::select(-ID, -TimeStamp, -TimeStamp2, -Quality) %>%
  filter(DataValue <= 30000)

#Calcular temperatura da prensa e renomear ela
names(bd_P1_TEMP_PI)[names(bd_P1_TEMP_PI) == "DataValue"] <- "Temp_PI"
bd_P1_TEMP_PI$Temp_PI =  bd_P1_TEMP_PI$Temp_PI/10

names(bd_P1_TEMP_PS)[names(bd_P1_TEMP_PS) == "DataValue"] <- "Temp_PS"
bd_P1_TEMP_PS$Temp_PS =  bd_P1_TEMP_PS$Temp_PS/10

names(bd_P1_SETP_PI)[names(bd_P1_SETP_PI) == "DataValue"] <- "SetPoint_PI"
bd_P1_SETP_PI$SetPoint_PI =  bd_P1_SETP_PI$SetPoint_PI/10

names(bd_P1_SETP_PS)[names(bd_P1_SETP_PS) == "DataValue"] <- "SetPoint_PS"
bd_P1_SETP_PS$SetPoint_PS =  bd_P1_SETP_PS$SetPoint_PS/10

#Unir bases de dados
bd_PRENSA_1 <- bd_P1_TEMP_PI %>%
  inner_join(bd_P1_TEMP_PS, by = "DateTime") %>%
  inner_join(bd_P1_SETP_PI, by = "DateTime") %>%
  inner_join(bd_P1_SETP_PS, by = "DateTime")

#Separar Data e Hora
bd_PRENSA_1$DateTime <- ymd_hms(bd_PRENSA_1$DateTime)
bd_PRENSA_1$Data <- as.Date(bd_PRENSA_1$DateTime)

#Calcular Turnos
hora_decimal <- hour(bd_PRENSA_1$DateTime) + minute(bd_PRENSA_1$DateTime) / 60
bd_PRENSA_1$Turno <- ifelse(hora_decimal >= 5 & hora_decimal < 15, "01",
                            ifelse(hora_decimal >= 15 | hora_decimal < 0.27, "02","03"))

#Criar Identificador do Turno
bd_PRENSA_1$Turno_ID <- ifelse(
  bd_PRENSA_1$Turno == "02" & hora_decimal < 0.27,
  as.character(bd_PRENSA_1$Data - 1),
  as.character(bd_PRENSA_1$Data)
)

bd_PRENSA_1$Turno_ID <- paste0(bd_PRENSA_1$Turno_ID, "_", bd_PRENSA_1$Turno)

#Remover Auxiliares
rm(hora_decimal,
   bd_P1_SETP_PI,
   bd_P1_SETP_PS,
   bd_P1_TEMP_PI,
   bd_P1_TEMP_PS)

#Ordenar dataframe
bd_PRENSA_1 <- bd_PRENSA_1 %>%
  dplyr::select(Turno_ID, DateTime,
                Temp_PI, Temp_PS,
                SetPoint_PI, SetPoint_PS)

#PRENSA 2 ######################################################################
#Filtrar temperaturas erradas e remover colunas desnecessarias
bd_P2_TEMP_PI <- bd_P2_TEMP_PI %>%
  dplyr::select(-ID, -TimeStamp, -TimeStamp2, -Quality) %>%
  filter(DataValue <= 30000)

bd_P2_TEMP_PS <- bd_P2_TEMP_PS %>%
  dplyr::select(-ID, -TimeStamp, -TimeStamp2, -Quality) %>%
  filter(DataValue <= 30000)

bd_P2_SETP_PI <- bd_P2_SETP_PI %>%
  dplyr::select(-ID, -TimeStamp, -TimeStamp2, -Quality) %>%
  filter(DataValue <= 30000)

bd_P2_SETP_PS <- bd_P2_SETP_PS %>%
  dplyr::select(-ID, -TimeStamp, -TimeStamp2, -Quality) %>%
  filter(DataValue <= 30000)

#Calcular temperatura da prensa e renomear ela
names(bd_P2_TEMP_PI)[names(bd_P2_TEMP_PI) == "DataValue"] <- "Temp_PI"
bd_P2_TEMP_PI$Temp_PI = bd_P2_TEMP_PI$Temp_PI / 10

names(bd_P2_TEMP_PS)[names(bd_P2_TEMP_PS) == "DataValue"] <- "Temp_PS"
bd_P2_TEMP_PS$Temp_PS = bd_P2_TEMP_PS$Temp_PS / 10

names(bd_P2_SETP_PI)[names(bd_P2_SETP_PI) == "DataValue"] <- "SetPoint_PI"
bd_P2_SETP_PI$SetPoint_PI = bd_P2_SETP_PI$SetPoint_PI / 10

names(bd_P2_SETP_PS)[names(bd_P2_SETP_PS) == "DataValue"] <- "SetPoint_PS"
bd_P2_SETP_PS$SetPoint_PS = bd_P2_SETP_PS$SetPoint_PS / 10

#Unir bases de dados
bd_PRENSA_2 <- bd_P2_TEMP_PI %>%
  inner_join(bd_P2_TEMP_PS, by = "DateTime") %>%
  inner_join(bd_P2_SETP_PI, by = "DateTime") %>%
  inner_join(bd_P2_SETP_PS, by = "DateTime")

#Separar Data e Hora
bd_PRENSA_2$DateTime <- ymd_hms(bd_PRENSA_2$DateTime)
bd_PRENSA_2$Data <- as.Date(bd_PRENSA_2$DateTime)

#Calcular Turnos
hora_decimal <- hour(bd_PRENSA_2$DateTime) + minute(bd_PRENSA_2$DateTime) / 60
bd_PRENSA_2$Turno <- ifelse(hora_decimal >= 5 & hora_decimal < 15, "01",
                            ifelse(hora_decimal >= 15 | hora_decimal < 0.27, "02", "03"))

#Criar Identificador do Turno		
bd_PRENSA_2$Turno_ID <- ifelse(
  bd_PRENSA_2$Turno == "02" & hora_decimal < 0.27,
  as.character(bd_PRENSA_2$Data - 1),
  as.character(bd_PRENSA_2$Data)
)		

bd_PRENSA_2$Turno_ID <- paste0(bd_PRENSA_2$Turno_ID, "_", bd_PRENSA_2$Turno)

#Remover Auxiliares
rm(hora_decimal,
   bd_P2_SETP_PI,
   bd_P2_SETP_PS,
   bd_P2_TEMP_PI,
   bd_P2_TEMP_PS)

#Ordenar dataframe
bd_PRENSA_2 <- bd_PRENSA_2 %>%
  dplyr::select(Turno_ID, DateTime,
                Temp_PI, Temp_PS,
                SetPoint_PI, SetPoint_PS)

#PRENSA 3 ######################################################################
#Filtrar temperaturas erradas e remover colunas desnecessarias
bd_P3_TEMP_PI <- bd_P3_TEMP_PI %>%
  dplyr::select(-ID, -TimeStamp, -TimeStamp2, -Quality) %>%
  filter(DataValue <= 30000)

bd_P3_TEMP_PS <- bd_P3_TEMP_PS %>%
  dplyr::select(-ID, -TimeStamp, -TimeStamp2, -Quality) %>%
  filter(DataValue <= 30000)

bd_P3_SETP_PI <- bd_P3_SETP_PI %>%
  dplyr::select(-ID, -TimeStamp, -TimeStamp2, -Quality) %>%
  filter(DataValue <= 30000)

bd_P3_SETP_PS <- bd_P3_SETP_PS %>%
  dplyr::select(-ID, -TimeStamp, -TimeStamp2, -Quality) %>%
  filter(DataValue <= 30000)

#Calcular temperatura da prensa e renomear ela
names(bd_P3_TEMP_PI)[names(bd_P3_TEMP_PI) == "DataValue"] <- "Temp_PI"
bd_P3_TEMP_PI$Temp_PI = bd_P3_TEMP_PI$Temp_PI / 10

names(bd_P3_TEMP_PS)[names(bd_P3_TEMP_PS) == "DataValue"] <- "Temp_PS"
bd_P3_TEMP_PS$Temp_PS = bd_P3_TEMP_PS$Temp_PS / 10

names(bd_P3_SETP_PI)[names(bd_P3_SETP_PI) == "DataValue"] <- "SetPoint_PI"
bd_P3_SETP_PI$SetPoint_PI = bd_P3_SETP_PI$SetPoint_PI / 10

names(bd_P3_SETP_PS)[names(bd_P3_SETP_PS) == "DataValue"] <- "SetPoint_PS"
bd_P3_SETP_PS$SetPoint_PS = bd_P3_SETP_PS$SetPoint_PS / 10

#Unir bases de dados
bd_PRENSA_3 <- bd_P3_TEMP_PI %>%
  inner_join(bd_P3_TEMP_PS, by = "DateTime") %>%
  inner_join(bd_P3_SETP_PI, by = "DateTime") %>%
  inner_join(bd_P3_SETP_PS, by = "DateTime")

#Separar Data e Hora
bd_PRENSA_3$DateTime <- ymd_hms(bd_PRENSA_3$DateTime)
bd_PRENSA_3$Data <- as.Date(bd_PRENSA_3$DateTime)
bd_PRENSA_3$Hora <- format(bd_PRENSA_3$DateTime, "%H:%M")

#Calcular Turnos
hora_decimal <- hour(bd_PRENSA_3$DateTime) + minute(bd_PRENSA_3$DateTime) / 60
bd_PRENSA_3$Turno <- ifelse(hora_decimal >= 5 & hora_decimal < 15, "01",
                            ifelse(hora_decimal >= 15 | hora_decimal < 0.27, "02", "03"))

#Criar Identificador do Turno		
bd_PRENSA_3$Turno_ID <- ifelse(
  bd_PRENSA_3$Turno == "02" & hora_decimal < 0.27,
  as.character(bd_PRENSA_3$Data - 1),
  as.character(bd_PRENSA_3$Data)
)		

bd_PRENSA_3$Turno_ID <- paste0(bd_PRENSA_3$Turno_ID, "_", bd_PRENSA_3$Turno)

#Remover Auxiliares
rm(hora_decimal,
   bd_P3_SETP_PI,
   bd_P3_SETP_PS,
   bd_P3_TEMP_PI,
   bd_P3_TEMP_PS)

#Ordenar dataframe
bd_PRENSA_3 <- bd_PRENSA_3 %>%
  dplyr::select(Turno_ID, DateTime,
                Temp_PI, Temp_PS,
                SetPoint_PI, SetPoint_PS)

################################################################################
#Integracao e Ajustes das bases de dados #######################################
################################################################################

#Remover Turnos com pelo menos de 50 registros de Temperatura
bd_PRENSA_3 %>%
  count(Turno_ID, name = "n_observacoes") %>%
  arrange(desc(n_observacoes)) %>% tail(50)

#Prensa 1
ids_validos <- bd_PRENSA_1 %>%
  count(Turno_ID, name = "n_obs") %>%
  filter(n_obs >= 50) %>%
  pull(Turno_ID)

bd_PRENSA_1 <- bd_PRENSA_1 %>%
  filter(Turno_ID %in% ids_validos)

#Prensa 2
ids_validos <- bd_PRENSA_2 %>%
  count(Turno_ID, name = "n_obs") %>%
  filter(n_obs >= 50) %>%
  pull(Turno_ID)

bd_PRENSA_2 <- bd_PRENSA_2 %>%
  filter(Turno_ID %in% ids_validos)

#Prensa 3
ids_validos <- bd_PRENSA_3 %>%
  count(Turno_ID, name = "n_obs") %>%
  filter(n_obs >= 50) %>%
  pull(Turno_ID)

bd_PRENSA_3 <- bd_PRENSA_3 %>%
  filter(Turno_ID %in% ids_validos)

#Manter Turnos cuja a distancia entre registros não ultrapassa - 4 e 6 minutos

#Prensa 1
bd_PRENSA_1 <- bd_PRENSA_1 %>%
  arrange(Turno_ID, DateTime) %>%
  group_by(Turno_ID) %>%
  mutate(
    Tempo_Posterior = as.numeric(difftime(lead(DateTime), DateTime, units = "mins")),
    Tempo_Posterior = if_else(is.na(Tempo_Posterior), 5, Tempo_Posterior)
  ) %>%
  ungroup()

bd_PRENSA_1 <- bd_PRENSA_1 %>%
  group_by(Turno_ID) %>%
  filter(!any(Tempo_Posterior < 4 | Tempo_Posterior > 6)) %>%
  ungroup()

#Prensa 2
bd_PRENSA_2 <- bd_PRENSA_2 %>%
  arrange(Turno_ID, DateTime) %>%
  group_by(Turno_ID) %>%
  mutate(
    Tempo_Posterior = as.numeric(difftime(lead(DateTime), DateTime, units = "mins")),
    Tempo_Posterior = if_else(is.na(Tempo_Posterior), 5, Tempo_Posterior)
  ) %>%
  ungroup()

bd_PRENSA_2 <- bd_PRENSA_2 %>%
  group_by(Turno_ID) %>%
  filter(!any(Tempo_Posterior < 4 | Tempo_Posterior > 6)) %>%
  ungroup()

#Prensa 3
bd_PRENSA_3 <- bd_PRENSA_3 %>%
  arrange(Turno_ID, DateTime) %>%
  group_by(Turno_ID) %>%
  mutate(
    Tempo_Posterior = as.numeric(difftime(lead(DateTime), DateTime, units = "mins")),
    Tempo_Posterior = if_else(is.na(Tempo_Posterior), 5, Tempo_Posterior)
  ) %>%
  ungroup()

bd_PRENSA_3 <- bd_PRENSA_3 %>%
  group_by(Turno_ID) %>%
  filter(!any(Tempo_Posterior < 4 | Tempo_Posterior > 6)) %>%
  ungroup()

#Add Prensa ID na Base de Dados de Prensas
bd_PRENSA_1$MAQUINA <- "PRENSA 01"
bd_PRENSA_2$MAQUINA <- "PRENSA 02"
bd_PRENSA_3$MAQUINA <- "PRENSA 03"

#Listar IDs disponiveis em PRENSAS
ids_validos <- bind_rows(
  bd_PRENSA_1 %>% dplyr::select(Turno_ID, MAQUINA),
  bd_PRENSA_2 %>% dplyr::select(Turno_ID, MAQUINA),
  bd_PRENSA_3 %>% dplyr::select(Turno_ID, MAQUINA)
) %>% distinct()

#Remover Apontamentos sem dados das PRENSAS
bd_APONTAMENTO <- bd_APONTAMENTO %>%
  semi_join(ids_validos, by = c("Turno_ID", "MAQUINA"))

#Remover periodos sem apontamentos
bd_PRENSA_1 <- bd_PRENSA_1 %>%
  semi_join(bd_APONTAMENTO %>% filter(MAQUINA == "PRENSA 01"), by = "Turno_ID")

bd_PRENSA_2 <- bd_PRENSA_2 %>%
  semi_join(bd_APONTAMENTO %>% filter(MAQUINA == "PRENSA 02"), by = "Turno_ID")

bd_PRENSA_3 <- bd_PRENSA_3 %>%
  semi_join(bd_APONTAMENTO %>% filter(MAQUINA == "PRENSA 03"), by = "Turno_ID")

#Dimensão dos dados
max(bd_APONTAMENTO$Data)
min(bd_APONTAMENTO$Data)

rm(ids_validos)

#Resumir dados das prensas #####################################################

bd_PRENSA_1_RESUMO <- bd_PRENSA_1 %>%
  group_by(Turno_ID, MAQUINA) %>%
  summarise(
    
    M_Temp_PI   = mean(Temp_PI, na.rm = TRUE),
    SD_Temp_PI = sd(Temp_PI, na.rm = TRUE),
    MIN_Temp_PI    = min(Temp_PI, na.rm = TRUE),
    MAX_Temp_PI     = max(Temp_PI, na.rm = TRUE),
    
    M_Temp_PS   = mean(Temp_PS, na.rm = TRUE),
    SD_Temp_PS = sd(Temp_PS, na.rm = TRUE),
    MIN_Temp_PS    = min(Temp_PS, na.rm = TRUE),
    MAX_Temp_PS     = max(Temp_PS, na.rm = TRUE),
    .groups = "drop"
  )

bd_PRENSA_2_RESUMO <- bd_PRENSA_2 %>%
  group_by(Turno_ID, MAQUINA) %>%
  summarise(
    
    M_Temp_PI   = mean(Temp_PI, na.rm = TRUE),
    SD_Temp_PI = sd(Temp_PI, na.rm = TRUE),
    MIN_Temp_PI    = min(Temp_PI, na.rm = TRUE),
    MAX_Temp_PI     = max(Temp_PI, na.rm = TRUE),
    
    M_Temp_PS   = mean(Temp_PS, na.rm = TRUE),
    SD_Temp_PS = sd(Temp_PS, na.rm = TRUE),
    MIN_Temp_PS    = min(Temp_PS, na.rm = TRUE),
    MAX_Temp_PS     = max(Temp_PS, na.rm = TRUE),
    .groups = "drop"
  )

bd_PRENSA_3_RESUMO <- bd_PRENSA_3 %>%
  group_by(Turno_ID, MAQUINA) %>%
  summarise(
    
    M_Temp_PI   = mean(Temp_PI, na.rm = TRUE),
    SD_Temp_PI = sd(Temp_PI, na.rm = TRUE),
    MIN_Temp_PI    = min(Temp_PI, na.rm = TRUE),
    MAX_Temp_PI     = max(Temp_PI, na.rm = TRUE),
    
    M_Temp_PS   = mean(Temp_PS, na.rm = TRUE),
    SD_Temp_PS = sd(Temp_PS, na.rm = TRUE),
    MIN_Temp_PS    = min(Temp_PS, na.rm = TRUE),
    MAX_Temp_PS     = max(Temp_PS, na.rm = TRUE),
    .groups = "drop"
  )

#Unir dados resumidos com dados de apontamento
bd_PRENSAS_RESUMO <- bind_rows(bd_PRENSA_1_RESUMO, bd_PRENSA_2_RESUMO, bd_PRENSA_3_RESUMO)

bd_APONTAMENTO <- bd_APONTAMENTO %>%
  left_join(bd_PRENSAS_RESUMO, by = c("Turno_ID", "MAQUINA"))

bd_APONTAMENTO$BOA_TOTAL[is.na(bd_APONTAMENTO$BOA_TOTAL)] <- 0

rm(bd_PRENSA_1_RESUMO,bd_PRENSA_2_RESUMO,bd_PRENSA_3_RESUMO,bd_PRENSAS_RESUMO)

################################################################################
#Plots #########################################################################
################################################################################

#Plotar Taxa de Refugo #########################################################
hist(bd_APONTAMENTO$Taxa_Refugo,
     breaks = seq(0, ceiling(1 / 0.025) * 0.025, by = 0.025),
     main = "Distribuição da Taxa de Refugo",
     xlab = "Taxa de Refugo",
     ylab = "Frequência",
     col = "#007FFF",
     border = "black")

#Plots das Taxas Especificas de Refugo #########################################

# Layout 3 linhas × 3 colunas
par(mfrow = c(4, 2), mar = c(4, 4, 2, 1))

# Taxa_PERDA_POR_EMENDA_DE_MATERIAL ############################################

dados <- bd_APONTAMENTO[["Taxa_PERDA_POR_EMENDA_DE_MATERIAL"]]

#remover dados null
dados <- dados[!is.na(dados)]

# Define o limite superior com base no percentil 95
limite_sup <- max(dados, na.rm = TRUE)

# Cria sequência de breaks
max_break <- ceiling(limite_sup / 0.025) * 0.025
breaks_seq <- seq(0, max_break, by = 0.025)

# Histograma
hist(dados,
     breaks = breaks_seq,
     main = "Distribuição de Taxa de Refugo por Emenda de Material",
     xlab = "Taxa",
     ylab = "Frequência",
     col = "#007FFF",
     border = "black",
     xlim = c(0, max_break),
     cex.main = 2,   # título maior
     cex.lab = 1.2,    # rótulos dos eixos maiores
     cex.axis = 1.5)    # escala um pouco menor)

# Adiciona texto com total de observações e quantos são zero
total <- sum(!is.na(dados))
zeros <- sum(dados == 0, na.rm = TRUE)

legenda <- paste0("n = ", total, "\nzeros = ", zeros)
mtext(legenda, side = 3, adj = 1, line = -2, cex = 1, col = "gray20")

# Taxa_PERDA_POR_RUPTURA_DE_ADERENCIA ##########################################

dados <- bd_APONTAMENTO[["Taxa_PERDA_POR_RUPTURA_DE_ADERENCIA"]]

#remover dados null
dados <- dados[!is.na(dados)]

# Define o limite superior com base no percentil 95
limite_sup <- max(dados, na.rm = TRUE)

# Cria sequência de breaks
max_break <- ceiling(limite_sup / 0.005) * 0.005
breaks_seq <- seq(0, max_break, by = 0.005)

# Histograma
hist(dados,
     breaks = breaks_seq,
     main = "Distribuição de Taxa de Refugo por Ruptura de Aderência",
     xlab = "Taxa",
     ylab = "Frequência",
     col = "#007FFF",
     border = "black",
     xlim = c(0, max_break),
     cex.main = 2,   # título maior
     cex.lab = 1.2,    # rótulos dos eixos maiores
     cex.axis = 1.5)    # escala um pouco menor)

# Adiciona texto com total de observações e quantos são zero
total <- sum(!is.na(dados))
zeros <- sum(dados == 0, na.rm = TRUE)

legenda <- paste0("n = ", total, "\nzeros = ", zeros)
mtext(legenda, side = 3, adj = 1, line = -2, cex = 1, col = "gray20")

# Taxa_MAL_CONFORMADO ##########################################################

dados <- bd_APONTAMENTO[["Taxa_MAL_CONFORMADO"]]

#remover dados null
dados <- dados[!is.na(dados)]

# Define o limite superior com base no percentil 95
limite_sup <- max(dados, na.rm = TRUE)

# Cria sequência de breaks
max_break <- ceiling(limite_sup / 0.005) * 0.005
breaks_seq <- seq(0, max_break, by = 0.005)

# Histograma
hist(dados,
     breaks = breaks_seq,
     main = "Distribuição de Taxa de Refugo por Mal Conformado",
     xlab = "Taxa",
     ylab = "Frequência",
     col = "#007FFF",
     border = "black",
     xlim = c(0, max_break),
     cex.main = 2,   # título maior
     cex.lab = 1.2,    # rótulos dos eixos maiores
     cex.axis = 1.5)    # escala um pouco menor)

# Adiciona texto com total de observações e quantos são zero
total <- sum(!is.na(dados))
zeros <- sum(dados == 0, na.rm = TRUE)

legenda <- paste0("n = ", total, "\nzeros = ", zeros)
mtext(legenda, side = 3, adj = 1, line = -2, cex = 1, col = "gray20")

# Taxa_PERDA_POR_PROBLEMA_DE_ALIMENTACAO_NA_MAQUINA ############################

dados <- bd_APONTAMENTO[["Taxa_PERDA_POR_PROBLEMA_DE_ALIMENTACAO_NA_MAQUINA"]]

#remover dados null
dados <- dados[!is.na(dados)]

# Define o limite superior com base no percentil 95
limite_sup <- max(dados, na.rm = TRUE)

# Cria sequência de breaks
max_break <- ceiling(limite_sup / 0.01) * 0.01
breaks_seq <- seq(0, max_break, by = 0.01)

# Histograma
hist(dados,
     breaks = breaks_seq,
     main = "Distribuição de Taxa de Refugo por Problema de Alimentação da Máquina",
     xlab = "Taxa",
     ylab = "Frequência",
     col = "#007FFF",
     border = "black",
     xlim = c(0, max_break),
     cex.main = 2,   # título maior
     cex.lab = 1.2,    # rótulos dos eixos maiores
     cex.axis = 1.5)    # escala um pouco menor)

# Adiciona texto com total de observações e quantos são zero
total <- sum(!is.na(dados))
zeros <- sum(dados == 0, na.rm = TRUE)

legenda <- paste0("n = ", total, "\nzeros = ", zeros)
mtext(legenda, side = 3, adj = 1, line = -2, cex = 1, col = "gray20")

# Taxa_PERDA_POR_DEFEITO_NA_MATERIA_PRIMA ######################################

dados <- bd_APONTAMENTO[["Taxa_PERDA_POR_DEFEITO_NA_MATERIA_PRIMA"]]

#remover dados null
dados <- dados[!is.na(dados)]

# Define o limite superior com base no percentil 95
limite_sup <- max(dados, na.rm = TRUE)

# Cria sequência de breaks
max_break <- ceiling(limite_sup / 0.01) * 0.01
breaks_seq <- seq(0, max_break, by = 0.01)

# Histograma
hist(dados,
     breaks = breaks_seq,
     main = "Distribuição de Taxa de Refugo por Defeito na Matéria-Prima",
     xlab = "Taxa",
     ylab = "Frequência",
     col = "#007FFF",
     border = "black",
     xlim = c(0, max_break),
     cex.main = 2,   # título maior
     cex.lab = 1.2,    # rótulos dos eixos maiores
     cex.axis = 1.5)    # escala um pouco menor)

# Adiciona texto com total de observações e quantos são zero
total <- sum(!is.na(dados))
zeros <- sum(dados == 0, na.rm = TRUE)

legenda <- paste0("n = ", total, "\nzeros = ", zeros)
mtext(legenda, side = 3, adj = 1, line = -2, cex = 1, col = "gray20")

# Taxa_QUEIMADA ################################################################

dados <- bd_APONTAMENTO[["Taxa_QUEIMADA"]]

#remover dados null
dados <- dados[!is.na(dados)]

# Define o limite superior com base no percentil 95
limite_sup <- max(dados, na.rm = TRUE)

# Cria sequência de breaks
max_break <- ceiling(limite_sup / 0.0005) * 0.0005
breaks_seq <- seq(0, max_break, by = 0.0005)

# Histograma
hist(dados,
     breaks = breaks_seq,
     main = "Distribuição de Taxa de Refugo por Peça Queimada",
     xlab = "Taxa",
     ylab = "Frequência",
     col = "#007FFF",
     border = "black",
     xlim = c(0, max_break),
     cex.main = 2,   # título maior
     cex.lab = 1.2,    # rótulos dos eixos maiores
     cex.axis = 1.5)    # escala um pouco menor)

# Adiciona texto com total de observações e quantos são zero
total <- sum(!is.na(dados))
zeros <- sum(dados == 0, na.rm = TRUE)

legenda <- paste0("n = ", total, "\nzeros = ", zeros)
mtext(legenda, side = 3, adj = 1, line = -2, cex = 1, col = "gray20")

# Taxa_MANCHADA ################################################################

dados <- bd_APONTAMENTO[["Taxa_MANCHADA"]]

#remover dados null
dados <- dados[!is.na(dados)]

# Define o limite superior com base no percentil 95
limite_sup <- max(dados, na.rm = TRUE)

# Cria sequência de breaks
max_break <- ceiling(limite_sup / 0.0005) * 0.0005
breaks_seq <- seq(0, max_break, by = 0.0005)

# Histograma
hist(dados,
     breaks = breaks_seq,
     main = "Distribuição de Taxa de Refugo por Peça Manchada",
     xlab = "Taxa",
     ylab = "Frequência",
     col = "#007FFF",
     border = "black",
     xlim = c(0, max_break),
     cex.main = 2,   # título maior
     cex.lab = 1.2,    # rótulos dos eixos maiores
     cex.axis = 1.5)    # escala um pouco menor)

# Adiciona texto com total de observações e quantos são zero
total <- sum(!is.na(dados))
zeros <- sum(dados == 0, na.rm = TRUE)

legenda <- paste0("n = ", total, "\nzeros = ", zeros)
mtext(legenda, side = 3, adj = 1, line = -2, cex = 1, col = "gray20")

################################################################################

#Histograma Perda Por Emenda de Material
par(mfrow = c(1, 1))
dados <- bd_APONTAMENTO[["Taxa_PERDA_POR_EMENDA_DE_MATERIAL"]]

# Define o limite superior com base no percentil 95
limite_sup <- max(dados, na.rm = TRUE)

# Cria sequência de breaks
max_break <- ceiling(limite_sup / 0.05) * 0.05
breaks_seq <- seq(0, max_break, by = 0.05)

# Histograma
hist(dados,
     breaks = breaks_seq,
     main = "Distribuição de Taxa Perda Por Emenda de Material",
     xlab = "Taxa",
     ylab = "Frequência",
     col = "#007FFF",
     border = "black",
     xlim = c(0, max_break))

################################################################################
#Remover Outliers ##############################################################
################################################################################

#Calcular os limites do IQR
Q1 <- quantile(bd_APONTAMENTO$Taxa_PERDA_POR_EMENDA_DE_MATERIAL, 0.05, na.rm = TRUE) #0.25
Q3 <- quantile(bd_APONTAMENTO$Taxa_PERDA_POR_EMENDA_DE_MATERIAL, 0.95, na.rm = TRUE) #0.75
IQR <- Q3 - Q1

# Definir os limites inferior e superior
limite_inferior <- Q1 - 1.5 * IQR
limite_superior <- Q3 + 1.5 * IQR

# Filtrar o dataframe removendo os outliers
bd_APONTAMENTO <- bd_APONTAMENTO %>%
  filter(Taxa_PERDA_POR_EMENDA_DE_MATERIAL >= limite_inferior &
           Taxa_PERDA_POR_EMENDA_DE_MATERIAL <= limite_superior)

#Histograma Perda Por Emenda de Material
par(mfrow = c(1, 1))
dados <- bd_APONTAMENTO[["Taxa_PERDA_POR_EMENDA_DE_MATERIAL"]]

# Define o limite superior com base no percentil 95
limite_sup <- max(dados, na.rm = TRUE)

# Cria sequência de breaks
max_break <- ceiling(limite_sup / 0.005) * 0.005
breaks_seq <- seq(0, max_break, by = 0.005)

# Histograma
hist(dados,
     breaks = breaks_seq,
     main = "Distribuição de Taxa Perda Por Emenda de Material - Outliers Removidos",
     xlab = "Taxa",
     ylab = "Frequência",
     col = "#007FFF",
     border = "black",
     xlim = c(0, max_break))

rm(breaks_seq,Q1,Q3,max_break,itens_validos,limite_inferior,limite_superior,dados,IQR,limite_sup,legenda, total, zeros)

################################################################################
#Remover periodos sem apontamentos - Outiliers Removidos #######################
################################################################################

bd_PRENSA_1 <- bd_PRENSA_1 %>%
  semi_join(bd_APONTAMENTO %>% filter(MAQUINA == "PRENSA 01"), by = "Turno_ID")

bd_PRENSA_2 <- bd_PRENSA_2 %>%
  semi_join(bd_APONTAMENTO %>% filter(MAQUINA == "PRENSA 02"), by = "Turno_ID")

bd_PRENSA_3 <- bd_PRENSA_3 %>%
  semi_join(bd_APONTAMENTO %>% filter(MAQUINA == "PRENSA 03"), by = "Turno_ID")
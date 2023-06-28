#### Scrip gera dados de variação de salário e de vículos no período 2012 - 2021 por CBO Tec

# Carregando pacotes ----
library(data.table)
library(dplyr)
library("imputeTS")

#rstudioapi::writeRStudioPreference("data_viewer_max_columns", 300L)

# Carregando dados ----
# rais
#rais = data.table::fread('dados/rais-panorama-2021.csv',encoding = "Latin-1")
rais <- data.table::fread("P:/RECENTES/DIEPS/GEFAPS/GEFAPS/2023/MMT/MMT - Legado/dados/rais-panorama-2021.csv", encoding = "Latin-1")
#rais <- readRDS("../1. Extração dos dados/RAIS/Dados/RAIS.RDS") |>
#  mutate(tipo_emprego = case_when(tipovinculo == 1 ~ "Celetista",
#                                  TRUE ~ NA))

# Limpeza ----
## Limpando CBO ----
rais$cboocupacao2002 <- as.integer(stringr::str_remove(rais$cboocupacao2002, '-')) #remover o traço
rais <- rais %>% filter(referencia != 2011, !is.na(cboocupacao2002))

# Função que cria bases de variação ----
cria_base_filtro <- function(base,filtro,nome) {
dados_2020 <- base %>%
  #filtrar para 2020
  filter(tipo_emprego %in% filtro,
         referencia == 2020) %>%
  #agrupando por CBO
  group_by(cboocupacao2002) %>%
  summarise(vinculos_2020 = n(), #Vínculos no ano
            media_rendimento_2020 = mean(salario_dez_defl, na.rm = TRUE)) #Salário médio

#### Dados 2021 Geral ----
dados_2021 <- base %>%
  #filtrar para 2021
  filter(tipo_emprego %in% filtro,
         referencia == 2021) %>%
  #agrupando por CBO
  group_by(cboocupacao2002) %>%
  summarise(vinculos_2021 = n(), #Vínculos no ano
            media_rendimento_2021 = mean(salario_dez_defl, na.rm = TRUE),
            media_salario_hora_2021 = mean(salario_hora, na.rm = TRUE)) #Salário médio

#### Dados 2012 Geral ----
dados_2012 <- base %>%
  #filtrar para 2012
  filter(tipo_emprego %in% filtro,
         referencia == 2012) %>%
  #agrupando por CBO
  group_by(cboocupacao2002) %>%
  summarise(vinculos_2012 = n(),#Vínculos no ano
            media_rendimento_2012 = mean(salario_dez_defl, na.rm = TRUE),
            media_salario_hora_2012 = mean(salario_hora, na.rm = TRUE)) #Salário médio

#### Dados 2012-2021 (Geral) ----  
salario_vinculo_cbo <- base %>%
  filter(tipo_emprego %in% filtro) %>% 
  #agrupando por CBO e por ano
  group_by(cboocupacao2002, referencia) %>%
  summarise(vinculos = n(),
            media_rendimento = mean(salario_dez_defl, na.rm = TRUE),
            media_salario_hora = mean(salario_hora, na.rm = TRUE)) 

### Medianas (geral)----
#### Mediana 2012-2021 ----
dados_periodo2012_2021 <- salario_vinculo_cbo %>%
  #agrupando por CBO
  group_by(cboocupacao2002) %>%
  summarise(mediana_vinculos2012_2021 = median(vinculos), #mediana de vínculos 2012 - 2021 
            mediana_rendimento2012_2021 = median(media_rendimento)) #mediana de rendimentos 2012 - 2021

### Unindo dados (geral) ----
dados <- left_join(dados_2012,left_join(dados_2020, dados_2021), by = "cboocupacao2002") %>%
  left_join(dados_periodo2012_2021, by = "cboocupacao2002")

### Calculando variação dados  ----
dados <- dados %>%
  mutate(tipo_emprego = nome, # Técnicas e Não Técnicas
         #Variação 2012 - 2021 dos sqlários
         variacao_rendimento2012_2020 = (media_rendimento_2020/
                                           mediana_rendimento2012_2021) -1,
         #Variação 2012 - 2021 dos vínculos
         variacao_vinculos2012_2020 = (vinculos_2020/
                                         mediana_vinculos2012_2021) - 1,
         #Variação 2012 - 2021 dos salários
         variacao_rendimento2012_2021 = (media_rendimento_2021/
                                           mediana_rendimento2012_2021) -1,
         #Variação 2012 - 2020 dos vínculos
         variacao_vinculos2012_2021 = (vinculos_2021/
                                         mediana_vinculos2012_2021) - 1)
return(dados)
}

# Base Geral ----
#base_variacao <-  rbind(cria_base_filtro(base = rais,
#                                         filtro = c("Estatutário","Celetista","Outros"),
#                                         nome = "Geral"),
#                        cria_base_filtro(base = rais,
#                                        filtro = "Estatutário", 
#                                        nome = "Estatutário"),
#                        cria_base_filtro(base = rais,
#                                        filtro = "Celetista", 
#                                        nome = "Celetista"))

base_variacao <- cria_base_filtro(base = rais,
                                  filtro = "Celetista",
                                  nome = "Celetista")

# Adicionando os nomes de CBOs ----
# nomes cbos
#estrutura_cbo <- readRDS('produto/rds/estrutura-cbo.rds') %>% 
#  select(cboocupacao2002, nome_cbo_ocupacao, 
#         cbo_subgrupo_principal, nome_cbo_subgrupo_principal)

estrutura_cbo <- readRDS("../1. Extração dos dados/RDS/Dicionário CBO.RDS") |>
  select(cboocupacao2002, nome_cbo_ocupacao, 
         cbo_subgrupo_principal, nome_cbo_subgrupo_principal)

base_variacao_final <- left_join(base_variacao,estrutura_cbo, by = "cboocupacao2002") %>%
  #na_replace("null") %>%
  mutate(cboocupacao2002 = paste0('"',cboocupacao2002,'"'),
         cbo_subgrupo_principal = paste0('"',cbo_subgrupo_principal,'"')) %>% 
  select(cboocupacao2002, nome_cbo_ocupacao,
         mediana_viculos = mediana_vinculos2012_2021,
         mediana_rendimentos = mediana_rendimento2012_2021,
         vinculos_ultimo_ano = vinculos_2021,
         media_rendimento_ultimo_ano = media_rendimento_2021,
         media_salario_hora_ultimo_ano = media_salario_hora_2021,
         vinculos_primeiro_ano = vinculos_2012,
         media_rendimento_primeiro_ano = media_rendimento_2012,
         media_salario_hora_primeiro_ano = media_salario_hora_2012,
         variacao_rendimentos = variacao_rendimento2012_2021,
         variacao_vinculos = variacao_vinculos2012_2021,
         codigo_cbo = cbo_subgrupo_principal,
         cbo = nome_cbo_subgrupo_principal,
         tipo_emprego) |>
  filter(mediana_vinculos_historico > 12)

#readr::write_excel_csv2(base_variacao_final, 'produto/csv/base-ranking-cbo.csv')
# readr::write_excel_csv2(base_variacao_CLT, 'produto/csv/base-ranking-cbo-clt.csv')

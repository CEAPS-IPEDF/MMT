#### Script que gera Base de Propoção de Vínculos e Salário Médio por CBO

# Limpa Diretório
rm(list=ls(all=T))
# Dados sem notação científica
options(scipen=100)

# Carregando pacotes ----
library(data.table)
library(tidyverse)

rstudioapi::writeRStudioPreference("data_viewer_max_columns", 300L)

# Carregando dados ----
#rais
rais = data.table::fread('dados/rais-panorama-2021.csv',encoding = "Latin-1")

# Selecionar variáveis ----
base <- rais %>%
  filter(referencia != 2011,
         !is.na(cboocupacao2002)) %>% 
  select(ano = referencia,
         tec = cbo_tec,
         tipo_emprego) %>% #escolaridade
  mutate(tec = ifelse(tec == 1, "Técnico", "Não Técnico"))

# Criando base ----
base <- base %>% 
  group_by(ano, tec, tipo_emprego) %>% 
  summarise(vinculos = n())

# Salvando base ----
readr::write_excel_csv2(base, "produto/csv/base-viculos-tec-tipo-emprego-ano")

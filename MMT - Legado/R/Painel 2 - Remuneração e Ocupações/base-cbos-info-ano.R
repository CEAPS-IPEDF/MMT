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
rais = data.table::fread('dados/rais-panorama-2021.csv',encoding = "UTF-8")

# Selecionar variáveis ----
base <- rais %>%
  filter(referencia != 2011,
         !is.na(cboocupacao2002)) %>%
  select(ano = referencia,
         cboocupacao2002,
         tec = cbo_tec,
         salario_hora,
         salario = salario_dez_defl,
         idade) %>% #escolaridade
  mutate(tec = ifelse(tec == 1, "Técnico", "Não Técnico"))

# Criando base ----
base <- base %>% 
  group_by(ano, cboocupacao2002,tec) %>% 
  summarise(vinculos = n(),
            salario_hora_medio = mean(salario_hora),
            salario_medio = mean(salario),
            idade_media = mean(idade))

# Adicionando os nomes de CBOs ----
# nomes cbos
estrutura_cbo <- readRDS('produto/rds/estrutura-cbo.rds')
# bind
base <- left_join(base, estrutura_cbo, by = "cboocupacao2002")

# Limpando bases auxiliares ----
rm(list = ls()[-match("base", ls())])

readr::write_excel_csv2(base, 'produto/csv/base-cbos-info-ano.csv')

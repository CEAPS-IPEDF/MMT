#### Scrip gera dados de vículos no período 2012 - 2021 Separando por CBO Tec e tipo de emprego

# Carregando pacotes ----
library(data.table)
library(dplyr)

rstudioapi::writeRStudioPreference("data_viewer_max_columns", 300L)

# Carregando dados ----
#rais
rais = data.table::fread('dados/rais-panorama-2021.csv',encoding = "Latin-1")

# Selecionar variáveis ----
base <- rais %>%
  select(ano = referencia,
         tipo_emprego, #salário hora e tipo de ocupação
         tec = cbo_tec) #escolaridade

## Base ----  
base_final <- base %>%
  # variável de técnico
  mutate(tec = ifelse(tec == 1, "Técnico", "Não Técnico")) %>% 
  group_by(ano,tipo_emprego,tec) %>% 
  summarise(vinculos = n()) %>% 
  rbind(base %>%
          # variável de técnico
          mutate(tec = ifelse(tec == 1, "Técnico", "Não Técnico")) %>% 
          group_by(ano,,tec) %>% 
          summarise(vinculos = n(),
                    tipo_emprego = "Geral")) %>% 
  filter(ano != 2011)

readr::write_excel_csv2(base_final, 'produto/csv/base-vinculos-tipo-emprego-ano.csv')

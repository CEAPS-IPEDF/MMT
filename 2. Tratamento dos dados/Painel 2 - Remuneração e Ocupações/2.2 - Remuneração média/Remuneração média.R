#'*Script que calcula a remuneração média por hora trabalhada*
# Carregar pacotes ----

library(tidyverse) # Manipulação dos dados
library(imputeTS) # SUbstituir NA's

`%notin%` <- Negate(`%in%`)           # Função de filtro
options(readr.show_col_types = FALSE) # Omitir formato das colunas no console

# Importação dos dados ----

## RAIS ----

rais <- readRDS("../1. Extração dos dados/RAIS/Dados/RAIS.RDS") |>
  filter(tipovinculo == 1,       # Celetista
         referencia != 2011,
         !is.na(cboocupacao2002)) |>
  select(ano = referencia,
         horas = horas_mensais,
         salario_hora,
         salario_dez_defl,
         tec = cbo_tec, 
         tec_em = cbo_tec_em_complet,
         tec_sup = cbo_tec_sup,
         escolaridade)

## Base por tipo de emprego ----

### Empregos técnicos ----
base_tec <- base %>%
  filter(tec == 1,
         salario_hora > 0,
         horas > 0) %>% 
  mutate(tipo = case_when(tec_em == 1 ~"Técnicos de nível médio",
                          tec_sup == 1~"Técnicos de nível superior")) %>% 
  group_by(tipo_emprego,ano, tipo) %>% #Agrupando por ano
  summarise(filtro = "Técnico",
            salario_hora = mean(salario_hora, na.rm = T), salario_mes = mean(salario_dez_defl, na.rm = T)) %>%
  rbind(base %>% 
          filter(tec == 1,
                 salario_hora > 0,
                 horas > 0) %>% 
          group_by(tipo_emprego,ano) %>% 
          summarise(tipo = "Média dos trabalhadores técnicos",
                    filtro = "Técnico",
                    salario_hora = mean(salario_hora, na.rm = T), salario_mes = mean(salario_dez_defl, na.rm = T),))
### Base Não Tec ----
base_nao_tec <- base %>%
  filter(tec == 0,
         salario_hora > 0,
         horas > 0) %>%
  mutate(tipo = case_when(analf == 1~"Até fundamental completo",
                          em_inc == 1 ~"Até fundamental completo",
                          em_comp == 1 ~"Médio completo",
                          sup_comp == 1 ~"Superior completo")) %>% 
  group_by(tipo_emprego,ano, tipo) %>% 
  summarise(filtro = "Não Técnico",
            salario_hora = mean(salario_hora, na.rm = T), salario_mes = mean(salario_dez_defl, na.rm = T)) %>% 
  rbind(base %>% 
          filter(tec == 0,
                 salario_hora > 0,
                 horas > 0) %>% 
          group_by(tipo_emprego,ano) %>% 
          summarise(tipo =  "Média dos trabalhadores não técnicos",
                    filtro = "Não Técnico",
                    salario_hora = mean(salario_hora, na.rm = T), salario_mes = mean(salario_dez_defl, na.rm = T)))
### Bind (Tipo Emprego) ----
base_tipo_emprego <- rbind(base_tec,base_nao_tec) %>%
  mutate(geral = case_when(tipo %in% c("Média dos trabalhadores técnicos", 
                                       "Média dos trabalhadores não técnicos")~"Geral",
                           TRUE~"Subgrupo"))
## Base Geral ----
### Base Tec ----
base_tec <- base %>%
  filter(tec == 1,
         salario_hora > 0,
         horas > 0) %>% 
  mutate(tipo = case_when(tec_em == 1 ~"Técnicos de nível médio",
                          tec_sup == 1~"Técnicos de nível superior")) %>% 
  group_by(ano, tipo) %>% #Agrupando por ano
  summarise(filtro = "Técnico",
            salario_hora = mean(salario_hora, na.rm = T), salario_mes = mean(salario_dez_defl, na.rm = T)) %>%
  rbind(base %>% 
          filter(tec == 1,
                 salario_hora > 0,
                 horas > 0) %>% 
          group_by(ano) %>% 
          summarise(tipo = "Média dos trabalhadores técnicos",
                    filtro = "Técnico",
                    salario_hora = mean(salario_hora, na.rm = T), salario_mes = mean(salario_dez_defl, na.rm = T)))
### Base Não Tec ----
base_nao_tec <- base %>%
  filter(tec == 0,
         salario_hora > 0,
         horas > 0) %>% 
  mutate(tipo = case_when(analf == 1~"Até fundamental completo",
                          em_inc == 1 ~"Até fundamental completo",
                          em_comp == 1 ~"Médio completo",
                          sup_comp == 1 ~"Superior completo"
  )) %>% 
  group_by(ano, tipo) %>% 
  summarise(filtro = "Não Técnico",
            salario_hora = mean(salario_hora, na.rm = T), salario_mes = mean(salario_dez_defl, na.rm = T)) %>% 
  rbind(base %>% 
          filter(tec == 0,
                 salario_hora > 0,
                 horas > 0) %>% 
          group_by(ano) %>% 
          summarise(tipo = "Média dos trabalhadores não técnicos",
                    filtro = "Não Técnico",
                    salario_hora = mean(salario_hora, na.rm = T), salario_mes = mean(salario_dez_defl, na.rm = T)))
### Bind (Tipo Emprego agregado) ----
base_tipo_emprego_geral <- rbind(base_tec,base_nao_tec) %>%
  mutate(geral = case_when(tipo %in% c("Média dos trabalhadores técnicos", 
                                       "Média dos trabalhadores não técnicos")~"Geral",
                           TRUE~"Subgrupo"),
         tipo_emprego = "Geral")

## Bind por Tipo Emprego ----
base_final <- rbind(base_tipo_emprego, base_tipo_emprego_geral)

# Limpando bases auxiliares ----
rm(list = ls()[-match("base_final", ls())])

# Salvar Base ----
write_excel_csv2(base_final,'produto/csv/base-remun-media-escolaridade.csv')


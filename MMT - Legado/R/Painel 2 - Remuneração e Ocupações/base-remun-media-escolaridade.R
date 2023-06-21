#### Scrip gerabase para o MMT de remuneração média

# Limpa Diretório
rm(list=ls(all=T))
# Dados sem notação científica
options(scipen=100)

# Carrega pacotes ----
library(data.table)
library(tidyverse)
library(readxl)

# Ler os dados ----
## RAIS ----
rais = data.table::fread('dados/rais-panorama-2021.csv',encoding = "Latin-1")

# Selecionar variáveis ----
base <- rais %>%
  filter(referencia != 2011,
         !is.na(cboocupacao2002)) %>%
  select(ano = referencia, horas = horas_mensais,
         salario_hora, salario_dez_defl, tipo_emprego, #salário hora e tipo de ocupação
         tec = cbo_tec, tec_em = cbo_tec_em_complet, tec_sup = cbo_tec_sup, #CBOS tec
         em_inc = medio_incompleto, em_comp = medio_completo, sup_comp = superior_completo, analf = analfabeto) #escolaridade

## Base por tipo de Emprego----
### Base Tec ----
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


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

base_tec <- rais |>
  filter(tec == 1,
         salario_hora > 0,
         horas > 0) |> 
  mutate(tipo = case_when(tec_em == 1  ~ "Técnicos de nível médio",
                          tec_sup == 1 ~ "Técnicos de nível superior")) |>
  group_by(ano, tipo) |> 
  summarise(salario_hora = mean(salario_hora, na.rm = T), 
            salario_mes = mean(salario_dez_defl, na.rm = T),
            filtro = "Técnico") |>
  rbind(rais |> 
          filter(tec == 1,
                 salario_hora > 0,
                 horas > 0) |> 
          group_by(ano) |> 
          summarise(salario_hora = mean(salario_hora, na.rm = T), 
                    salario_mes = mean(salario_dez_defl, na.rm = T),
                    tipo = "Média dos trabalhadores técnicos",
                    filtro = "Técnico"))

### Empregos não técnicos ----

base_nao_tec <- rais |>
  filter(tec == 0,
         salario_hora > 0,
         horas > 0) |>
  mutate(tipo = case_when(escolaridade == 1 ~"Até fundamental completo",
                          escolaridade == 2 ~"Até fundamental completo",
                          escolaridade == 3 ~"Médio completo",
                          escolaridade == 4 ~"Superior completo")) |> 
  group_by(ano, tipo) |> 
  summarise(filtro = "Não técnico",
            salario_hora = mean(salario_hora, na.rm = T), 
            salario_mes = mean(salario_dez_defl, na.rm = T)) |> 
  rbind(rais |> 
          filter(tec == 0,
                 salario_hora > 0,
                 horas > 0) |> 
          group_by(ano) |> 
          summarise(tipo =  "Média dos trabalhadores não técnicos",
                    filtro = "Não técnico",
                    salario_hora = mean(salario_hora, na.rm = T), salario_mes = mean(salario_dez_defl, na.rm = T)))

### Merge das bases ----

base_tipo_emprego <- rbind(base_tec, base_nao_tec) |>
  mutate(geral = case_when(tipo %in% c("Média dos trabalhadores técnicos", "Média dos trabalhadores não técnicos") ~ "Geral",
                           TRUE ~ "Subgrupo"))

# Exportar CSV ----

write_excel_csv2(base_tipo_emprego, "Painel 2 - Remuneração e Ocupações/Resultados/2.2 - Remuneração média.csv")

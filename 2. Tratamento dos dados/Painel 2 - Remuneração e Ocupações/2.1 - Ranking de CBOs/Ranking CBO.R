#'*Script que calcula a remuneração média por hora trabalhada*

# Carregar pacotes ----

library(tidyverse)# Manipulação dos dados
library(imputeTS) # SUbstituir NA's
library(readxl)   # Leitura de Arquivos em .xlsx

`%notin%` <- Negate(`%in%`)           # Função de filtro
options(readr.show_col_types = FALSE) # Omitir formato das colunas no console

# Importação dos dados ----

rais <- readRDS("../1. Extração dos dados/Dados/RAIS.RDS")

estrutura_cbo <- readRDS("../1. Extração dos dados/Dicionários/Dicionário CBO.RDS") |>
  select(cboocupacao2002, nome_cbo_ocupacao, 
         cbo_familia, nome_cbo_familia)

cbos_filtro <- c(4110, 5211, 5143, 4132, 5173, 3222, 5134, 5174, 4211, # Famíias de CBOs que concentram 95% do tota de víncuos
                 4221, 5142, 4141, 2124, 2235, 7170, 1423, 5132, 7823, 
                 2251, 2521, 5135, 1421, 4101, 7832, 7825, 5141, 4223, 
                 7824, 7152, 3541, 5112, 4131, 5171, 7842, 2523, 3132, 
                 8485, 3515, 2312, 2234, 8483, 9144, 5191, 1414, 2394, 
                 3242, 4122, 2522, 3172, 2410, 3224, 4142, 7155, 3311, 
                 2236, 6220, 7156, 5199, 9922, 3171, 3331, 5201, 3714, 
                 2142, 7241, 2311, 3425, 2611, 7102, 4222, 7841, 2345, 
                 4152, 3341, 3133, 3241, 6210, 3131, 2241, 7321, 5163, 
                 5133, 1415, 5152, 2524, 9511, 4201, 2321, 5136, 2525, 
                 4102, 7166, 2123, 2237, 7822, 7711, 3513, 2313, 3517, 
                 7244, 1425, 7151, 3912, 2532, 3542, 7153, 3516, 2143, 
                 3514, 4151, 2515, 3121, 5101, 4213, 6233, 8414, 5193, 
                 5162, 7741, 2516, 3141, 8621, 9913, 3911, 1422, 2332, 
                 2624, 5153, 2344, 2531, 9112, 7632, 9113, 3511, 3951, 
                 7242, 2512, 4121, 2612, 6410, 7243, 1231, 3421, 5103, 
                 2346, 7631, 7313, 1412, 5151, 9143, 9921, 8118, 3221, 
                 7630, 3252, 9541, 7154, 3731, 7165, 9513, 7233, 1416, 
                 2212, 3144, 5231, 1312, 3424, 3312, 3711, 5241, 2034, 
                 1313, 2238, 7831, 7163, 3732, 3548, 2711, 7311, 3250, 
                 2141, 7662, 3423, 4212, 2122, 1424, 2348, 1114, 2232, 
                 2149, 6230, 8623, 7312, 5161, 7257, 3741, 9101, 5192, 
                 9531, 1417, 5111, 8481, 7663, 7826, 7164, 3185, 2239, 
                 8181, 8401, 3722, 2543, 8418, 1311, 2221, 3134, 7157, 
                 3123, 3251, 3143, 7661, 2252, 9501, 1427, 7212, 7245, 
                 7112, 3744, 1210, 3011, 9192, 6231, 9191, 2617, 6223, 
                 3181, 3532, 7122, 3122, 2613, 1426, 2211, 2233, 2144, 
                 3111, 6232, 3751, 8131, 5242, 9141, 7250, 2153, 8214, 
                 2347, 3003, 7827, 9131, 7821, 3412, 1233, 6321, 3180, 
                 7522, 2614, 5165, 9111, 5121, 7664, 3522, 2621, 2527, 
                 9102, 5131, 3721, 7652, 7213, 8625, 9193, 4241, 3188, 
                 7111, 3115, 4231, 7252, 5102, 7411)

# Código para definir as CBOs

#dados |>
#  filter(!is.na(cbo_familia)) |>
#  group_by(cbo_familia) |>
#  summarise(vinculos = sum(vinculos_recente),
#            freq = (vinculos / 772462) * 100) |>
#  arrange(desc(freq)) |>
#  mutate(freq_acum = cumsum(freq)) |>
#  filter(freq_acum <= 99) |>
#  pull(cbo_familia)

# Tratamento dos dados ----

## Retirar 2011 da base ----

rais <- rais |>
  left_join(estrutura_cbo |>
              select(cboocupacao2002, cbo_familia)) |>
  filter(cbo_familia %in% cbos_filtro)

## Dados do ano mais recente ----

### Vínculos ----

dados_recente_vinculos <- rais |>
  filter(tipovinculo == 1,                 # Celetista
         referencia == max(referencia)) |> # Filtar pelo ano mais recente
  group_by(cbo_familia) %>%
  summarise(referencia = max(referencia),
            vinculos_recente = n())

### Salários ----

dados_recente_salarios <- rais |>
  filter(tipovinculo == 1,              # Celetista
         referencia == max(referencia), # Filtar pelo ano mais recente
         salario_hora != 0) |>
  group_by(cbo_familia) %>%
  summarise(referencia = max(referencia),
            mediana_rendimento_recente = median(salario_dez_defl, na.rm = TRUE), # Salário médio
            mediana_salario_hora_recente = median(salario_hora, na.rm = TRUE))   # Hora de trabalho média

dados_recente <- dados_recente_salarios |>
  left_join(dados_recente_vinculos)

remove(dados_recente_salarios, dados_recente_vinculos)

## Dados do ano base ----

### Vínculos ----

dados_base_vinculos <- rais |>
  filter(tipovinculo == 1,                     # Celetista
         referencia == max(referencia) - 1) |> # Período base - t-1 (2020)
  group_by(cbo_familia) |>
  summarise(vinculos_base = n())

### Salários ----

dados_base_salarios <- rais |>
  filter(tipovinculo == 1,                  # Celetista
         referencia == max(referencia) - 1, # Período base - t-1 (2020)
         salario_hora != 0) |>
  group_by(cbo_familia) |>
  summarise(mediana_rendimento_base = median(salario_dez_defl, na.rm = TRUE), # Salário médio
            mediana_salario_hora_base = median(salario_hora, na.rm = TRUE))   # Hora de trabalho média

dados_base <- dados_base_salarios |>
  left_join(dados_base_vinculos)

remove(dados_base_salarios, dados_base_vinculos)

## Merge das bases ----

dados <- dados_base |>
  left_join(dados_recente, by = "cbo_familia") |>
  left_join(estrutura_cbo |>
              select(cbo_familia, nome_cbo_familia),
            by = "cbo_familia") |>
  mutate(variacao_rendimento = (mediana_rendimento_recente / mediana_rendimento_base) - 1,
         variacao_vinculos = (vinculos_recente / vinculos_base) - 1) |>
  distinct_all()

# Exportar CSV ----

write_excel_csv2(dados, "Painel 2 - Remuneração e Ocupações/Resultados/2.1 - Ranking de CBOs.csv")

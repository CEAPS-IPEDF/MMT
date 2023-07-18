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

cbos_filtro <- c(4110L, 5211L, 5143L, 4132L, 5173L, 3222L, 5134L, 5174L, 4211L, # Famílias de CBOs que concentram 95% do total de vínculos
                 4221L, 5142L, 4141L, 2124L, 2235L, 7170L, 1423L, 5132L, 7823L, 
                 2251L, 2521L, 5135L, 1421L, 4101L, 7832L, 7825L, 5141L, 4223L, 
                 7824L, 7152L, 3541L, 5112L, 4131L, 5171L, 7842L, 2523L, 3132L, 
                 8485L, 3515L, 2312L, 2234L, 8483L, 9144L, 5191L, 1414L, 2394L, 
                 3242L, 4122L, 2522L, 3172L, 2410L, 3224L, 4142L, 7155L, 3311L, 
                 2236L, 6220L, 7156L, 5199L, 9922L, 3171L, 3331L, 5201L, 3714L, 
                 2142L, 7241L, 2311L, 3425L, 2611L, 7102L, 4222L, 7841L, 2345L, 
                 4152L, 3341L, 3133L, 3241L, 6210L, 3131L, 2241L, 7321L, 5163L, 
                 5133L, 1415L, 5152L, 2524L, 9511L, 4201L, 2321L, 5136L, 2525L, 
                 4102L, 7166L, 2123L, 2237L, 7822L, 7711L, 3513L, 2313L, 3517L, 
                 7244L, 1425L, 7151L, 3912L, 2532L, 3542L, 7153L, 3516L, 2143L, 
                 3514L, 4151L, 2515L, 3121L, 5101L, 4213L, 6233L, 8414L, 5193L, 
                 5162L, 7741L, 2516L, 3141L, 8621L, 9913L, 3911L, 1422L, 2332L, 
                 2624L, 5153L, 2344L, 2531L, 9112L, 7632L, 9113L, 3511L, 3951L, 
                 7242L, 2512L, 4121L, 2612L, 6410L, 7243L, 1231L, 3421L, 5103L, 
                 2346L, 7631L, 7313L, 1412L, 5151L, 9143L, 9921L, 8118L, 3221L, 
                 7630L, 3252L, 9541L, 7154L, 3731L, 7165L, 9513L, 7233L, 1416L, 
                 2212L, 3144L, 5231L, 1312L, 3424L, 3312L, 3711L, 5241L, 2034L, 
                 1313L, 2238L, 7831L, 7163L, 3732L, 3548L, 2711L, 7311L, 3250L, 
                 2141L, 7662L, 3423L, 4212L, 2122L, 1424L, 2348L, 1114L, 2232L, 
                 2149L, 6230L, 8623L, 7312L, 5161L, 7257L, 3741L, 9101L, 5192L, 
                 9531L, 1417L, 5111L, 8481L, 7663L, 7826L, 7164L, 3185L, 2239L, 
                 8181L, 8401L, 3722L, 2543L, 8418L, 1311L, 2221L, 3134L, 7157L, 
                 3123L, 3251L, 3143L, 7661L, 2252L, 9501L, 1427L, 7212L, 7245L, 
                 7112L, 3744L, 1210L, 3011L, 9192L, 6231L, 9191L, 2617L, 6223L, 
                 3181L, 3532L, 7122L, 3122L, 2613L, 1426L, 2211L, 2233L, 2144L, 
                 3111L, 6232L, 3751L, 8131L, 5242L, 9141L, 7250L, 2153L, 8214L, 
                 2347L, 3003L, 7827L, 9131L, 7821L, 3412L, 1233L, 6321L, 3180L, 
                 7522L, 2614L, 5165L, 9111L, 5121L, 7664L, 3522L, 2621L, 2527L, 
                 9102L, 5131L, 3721L, 7652L, 7213L, 8625L, 9193L, 4241L, 3188L, 
                 7111L, 3115L, 4231L, 7252L, 5102L, 7411L)

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

dados |>
  arrange(variacao_rendimento) |>
  slice(1:10) |>
  ggplot(aes(x = reorder(nome_cbo_familia, variacao_rendimento),
             y = variacao_rendimento)) +
  geom_col(fill = "#0f6bb5",
           width = 0.8) +
  geom_text(aes(x = reorder(nome_cbo_familia,variacao_rendimento),
                y = .06, 
                label = scales::percent(round(variacao_rendimento,3)),
                fontface = "bold"), 
            show.legend = F,
            hjust = "right") +
  labs(x = "",
       y = "") +
  scale_y_continuous(label = scales::percent) +
  theme_minimal(base_size = 15) +
  coord_flip()

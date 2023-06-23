#'*Script que calcula o ranking de média salarial por CBO*

# Carregar pacotes ----

library(tidyverse) # Manipulação dos dados
library(imputeTS) # SUbstituir NA's

`%notin%` <- Negate(`%in%`)           # Função de filtro
options(readr.show_col_types = FALSE) # Omitir formato das colunas no console

# Importação dos dados ----

rais <- readRDS("../1. Extração dos dados/RAIS/Dados/RAIS.RDS") |>
  mutate(tipo_emprego = case_when(tipovinculo == 1 ~ "Celetista",
                                  TRUE ~ NA))
estrutura_cbo <- readRDS("../1. Extração dos dados/RDS/Dicionário CBO.RDS") |>
  select(cboocupacao2002, nome_cbo_ocupacao, 
         cbo_subgrupo_principal, nome_cbo_subgrupo_principal)

# Tratamento dos dados ----

## Retirar 2011 da base ----

rais <- rais |>
  filter(referencia != 2011,
         !is.na(cboocupacao2002))

## Dados do ano mais recente ----

dados_recente <- rais |>
  filter(tipovinculo == 1,                 # Celetista
         referencia == max(referencia)) |> # Filtar pelo ano mais recente
  group_by(cboocupacao2002) %>%
  summarise(referencia = max(referencia),
            vinculos_recente = n(),
            media_rendimento_recente = mean(salario_dez_defl, na.rm = TRUE), # Salário médio
            media_salario_hora_recente = mean(salario_hora, na.rm = TRUE))   # Hora de trabalho média

## Dados do ano base ----

dados_base <- rais %>%
  filter(tipovinculo == 1,
         referencia == 2012) %>%
  group_by(cboocupacao2002) %>%
  summarise(vinculos_base = n(),
            media_rendimento_base = mean(salario_dez_defl, na.rm = TRUE),  # Salário médio
            media_salario_hora_base = mean(salario_hora, na.rm = TRUE)) |> # Hora de trabalho média
  mutate_if(is.integer, as.numeric)

## Dados históricos ----

salario_vinculo_cbo <- rais |>
  filter(tipovinculo == 1) |> # Celetista 
  group_by(cboocupacao2002, referencia) %>%
  summarise(vinculos = n(),
            media_rendimento = mean(salario_dez_defl, na.rm = TRUE), # Salário médio
            media_salario_hora = mean(salario_hora, na.rm = TRUE))   # Hora de trabalho médio

## Mediana do histórico ----

dados_historico <- salario_vinculo_cbo |>
  group_by(cboocupacao2002) |>
  summarise(mediana_vinculos_historico = median(vinculos),           # Mediana de vínculos histórico
            mediana_rendimento_historico = median(media_rendimento)) # Mediana de rendimentos histórico

## Merge das bases ----

dados <- dados_base |>
  left_join(dados_recente, by = "cboocupacao2002") %>%
  left_join(dados_historico, by = "cboocupacao2002") |>
  mutate(variacao_rendimento = (media_rendimento_recente / mediana_rendimento_historico) - 1,
         variacao_vinculos = (vinculos_recente / mediana_vinculos_historico) - 1)

dados <- dados %>%
  left_join(estrutura_cbo, by = "cboocupacao2002") %>%
  na_replace(.,"null")%>%
  #replace(is.na(.), "null") %>%
  #mutate_at(vars(cboocupacao2002, cbo_subgrupo_principal), ~ paste0('"', ., '"'))%>%
  select(cboocupacao2002, 
         nome_cbo_ocupacao,
         mediana_vinculos = mediana_vinculos_historico,
         mediana_rendimentos = mediana_rendimento_historico,
         vinculos_ultimo_ano = vinculos_recente,
         media_rendimento_ultimo_ano = media_rendimento_recente,
         media_salario_hora_ultimo_ano = media_salario_hora_recente,
         vinculos_primeiro_ano = vinculos_base,                     # Informações de 2012, discutir a inclusão na reunião
         media_rendimento_primeiro_ano = media_rendimento_base,     # Informações de 2012, discutir a inclusão na reunião
         media_salario_hora_primeiro_ano = media_salario_hora_base, # Informações de 2012, discutir a inclusão na reunião
         variacao_rendimentos = variacao_rendimento,
         variacao_vinculos = variacao_vinculos,
         cbo = nome_cbo_subgrupo_principal,
         codigo_cbo = cbo_subgrupo_principal) |>
  arrange(desc(variacao_rendimentos))

(dados |> 
  arrange(desc(variacao_vinculos)) |> 
  select(cbo = nome_cbo_ocupacao, var = variacao_vinculos) |> 
  head() |> 
  as.data.frame() |> 
  ggplot(aes(x = reorder(cbo,var),y = var)) +
  geom_bar(stat = "identity",fill = "#0f6bb5") + 
  coord_flip() + theme_minimal(15) +
  labs(x = "",y = "", fill = "CBOs com ganho salarial")) |> plotly::ggplotly()

# Exportação dos
#'*Script que calcula o ranking de média salarial por CBO*

# Carregar pacotes ----

library(tidyverse) # Manipulação dos dados

`%notin%` <- Negate(`%in%`)           # Função de filtro
options(readr.show_col_types = FALSE) # Omitir formato das colunas no console

# Importação dos dados ----

rais <- readRDS("../1. Extração dos dados/RAIS/Dados/RAIS.RDS")

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

## Dados históricos ----

salario_vinculo_cbo <- rais |>
  filter(tipovinculo == 1) |> # Celetista 
  group_by(cboocupacao2002, referencia) %>%
  summarise(vinculos = n(),
            media_rendimento = mean(salario_dez_defl, na.rm = TRUE), # Salário médio
            media_salario_hora = mean(salario_hora, na.rm = TRUE))   # Hora de trabalho média

## Mediana do histórico ----

dados_historico <- salario_vinculo_cbo |>
  group_by(cboocupacao2002) |>
  summarise(mediana_vinculos_historico = median(vinculos),           # Mediana de vínculos histórico
            mediana_rendimento_historico = median(media_rendimento)) # Mediana de rendimentos histórico

dados <- dados_recente |>
  left_join(dados_historico, by = "cboocupacao2002") |>
  mutate(#Variação 2012 - 2021 dos sqlários
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
    mutate(tipo = nome, # Técnicas e Não Técnicas
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
#### Scrip gera dados de variação de salário e de vículos no período 2012 - 2021 e no período 2012 - 2021
### Rbind com ocupações gerais e técnicas utilizando medianas na fórmula de cálculo da variação


# Carregando pacotes ----
library(data.table)
library(dplyr)

rstudioapi::writeRStudioPreference("data_viewer_max_columns", 300L)

# Carregando dados ----
#rais
rais = data.table::fread('dados/rais-panorama-2021.csv',encoding = "UTF-8")

# Adicionando os nomes de CBOs ----
# nomes cbos
estrutura_cbo <- readRDS('produto/rds/estrutura-cbo.rds')
# bind
rais <- left_join(rais, estrutura_cbo, by = "cboocupacao2002")

# Limpeza ----
## Limpando CBO ----
rais$cboocupacao2002 <- as.integer(stringr::str_remove(rais$cboocupacao2002, '-')) #remover o traço
rais <- rais %>% filter(referencia != 2011, !is.na(cboocupacao2002))

# Geral ----
## Bases primarias (geral) ----
### Dados 2020 Geral ----
dados_2020 <- rais %>%
  #filtrar para 2020
  filter(referencia == 2020) %>%
  #agrupando por CBO
  group_by(cbo_familia) %>%
  summarise(vinculos_2020 = n(), #Vínculos no ano
            media_rendimento_2020 = mean(salario_dez_defl, na.rm = TRUE)) #Salário médio

### Dados 2021 Geral ----
dados_2021 <- rais %>%
  #filtrar para 2021
  filter(referencia == 2021) %>%
  #agrupando por CBO
  group_by(cbo_familia) %>%
  summarise(vinculos_2021 = n(), #Vínculos no ano
            media_rendimento_2021 = mean(salario_dez_defl, na.rm = TRUE)) #Salário médio

### Dados 2012 Geral ----
dados_2012 <- rais %>%
  #filtrar para 2012
  filter(referencia == 2012) %>%
  #agrupando por CBO
  group_by(cbo_familia) %>%
  summarise(vinculos_2012 = n(),#Vínculos no ano
            media_rendimento_2012 = mean(salario_dez_defl, na.rm = TRUE)) #Salário médio

### Dados 2012-2021 (Geral) ----  
salario_vinculo_cbo <- rais %>%
  #agrupando por CBO e por ano
  group_by(cbo_familia, referencia) %>%
  summarise(vinculos = n(),
            media_rendimento = mean(salario_dez_defl, na.rm = TRUE),
            media_salario_hora = mean(salario_hora, na.rm = TRUE)) 

## Medianas (geral)----
### Mediana 2012-2021 ----
dados_periodo2012_2021 <- salario_vinculo_cbo %>%
  #agrupando por CBO
  group_by(cbo_familia) %>%
  summarise(mediana_vinculos2012_2021 = median(vinculos), #mediana de vínculos 2012 - 2021 
            mediana_rendimento2012_2021 = median(media_rendimento)) #mediana de rendimentos 2012 - 2021

### Mediana 2012-2020 ----
dados_periodo2012_2020 <- salario_vinculo_cbo %>%
  #removendo 2021
  filter(referencia != 2021) %>%
  #agrupando por CBO
  group_by(cbo_familia) %>%
  summarise(mediana_vinculos2012_2020 = median(vinculos), #mediana de vínculos 2012 - 2020 
            mediana_rendimento2012_2020 = median(media_rendimento)) #mediana de rendimentos 2012 - 2021 

## Unindo dados (geral) ----
dados <- dplyr::left_join(dados_2012,
                          dplyr::left_join(dados_2020, dados_2021), 
                          by = "cbo_familia") %>%
  left_join(dados_periodo2012_2020, by = "cbo_familia") %>%
  left_join(dados_periodo2012_2021, by = "cbo_familia")

## Calculando variação dados  ----
dados <- dados %>%
  mutate(tipo_cbo = "geral", # Técnicas e Não Técnicas
         #Variação 2012 - 2020 dos sqlários
         variacao_rendimento2012_2020 = (media_rendimento_2020/
                                           mediana_rendimento2012_2021) -1,
         #Variação 2012 - 2020 dos vínculos
         variacao_vinculos2012_2020 = (vinculos_2020/
                                         mediana_vinculos2012_2021) - 1,
         #Variação 2012 - 2021 dos salários
         variacao_rendimento2012_2021 = (media_rendimento_2021/
                                           mediana_rendimento2012_2021) -1,
         #Variação 2012 - 2020 dos vínculos
         variacao_vinculos2012_2021 = (vinculos_2021/
                                         mediana_vinculos2012_2021) - 1)
# Técnico ----
## Bases primarias (Técnico) ----
### Dados 2020 Geral ----

dados_2020_tec <- rais %>%
  #filtrar para 2020
  filter(referencia == 2020,
         cbo_tec == 1) %>%
  #agrupando por CBO
  group_by(cbo_familia) %>%
  summarise(vinculos_2020 = n(), #Vínculos no ano
            media_rendimento_2020 = mean(salario_dez_defl, na.rm = TRUE)) #Salário médio

### Dados 2021 Geral ----
dados_2021_tec <- rais %>%
  #filtrar para 2021
  filter(referencia == 2021,
         cbo_tec == 1) %>%
  #agrupando por CBO
  group_by(cbo_familia) %>%
  summarise(vinculos_2021 = n(), #Vínculos no ano
            media_rendimento_2021 = mean(salario_dez_defl, na.rm = TRUE)) #Salário médio

### Dados 2012 Geral ----
dados_2012_tec <- rais %>%
  #filtrar para 2012
  filter(referencia == 2012,
         cbo_tec == 1) %>%
  #agrupando por CBO
  group_by(cbo_familia) %>%
  summarise(vinculos_2012 = n(),#Vínculos no ano
            media_rendimento_2012 = mean(salario_dez_defl, na.rm = TRUE)) #Salário médio

### Dados 2012-2021 (Técnico) ----  
salario_vinculo_cbo_tec <- rais %>%
  #filtrando CBOs Tec
  filter(cbo_tec == 1) %>% 
  #agrupando por CBO e por ano
  group_by(cbo_familia, referencia) %>%
  summarise(vinculos = n(),
            media_rendimento = mean(salario_dez_defl, na.rm = TRUE),
            media_salario_hora = mean(salario_hora, na.rm = TRUE)) 

## Medianas (Técnico) ----
### Mediana 2012-2021 ----
dados_periodo2012_2021_tec <- salario_vinculo_cbo_tec %>%
  #agrupando por CBO
  group_by(cbo_familia) %>%
  summarise(mediana_vinculos2012_2021 = median(vinculos), #mediana de vínculos 2012 - 2021 
            mediana_rendimento2012_2021 = median(media_rendimento)) #mediana de rendimentos 2012 - 2021

### Mediana 2012-2020 ----
dados_periodo2012_2020_tec <- salario_vinculo_cbo_tec %>%
  #removendo 2021
  filter(referencia != 2021) %>%
  #agrupando por CBO
  group_by(cbo_familia) %>%
  summarise(mediana_vinculos2012_2020 = median(vinculos), #mediana de vínculos 2012 - 2020 
            mediana_rendimento2012_2020 = median(media_rendimento)) #mediana de rendimentos 2012 - 2021 

## Unindo dados Gerais ----
dados_tec <- dplyr::left_join(dados_2012_tec,
                              dplyr::left_join(dados_2020_tec, dados_2021_tec), 
                              by = "cbo_familia") %>%
  left_join(dados_periodo2012_2020_tec, by = "cbo_familia") %>%
  left_join(dados_periodo2012_2021_tec, by = "cbo_familia")

## Calculando variação dados (Técnico) ----
dados_tec <- dados_tec %>%
  mutate(tipo_cbo = "tecnica", # Técnicas
         #Variação 2012 - 2020 dos sqlários
         variacao_rendimento2012_2020 = (media_rendimento_2020/
                                           mediana_rendimento2012_2021) -1,
         #Variação 2012 - 2020 dos vínculos
         variacao_vinculos2012_2020 = (vinculos_2020/
                                         mediana_vinculos2012_2021) - 1,
         #Variação 2012 - 2021 dos sqlários
         variacao_rendimento2012_2021 = (media_rendimento_2021/
                                           mediana_rendimento2012_2021) -1,
         #Variação 2012 - 2020 dos vínculos
         variacao_vinculos2012_2021 = (vinculos_2021/
                                         mediana_vinculos2012_2021) - 1)

# Unido Bases Geral e Técnica (bind) ----
base_variacao <- rbind(dados, dados_tec)

# Adicionando os nomes de CBOs ----
# nomes cbos
estrutura_cbo <- readRDS('produto/rds/estrutura-cbo.rds') %>% 
  select(-c(cboocupacao2002, nome_cbo_ocupacao))
# bind
cbo_familia_variacao <- left_join(base_variacao, estrutura_cbo, by = "cbo_familia") %>% 
  unique()

# Salvando Base ----
readr::write_excel_csv2(cbo_familia_variacao, 'produto/csv/base-cbos-familia-variacao-sal-vinc.csv')


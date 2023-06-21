#### Scrip gera dados de variação de salário e de vículos no período 2011 - 2019 e no período 2011 - 2020
### Rbind com ocupações gerais e técnicas

# Carregando pacotes
library(data.table)
library(dplyr)

rstudioapi::writeRStudioPreference("data_viewer_max_columns", 300L)

# Carregando dados
rais = data.table::fread('dados/primario/rais-panorama.csv', encoding = "UTF-8")
cbos = data.table::fread('dados/CBO2002 - Ocupacao.csv', encoding = "Latin-1")

# Limpeza ----
## Limpando CBO ----
cbos <- cbos %>% rename(cboocupacao2002 = CODIGO, nome_cbo = TITULO) # renomear Base de CBOs
rais$cboocupacao2002 <- as.integer(stringr::str_remove(rais$cboocupacao2002, '-')) #remover o traço

## Dados 2019 Geral ----
dados_2019 <- rais %>%
  #filtrar para 2019
  filter(referencia == 2019) %>%
  #agrupando por CBO
  group_by(cboocupacao2002) %>%
  summarise(vinculos_2019 = n(), #Vínculos no ano
            media_rendimento_2019 = mean(salario_dez_defl, na.rm = TRUE)) #Salário médio


## Dados 2020 Geral ----
dados_2020 <- rais %>%
  #filtrar para 2020
  filter(referencia == 2020) %>%
  #agrupando por CBO
  group_by(cboocupacao2002) %>%
  summarise(vinculos_2020 = n(), #Vínculos no ano
            media_rendimento_2020 = mean(salario_dez_defl, na.rm = TRUE)) #Salário médio

## Dados 2011 Geral ----

dados_2011 <- rais %>%
  #filtrar para 2011
  filter(referencia == 2011) %>%
  #agrupando por CBO
  group_by(cboocupacao2002) %>%
  summarise(vinculos_2011 = n(),#Vínculos no ano
            media_rendimento_2011 = mean(salario_dez_defl, na.rm = TRUE)) #Salário médio


## Dados 2011-2020 ---- Primeira base 

salario_vinculo_cbo <- rais %>%
  #agrupando por CBO e por ano
  group_by(cboocupacao2002, referencia) %>%
  summarise(vinculos = n(),
            media_rendimento = mean(salario_dez_defl, na.rm = TRUE),
            media_salario_hora = mean(salario_hora, na.rm = TRUE)) 

  #juntar com nome das CBOs
base_cbo_ano <- left_join(salario_vinculo_cbo, cbos, by = "cboocupacao2002") ## base final de vínculos e salario médio por CBO e Ano 

  
## Mediana 2011-2020 ----
  dados_periodo2011_2020 <- salario_vinculo_cbo %>%
  #agrupando por CBO
  group_by(cboocupacao2002) %>%
  summarise(mediana_vinculos2011_2020 = median(vinculos), #mediana de vínculos 2011 - 2020 
            mediana_rendimento2011_2020 = median(media_rendimento)) #mediana de rendimentos 2011 - 2020

  ## Mediana 2011-2019 ----
dados_periodo2011_2019 <- salario_vinculo_cbo %>%
  #removendo 2020
  filter(referencia != 2020) %>%
  #agrupando por CBO
  group_by(cboocupacao2002) %>%
  summarise(mediana_vinculos2011_2019 = median(vinculos), #mediana de vínculos 2011 - 2019 
            mediana_rendimento2011_2019 = median(media_rendimento)) #mediana de rendimentos 2011 - 2020 

## Unindo dados Gerais ----
dados <- dplyr::left_join(dados_2011,
                         dplyr::left_join(dados_2019, dados_2020), 
                         by = "cboocupacao2002") %>%
  left_join(dados_periodo2011_2019, by = "cboocupacao2002") %>%
  left_join(dados_periodo2011_2020, by = "cboocupacao2002")


dados <- dplyr::left_join(cbos, dados, by = "cboocupacao2002")

## Calculando variação dados Gerais ----
dados <- dados %>%
  mutate(tipo_cbo = "geral", # Técnicas e Não Técnicas
         #Variação 2011 - 2019 dos sqlários
         variacao_rendimento2011_2019 = (media_rendimento_2019/
                                        mediana_rendimento2011_2020) -1,
         #Variação 2011 - 2019 dos vínculos
         variacao_vinculos2011_2019 = (vinculos_2019/
                                      mediana_vinculos2011_2020) - 1,
         #Variação 2011 - 2020 dos sqlários
         variacao_rendimento2011_2020 = (media_rendimento_2020/
                                        mediana_rendimento2011_2020) -1,
         #Variação 2011 - 2019 dos vínculos
         variacao_vinculos2011_2020 = (vinculos_2020/
                                      mediana_vinculos2011_2020) - 1)

#############################################################################

## Dados 2019 Geral ----
dados_2019_tec <- rais %>%
  #filtrar para 2019
  filter(referencia == 2019,
         cbo_tec == 1) %>%
  #agrupando por CBO
  group_by(cboocupacao2002) %>%
  summarise(vinculos_2019 = n(), #Vínculos no ano
            media_rendimento_2019 = mean(salario_dez_defl, na.rm = TRUE)) #Salário médio


## Dados 2020 Geral ----
dados_2020_tec <- rais %>%
  #filtrar para 2020
  filter(referencia == 2020,
         cbo_tec == 1) %>%
  #agrupando por CBO
  group_by(cboocupacao2002) %>%
  summarise(vinculos_2020 = n(), #Vínculos no ano
            media_rendimento_2020 = mean(salario_dez_defl, na.rm = TRUE)) #Salário médio

## Dados 2011 Geral ----

dados_2011_tec <- rais %>%
  #filtrar para 2011
  filter(referencia == 2011,
         cbo_tec == 1) %>%
  #agrupando por CBO
  group_by(cboocupacao2002) %>%
  summarise(vinculos_2011 = n(),#Vínculos no ano
            media_rendimento_2011 = mean(salario_dez_defl, na.rm = TRUE)) #Salário médio

##### Base de dados de vículos e salários por CBO e Ano ###############
salario_vinculo_cbo_tec <- rais %>%                                   #
  filter(cbo_tec == 1) %>%                                            #
  #agrupando por CBO e por ano                                        #
  group_by(cboocupacao2002, referencia) %>%                           #
  summarise(vinculos = n(),                                           #
            media_rendimento = mean(salario_dez_defl, na.rm = TRUE),  #
            media_salario_hora = mean(salario_hora, na.rm = TRUE))    #


## Mediana 2011-2020 ----
dados_periodo2011_2020_tec <- salario_vinculo_cbo_tec %>%
  #agrupando por CBO
  group_by(cboocupacao2002) %>%
  summarise(mediana_vinculos2011_2020 = median(vinculos), #mediana de vínculos 2011 - 2020 
            mediana_rendimento2011_2020 = median(media_rendimento)) #mediana de rendimentos 2011 - 2020

## Mediana 2011-2019 ----

dados_periodo2011_2019_tec <- salario_vinculo_cbo_tec %>%
  #removendo 2020
  filter(referencia != 2020) %>%
  #agrupando por CBO
  group_by(cboocupacao2002) %>%
  summarise(mediana_vinculos2011_2019 = median(vinculos), #mediana de vínculos 2011 - 2019 
            mediana_rendimento2011_2019 = median(media_rendimento)) #mediana de rendimentos 2011 - 2020 

## Unindo dados Gerais ----
dados_tec <- dplyr::left_join(dados_2011_tec,
                          dplyr::left_join(dados_2019_tec, dados_2020_tec), 
                          by = "cboocupacao2002") %>%
  left_join(dados_periodo2011_2019_tec, by = "cboocupacao2002") %>%
  left_join(dados_periodo2011_2020_tec, by = "cboocupacao2002")


dados_tec <- dplyr::left_join(cbos, dados_tec, by = "cboocupacao2002")

## Calculando variação dados Gerais ----
dados_tec <- dados_tec %>%
  mutate(tipo_cbo = "tecnica", # Técnicas
         #Variação 2011 - 2019 dos sqlários
         variacao_rendimento2011_2019 = (media_rendimento_2019/
                                           mediana_rendimento2011_2020) -1,
         #Variação 2011 - 2019 dos vínculos
         variacao_vinculos2011_2019 = (vinculos_2019/
                                         mediana_vinculos2011_2020) - 1,
         #Variação 2011 - 2020 dos sqlários
         variacao_rendimento2011_2020 = (media_rendimento_2020/
                                           mediana_rendimento2011_2020) -1,
         #Variação 2011 - 2019 dos vínculos
         variacao_vinculos2011_2020 = (vinculos_2020/
                                         mediana_vinculos2011_2020) - 1)

#### BASE 2 - Juntando as duas bases
base_variacao <- rbind(dados, dados_tec)

readr::write_excel_csv2(base_variacao, 'cbos-variacao.csv')
readr::write_excel_csv2(base_variacao, 'cbos-variacao_08122022.csv')

# BASE 1 - Juntando com nome das CBOs
base_cbo_ano_tec <- left_join(salario_vinculo_cbo, cbos, by = "cboocupacao2002")


readr::write_excel_csv2(base_cbo_ano_tec, 'cbos-ano.csv')

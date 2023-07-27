#'*Script que gera os resultados do Painel 2*

# Carregar pacotes ----

library(tidyverse) # Manipulação dos dados
library(imputeTS)  # SUbstituir NA's
library(readxl)    # Leitura de Arquivos em .xlsx
library(odbc)      # Conectar ao banco de dados

`%notin%` <- Negate(`%in%`)

foiAtualizado <- function(arquivo) {
  
  data <- Sys.Date()
  data_arquivo <- as.Date(file.info(paste0("Painel 2 - Remuneração e Ocupações/Resultados/", arquivo, ".csv"))$mtime)
  
  if (data >= data_arquivo) {
    return(TRUE)
  } else {
    return(FALSE)
  }
  
}

# Rodar scripts ----

rais <- readRDS("../1. Extração dos dados/Dados/RAIS.RDS")

## 2.1 - Ranking de CBOs ----

source("Painel 2 - Remuneração e Ocupações/2.1 - Ranking de CBOs/Ranking CBO.R")

## 2.2 - Remuneração média ----

source("Painel 2 - Remuneração e Ocupações/2.2 - Remuneração média/Remuneração média.R")

## 2.3 - Número de vínculos ----

source("Painel 2 - Remuneração e Ocupações/2.3 - Número de vínculos/Número de vínculos.R")

## 2.4 - Taxa de rotatividade ----

source("Painel 2 - Remuneração e Ocupações/2.4 - Taxa de rotatividade/Rotatividade.R")

# Checar se foi atualizado ----

arquivos <- c("2.1 - Ranking de CBOs",
              "2.2 - Remuneração média",
              "2.3 - Número de vínculos",
              "2.4 - Taxa de rotatividade")

sapply(arquivos, foiAtualizado)

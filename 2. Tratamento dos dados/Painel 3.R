#'*Script que gera os resultados do Painel 3*

# Carregar pacotes ----

library(tidyverse) # Manipulação dos dados
library(imputeTS)  # SUbstituir NA's
library(readxl)    # Leitura de Arquivos em .xlsx
library(stringr)
library(treemapify)

foiAtualizado <- function(arquivo) {
  
  data <- Sys.Date()
  data_arquivo <- as.Date(file.info(paste0("Painel 3 - Ocupações Técnicas/Resultados/", arquivo, ".csv"))$mtime)
  
  if (data >= data_arquivo) {
    return(TRUE)
  } else {
    return(FALSE)
  }
  
}

# Rodar scripts ----

rais <- readRDS("../1. Extração dos dados/Dados/RAIS.RDS")

## 2.1 - Ranking de CBOs ----

source("Painel 3 - Ocupações Técnicas/3.1 - Ocupações para Empresas/Ocupações para Empresas.R")

## 2.2 - Remuneração média ----

source("Painel 3 - Ocupações Técnicas/3.2 - Empresas para ocupações/Empresas para Ocupações.R")

## 2.3 - Número de vínculos ----

source("Painel 3 - Ocupações Técnicas/3.3 - Ocupações e eixos/Ocupações e Eixos.R")

## 2.4 - Taxa de rotatividade ----

source("Painel 3 - Ocupações Técnicas/3.4 - Remuneração e Vínculos/Remuneração e Vínculos.R")

# Checar se foi atualizado ----

arquivos <- c("3.1 - Ocupações para empresas",
              "3.2 - Empresas para ocupações",
              "3.3 - Ocupações e eixos",
              "3.4 - Remuneração e vínculos")

sapply(arquivos, foiAtualizado)

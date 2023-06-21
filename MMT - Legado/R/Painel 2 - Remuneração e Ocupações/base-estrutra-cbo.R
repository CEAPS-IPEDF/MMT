# Script que une as estruturas de CBO relevantes ao MMT: Grande Grupo, Subgrupo Principal

# Limpa Diretório 
rm(list=ls(all=T))
# Dados sem notação científica
options(scipen=100)

# Pacotes ----
library(data.table)
library(tidyverse)
library(readxl)

# Ler os dados ----
## Grande Grupo ----
cbo_grande_grupo <- data.table(read_excel('dados/nomes_cbo.xlsx', 
                               sheet = 1, 
                               col_names = TRUE))

# Corringindo formato da variável de double para character
cbo_grande_grupo$cbo_grande_grupo <- as.character(cbo_grande_grupo$cbo_grande_grupo) 

## Subgrupo Principal ----
cbo_subgrupo_principal <- read_excel('dados/nomes_cbo.xlsx', 
                                     sheet = 2, 
                                     col_names = TRUE)
# Corringindo formato da variável
cbo_subgrupo_principal$cbo_subgrupo_principal <- as.character(as.integer(cbo_subgrupo_principal$cbo_subgrupo_principal))
## Subgrupo ----
cbo_subgrupo <- read_excel('dados/nomes_cbo.xlsx', 
                           sheet = 3, 
                           col_names = TRUE)
## Familia ----
cbo_familia <- read_excel('dados/nomes_cbo.xlsx', 
                           sheet = 4, 
                           col_names = TRUE)

# Corringindo formato da variável
cbo_subgrupo$cbo_subgrupo <- as.character(as.integer(cbo_subgrupo$cbo_subgrupo))
cbo_familia$cbo_familia <- as.character(as.integer(cbo_familia$cbo_familia))

## Ocupação ----
cbo_ocupacao <- data.table(read_excel('dados/nomes_cbo.xlsx', 
                           sheet = 5, 
                           col_names = TRUE))

# Recortando os dígitos das CBOs ----
## Grande Grupo ----
cbo_ocupacao[, cbo_grande_grupo := as.character(as.integer(stringr::str_pad(cbo_ocupacao$cboocupacao2002, width = 6, side = 'left', pad = '0'))  %/% 1e5)]
## Sub Grupo Principal ----
cbo_ocupacao[, cbo_subgrupo_principal := as.character(as.integer(stringr::str_pad(cbo_ocupacao$cboocupacao2002, width = 6, side = 'left', pad = '0'))  %/% 1e4)]
## Sub Grupo ----
cbo_ocupacao[, cbo_subgrupo := as.character(as.integer(stringr::str_pad(cbo_ocupacao$cboocupacao2002, width = 6, side = 'left', pad = '0'))  %/% 1e3)]
## Família ----
cbo_ocupacao[, cbo_familia := as.character(as.integer(stringr::str_pad(cbo_ocupacao$cboocupacao2002, width = 6, side = 'left', pad = '0'))  %/% 1e2)]

# Juntando Bases ----
cbo_ocupacao <- left_join(cbo_ocupacao, cbo_grande_grupo, by = "cbo_grande_grupo")
cbo_ocupacao <- left_join(cbo_ocupacao, cbo_subgrupo_principal, by = "cbo_subgrupo_principal")
cbo_ocupacao <- left_join(cbo_ocupacao, cbo_subgrupo, by = "cbo_subgrupo")
cbo_ocupacao <- left_join(cbo_ocupacao, cbo_familia, by = "cbo_familia")

# Conversão geral de character para integer

cbo_ocupacao <- data.frame(cbo_ocupacao)
codigos <- c("cboocupacao2002","cbo_grande_grupo","cbo_subgrupo_principal","cbo_subgrupo","cbo_familia")
cbo_ocupacao[codigos] <- sapply(cbo_ocupacao[codigos],as.integer)

# Base Final ----
view(cbo_ocupacao)

# Limpando bases auxiliares ----
rm(list = ls()[-match("cbo_ocupacao", ls())])

# Salvando Base ----
# RDS
saveRDS(cbo_ocupacao, 'RDS/estrutura-cbo.rds')
# csv
write_excel_csv2(cbo_ocupacao,'produto/csv/estrutura-cbo.csv')

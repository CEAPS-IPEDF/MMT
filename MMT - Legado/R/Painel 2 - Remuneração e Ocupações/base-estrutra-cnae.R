# Script que une as estruturas de CNAE relevantes ao MMT

# Limpa Diretório 
rm(list=ls(all=T))
# Dados sem notação científica
options(scipen=100)

# Pacotes ----
library(tidyverse)

# Ler os dados ----
## Divisão ----
cnae_div <- data.table::data.table(readxl::read_excel('dados/nomes_cnae.xlsx',
                                                   sheet = 1,
                                                   col_names = TRUE))
## Grupo ----
cnae_grup <- data.table::data.table(readxl::read_excel('dados/nomes_cnae.xlsx',
                                                   sheet = 2,
                                                   col_names = TRUE))
## Subclasse ----
cnae_sub <- data.table::data.table(readxl::read_excel('dados/nomes_cnae.xlsx',
                                                   sheet = 3,
                                                   col_names = TRUE))
# Adiciona colunas de subdivisão ----
## Divisão ----
cnae_sub[,cnae_divisao := substring(cnae_sub$cnae20subclasse,1,2)]
## Grupo ----
cnae_sub[,cnae_grupo := substring(cnae_sub$cnae20subclasse,1,3)]

# Une as bases ----
estrutura_cnae <- cnae_sub %>% 
  dplyr::left_join(cnae_grup) %>% 
  dplyr::left_join(cnae_div) 

# Salvando base ----
saveRDS(estrutura_cnae,"RDS/estrutura-cnae.rds")  



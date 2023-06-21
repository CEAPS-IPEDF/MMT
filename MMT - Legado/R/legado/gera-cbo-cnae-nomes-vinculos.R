# Carrega pacotes ####
library(data.table)
library(tidyverse)
library(readxl)

# Diretorio ####
diretorio <- c("P:/RECENTES/DIEPS/GEFAPS/GEFAPS/2022/Panorama da Capacitação/rais-panorama/dados")
rais <- c("/primario/rais-panorama.csv")


# Ler os dados ####

## RAIS
dados <- data.table::fread(paste0(diretorio, rais)) #RAIS bruta
base <- dados %>% select(referencia, cboocupacao2002, cbo_tec, cnae20subclasse) # Variáveis de interesse


## Nomes CBO
cbo_grande_grupo <- read_excel(paste0(diretorio,'/estrutura_cbo/nomes_cbo.xlsx'), 
                        sheet = 1, 
                        col_names = TRUE)

cbo_grande_grupo$cbo_grande_grupo <- as.character(cbo_grande_grupo$cbo_grande_grupo)

cbo_subgrupo_principal <- read_excel(paste0(diretorio,'/estrutura_cbo/nomes_cbo.xlsx'), 
                               sheet = 2, 
                               col_names = TRUE)
cbo_subgrupo_principal$cbo_subgrupo_principal <- as.character(cbo_subgrupo_principal$cbo_subgrupo_principal)

cbo_subgrupo <- read_excel(paste0(diretorio,'/estrutura_cbo/nomes_cbo.xlsx'), 
                                     sheet = 3, 
                                     col_names = TRUE)
cbo_ocupacao <- read_excel(paste0(diretorio,'/estrutura_cbo/nomes_cbo.xlsx'), 
                           sheet = 4, 
                           col_names = TRUE)

## Nomes CNAE 2.0
cnae_divisao <- read_excel(paste0(diretorio,'/estrutura_cbo/nomes_cnae.xlsx'), 
                           sheet = 1, 
                           col_names = TRUE)
cnae_grupo <- read_excel(paste0(diretorio,'/estrutura_cbo/nomes_cnae.xlsx'), 
                           sheet = 2, 
                           col_names = TRUE)
cnae_subclasse <- read_excel(paste0(diretorio,'/estrutura_cbo/nomes_cnae.xlsx'), 
                         sheet = 3, 
                         col_names = TRUE)

# Corrijindo Cnae 2.3 da base ####

base[base$cnae20subclasse ==1091101, "cnae20subclasse"] <-1091100
base[base$cnae20subclasse ==1091102, "cnae20subclasse"] <-4721101
base[base$cnae20subclasse ==1610203, "cnae20subclasse"] <-1610201
base[base$cnae20subclasse ==1822901, "cnae20subclasse"] <-1822900
base[base$cnae20subclasse ==1822902, "cnae20subclasse"] <-1822900
base[base$cnae20subclasse ==1822999, "cnae20subclasse"] <-1822900
base[base$cnae20subclasse ==2013401, "cnae20subclasse"] <-2013400
base[base$cnae20subclasse ==2539001, "cnae20subclasse"] <-2539000
base[base$cnae20subclasse ==3091101, "cnae20subclasse"] <-3091100
base[base$cnae20subclasse ==3250709, "cnae20subclasse"] <-3250707
base[base$cnae20subclasse ==3511501, "cnae20subclasse"] <-3511500
base[base$cnae20subclasse ==3511502, "cnae20subclasse"] <-3511500
base[base$cnae20subclasse ==4520008, "cnae20subclasse"] <-4520001
base[base$cnae20subclasse ==4541206, "cnae20subclasse"] <-4541205
base[base$cnae20subclasse ==4713004, "cnae20subclasse"] <-4713001
base[base$cnae20subclasse ==4713005, "cnae20subclasse"] <-4713001
base[base$cnae20subclasse ==4729602, "cnae20subclasse"] <-4729699
base[base$cnae20subclasse ==4744006, "cnae20subclasse"] <-4744005
base[base$cnae20subclasse ==4751201, "cnae20subclasse"] <-4751200
base[base$cnae20subclasse ==4751202, "cnae20subclasse"] <-4751200
base[base$cnae20subclasse ==5611204, "cnae20subclasse"] <-5611202
base[base$cnae20subclasse ==5611205, "cnae20subclasse"] <-5611202
base[base$cnae20subclasse ==5812301, "cnae20subclasse"] <-5812300
base[base$cnae20subclasse ==5812302, "cnae20subclasse"] <-5812300
base[base$cnae20subclasse ==5822101, "cnae20subclasse"] <-5822100
base[base$cnae20subclasse ==6201501, "cnae20subclasse"] <-6201500
base[base$cnae20subclasse ==6201502, "cnae20subclasse"] <-6201500
base[base$cnae20subclasse ==6810203, "cnae20subclasse"] <-6810202
base[base$cnae20subclasse ==7410299, "cnae20subclasse"] <-7410202
base[base$cnae20subclasse ==8020001, "cnae20subclasse"] <-8020000
base[base$cnae20subclasse ==8020002, "cnae20subclasse"] <-8020000
base[base$cnae20subclasse ==8690903, "cnae20subclasse"] <-8690901
base[base$cnae20subclasse ==8690904, "cnae20subclasse"] <-8690999
base[base$cnae20subclasse ==9412001, "cnae20subclasse"] <-9412000
base[base$cnae20subclasse ==9412099, "cnae20subclasse"] <-9412000
base[base$cnae20subclasse ==9609206, "cnae20subclasse"] <-9609299
base[base$cnae20subclasse ==9609207, "cnae20subclasse"] <-9609203
base[base$cnae20subclasse ==9609208, "cnae20subclasse"] <-9609203
base[base$cnae20subclasse ==4713004, "cnae20subclasse"] <-4713001

# Criando códigos para agregações Cnae e CBO ####

# Recortando os dígitos das CBOs
## Grande Grupo
base[, cbo_grande_grupo := as.character(as.integer(stringr::str_pad(stringr::str_remove(cboocupacao2002, '-'), width = 6, side = 'left', pad = '0'))  %/% 1e5)]
## Sub Grupo Principal
base[, cbo_subgrupo_principal := as.character(as.integer(stringr::str_pad(stringr::str_remove(cboocupacao2002, '-'), width = 6, side = 'left', pad = '0'))  %/% 1e4)]
## Sub Grupo 
base[, cbo_subgrupo := as.character(as.integer(stringr::str_pad(stringr::str_remove(cboocupacao2002, '-'), width = 6, side = 'left', pad = '0'))  %/% 1e3)]
## Removendo "-" da CBO
base$cboocupacao2002 <- as.character(stringr::str_remove(base$cboocupacao2002, "-"))

# Recortando os dígitos das Cnae
## Divisão
base[, cnae_divisao := as.integer(stringr::str_pad(stringr::str_remove(cnae20subclasse, '-'), width = 7, side = 'left', pad = '0'))  %/% 1e5]
## Grupo
base[, cnae_grupo := as.integer(stringr::str_pad(stringr::str_remove(cnae20subclasse, '-'), width = 7, side = 'left', pad = '0'))  %/% 1e4]


# Criando a base de vínculos ####
## Geral
base_geral <- base %>%
  group_by(referencia,
           cboocupacao2002, cbo_subgrupo, cbo_subgrupo_principal, cbo_grande_grupo,
           cnae20subclasse, cnae_grupo, cnae_divisao ) %>%
  summarise(vinculos = n(),
            tipo = "geral")
## Técnicas
base_tec <- base %>%
  filter(cbo_tec == 1) %>%
  group_by(referencia,
           cboocupacao2002, cbo_subgrupo, cbo_subgrupo_principal, cbo_grande_grupo,
           cnae20subclasse, cnae_grupo, cnae_divisao ) %>%
  summarise(vinculos = n(),
            tipo = "tecnica")
## Junção
bind <- rbind(base_geral, base_tec)

# Inserindo nomes ####
# CBOs
base_final <- left_join(bind, cbo_grande_grupo, by = "cbo_grande_grupo") %>%
  left_join(cbo_subgrupo_principal, by = "cbo_subgrupo_principal") %>%
  left_join(cbo_subgrupo, by = "cbo_subgrupo") %>%
  left_join(cbo_ocupacao, by = "cboocupacao2002")
# Cnae
cnae_divisao$cnae_divisao <- as.double(cnae_divisao$cnae_divisao)
cnae_grupo$cnae_grupo <- as.double(cnae_grupo$cnae_grupo)
cnae_subclasse$cnae20subclasse <- as.double(cnae_subclasse$cnae20subclasse)

base_final <- left_join(base_final, cnae_divisao, by = "cnae_divisao") %>%
  left_join(cnae_grupo, by = "cnae_grupo") %>%
  left_join(cnae_subclasse, by = "cnae20subclasse")

#Corrijindo erro de CBOs não declaradas
base_final[base_final$nome_cbo_grandegrupo == "MEMBROS DAS FORÇAS ARMADAS, POLICIAIS E BOMBEIROS MILITARES", "nome_cbo_grandegrupo"] <- NA 

base_final[is.na(base_final$nome_cbo_grandegrupo), "nome_cbo_grandegrupo"] <- "NÃO DECLARADO"
base_final[is.na(base_final$nome_cbo_subgrupo), "nome_cbo_subgrupo"] <- "NÃO DECLARADO"
base_final[is.na(base_final$nome_cbo_subgrupo_principal), "nome_cbo_subgrupo_principal"] <- "NÃO DECLARADO"
base_final[is.na(base_final$nome_cbo_ocupacao), "nome_cbo_ocupacao"] <- "Não declarado"
base_final[base_final$cboocupacao2002 == "00001", "cboocupacao2002"] <- "0"


# Rearranjando Colunas 
base_final <- base_final %>% 
  select(referencia, tipo, vinculos, 
         cboocupacao2002, nome_cbo_ocupacao, 
         cbo_subgrupo, nome_cbo_subgrupo,
         cbo_subgrupo_principal, nome_cbo_subgrupo_principal,
         cbo_grande_grupo, nome_cbo_grandegrupo,
         cnae20subclasse, nome_cnae_subclasse,
         cnae_grupo, nome_cnae_grupo,
         cnae_divisao, nome_cnae_divisao)

readr::write_excel_csv2(base_final,paste0(diretorio,"/cbo-cnae-nomes-vinculos-ano.csv"))

base2 <- base_final %>%
  group_by(referencia, tipo,
           cboocupacao2002, cbo_subgrupo, cbo_subgrupo_principal,
           cbo_grande_grupo, nome_cbo_ocupacao, nome_cbo_subgrupo,
           nome_cbo_subgrupo_principal, nome_cbo_grandegrupo) %>%
  summarise(vinculos = sum(vinculos))

readr::write_excel_csv2(base2,paste0(diretorio,"/cbo-nomes-vinculos-ano.csv"))

# Carrega funções ----
source("Z:/GEFAPS/2023/PED/INFORMALIDADE/utils.R")

packs<-c("tidyverse", "haven","survey","readxl","realdist",
         "readxl","xlsx","TTR","srvyr","RODBC","easypackages",
         "memisc","zoo","reshape2","lubridate")

lapply(packs, require, character.only = TRUE)

install.packages("odbc")
# rm(list = ls())

# Leitura dos dados no dbCodeplan ----
dbi <- DBI::dbConnect(odbc::odbc(), "db_codeplan",uid=('codeplan'),pwd=('codeplan'))
db <- RODBC::odbcConnect("db_codeplan",uid=('codeplan'),pwd=('codeplan'))

sql = paste0('DB_CODEPLAN.ped.NovaPEDDF', 2016:2017)

arq_2016_2017 = purrr::map_dfr(sql, 
                               function(x){
                                 dados = RODBC::sqlQuery(db, paste0("SELECT * FROM ", x))
                                 names(dados) = stringr::str_to_upper(names(dados))
                                 return(dados)
                               })

# Dados PED ----

#### Variáveis rendimentos ####

sql = paste0('DB_CODEPLAN.ped.NovaPEDDF', 2018:2022)

arq_2018_2022 = purrr::map_dfr(sql, 
                               function(x){
                                 dados = RODBC::sqlQuery(db, paste0("SELECT * FROM ", x))
                                 names(dados) = stringr::str_to_upper(names(dados))
                                 return(dados)
                               })

arq_2016_2017_select <- dplyr::select(.data = arq_2016_2017,
                                      AAMM, SIT, F210, F220, POS, F320, F331, F250, F270)

arq_2018_2022_select <- dplyr::select(.data = arq_2018_2022,
                                      AAMM, SIT, F210, F220, POS, F470, F481, F270, F280)

freq_2016_2017 <- factor(arq_2016_2017_select$AAMM,
                         arq_2016_2017_select$SIT,
                         arq_2016_2017_select$F210,
                         arq_2016_2017_select$F220,
                         arq_2016_2017_select$POS)


frequency_2016_2017 <- summary(arq_2016_2017_select)

#frequencia 2016 e 2017

table(arq_2016_2017_select$AAMM)
table(arq_2016_2017_select$SIT)
table(arq_2016_2017_select$F210)
table(arq_2016_2017_select$F220)
table(arq_2016_2017_select$POS)
table(arq_2016_2017_select$F320)

summary(arq_2016_2017_select)

# frequencia 2018 a 2022

table(arq_2018_2022_select$AAMM)
table(arq_2018_2022_select$SIT)
table(arq_2018_2022_select$F210)
table(arq_2018_2022_select$F220)
table(arq_2018_2022_select$POS)
table(arq_2018_2022_select$F470)

summary(arq_2018_2022_select)

# empilhar data frames 

arq_2016_2022 <- arq_2016_2017_select %>% 
as.data.frame() %>%   
  rename("F320" = "F470",
         "F331" = "F481",
         "F250" = "F280") %>% 
  append(arq_2018_2022_select)


  ls(arq_2016_2022)
  ls(arq_2018_2022_select)
  
  
#### Variáveis demográficas ####



# Fecha conexão com DB
# odbcCloseAll()

#db <- DBI::dbConnect(odbc::odbc(), "db_codeplan",uid=('codeplan'),pwd=('codeplan'))



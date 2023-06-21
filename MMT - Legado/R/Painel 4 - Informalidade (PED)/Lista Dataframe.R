packs<-c("tidyverse", "haven","survey","readxl","realdist",
         "readxl","xlsx","TTR","srvyr","RODBC","easypackages",
         "memisc","zoo","reshape2","lubridate")

# install.packages("realdist")

lapply(packs, require, character.only = TRUE)

# install.packages("odbc")
# rm(list = ls())

#### Leitura dos dados no dbCodeplan ----####

dbi <- DBI::dbConnect(odbc::odbc(), "db_codeplan",uid=('codeplan'),pwd=('codeplan'))
db <- RODBC::odbcConnect("db_codeplan",uid=('codeplan'),pwd=('codeplan'))

#### Tratamento dos arquivos 2016-2017

sql = paste0('DB_CODEPLAN.ped.NovaPEDDF', 2016:2017)

arq_2016_2017 = purrr::map_dfr(sql, 
                               function(x){
                                 dados = RODBC::sqlQuery(db, paste0("SELECT * FROM ", x))
                                 names(dados) = stringr::str_to_upper(names(dados))
                                 return(dados)
                               })

arq_2016_2017_select <- dplyr::select(.data = arq_2016_2017,FATOR,
                                      AAMM, SIT, F210, F220, POS, F470 = F320, F481 = F331, F270 = F250, F280 = F270)


sql = paste0('DB_CODEPLAN.ped.NovaPEDDF', 2018:2022)

arq_2018_2022 = purrr::map_dfr(sql, 
                               function(x){
                                 dados = RODBC::sqlQuery(db, paste0("SELECT * FROM ", x))
                                 names(dados) = stringr::str_to_upper(names(dados))
                                 return(dados)
                               })

# selecionando variáveis

arq_2018_2022_select <- dplyr::select(.data = arq_2018_2022,FATOR,
                                      AAMM, SIT, F210, F220, POS, F470, F481, F270, F280)


ped <- rbind(arq_2016_2017_select,arq_2018_2022_select)

# teste<-arq_2016_2017_select %>% 
#   append(arq_2018_2022_select) %>% 
#   as.data.frame()
typeof(ped)
class(ped)


INPC <- read_excel("W:/GEFAPS/2023/PED/Planilha/INPC1.xlsx")

INPC <- INPC %>% 
  mutate(var = ifelse(row_number()==1,NA,(`indice geral`/100) + 1),
         base100 = ifelse(row_number()==1,100,NA),
         indice = round(cumprod(c(na.omit(base100),na.omit(var))),2)) %>% 
  filter(AAAAMM>=201607 & AAAAMM<=max(AAAAMM))%>% 
  mutate(inflator=last(indice)/indice, 
         AAMM1=as.yearmon(as.character(AAAAMM), "%Y%m"))


# TABELA 14: Tratamento dos dados e criação das categorias Assalariados, Autonomos e outros ----

arq_tab_14a<- ped%>% 
  # dplyr::rename(SIT=sit) %>%
  filter(
    AAMM<=202212 & SIT==4 & F210 !=12 & F220 !=3 & F220 !=6) %>% 
  mutate(         POS1=case_when(POS %in% c(1,2,3,4)~1,
                                 POS %in% c(5,6)~5,
                                 TRUE~as.numeric(POS)),
                  POS1=case_when(!(POS1 %in% c(1,5))~10,
                                 TRUE~POS1),
                  rendacz=case_when(F470==1000000000 ~ F481,TRUE~F470),
                  rendacz=case_when(rendacz>9999999 ~ -1,TRUE~as.numeric(rendacz)),
                  renda=case_when(((POS1==1|POS==8)&(rendacz==0))~ -10,
                                  TRUE~rendacz),
                  renda=case_when((renda==-1|renda==-10)~as.numeric(NA),
                                  TRUE~renda),
                  ano=substr(AAMM,start = 1,stop = 4),
                  AAMM1=as.yearmon(as.character(AAMM), "%Y%m")-1/12)%>%
  left_join(INPC,by="AAMM1")%>%
  mutate(rrenda=(renda),
         renda_at=(rrenda*inflator),
         FATOR1=FATOR/12)

# TABELA 18 - Parte 1: criação das categorias Assalariados Privados, Assalariados Públicos e outros ----


arq_tab_17<-arq_tab_14a %>% 
  filter(POS==1|POS==2|POS==3) %>% 
  mutate(POS_PRIV_PUB= case_when(POS==1|POS==2~1,
                                 POS==3~2, 
                                 TRUE~3))




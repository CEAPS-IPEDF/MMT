#### Carrega funções ---- ####
# source("Z:/GEFAPS/2023/PED/INFORMALIDADE/utils.R") 
packs<-c("tidyverse", "haven","survey","readxl","realdist",
         "readxl","xlsx","TTR","srvyr","RODBC","easypackages",
         "memisc","zoo","reshape2","lubridate")

install.packages("realdist")

lapply(packs, require, character.only = TRUE)

install.packages("odbc")
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

# Dados PED ----

#### Variáveis rendimentos ####

# typeof(arq_2018_2022)

arq_2016_2017_select <- dplyr::select(.data = arq_2016_2017,FATOR,
                                      AAMM, SIT, F210, F220, POS, F320, F331, F250, F270)

# freq_2016_2017 <- factor(arq_2016_2017_select$AAMM,
#                          arq_2016_2017_select$SIT,
#                          arq_2016_2017_select$F210,
#                          arq_2016_2017_select$F220,
#                          arq_2016_2017_select$POS)


# summary(arq_2016_2017_select)

#frequencia 2016 e 2017

# table(arq_2016_2017_select$AAMM)
# table(arq_2016_2017_select$SIT)
# table(arq_2016_2017_select$F210)
# table(arq_2016_2017_select$F220)
# table(arq_2016_2017_select$POS)
# table(arq_2016_2017_select$F320)
# 
# summary(arq_2016_2017_select)

# frequencia 2018 a 2022

# table(arq_2018_2022_select$AAMM)
# table(arq_2018_2022_select$SIT)
# table(arq_2018_2022_select$F210)
# table(arq_2018_2022_select$F220)
# table(arq_2018_2022_select$POS)
# table(arq_2018_2022_select$F470)
# 
# summary(arq_2018_2022_select)

# empilhar data frames --- ATENÇÃO!

# arq_2016_2022 <- arq_2016_2017_select %>% 
#   rename("F320" = "F470",
#          "F331" = "F481",
#          "F250" = "F280") %>% 
#   append(arq_2018_2022_select) %>%  
#   as.data.frame()
# 
# ls(arq_2016_2022)
# ls(arq_2018_2022_select)

arq_2016_2017_select <- arq_2016_2017_select %>% 
  rename("F320" = "F470",
         "F331" = "F481",
         "F250" = "F280")

#### Variáveis demográficas ####


# Fecha conexão com DB
# odbcCloseAll()

#db <- DBI::dbConnect(odbc::odbc(), "db_codeplan",uid=('codeplan'),pwd=('codeplan'))


# Fecha conexão com DB
odbcCloseAll()

# Tratamento da planilha INPC para cálculo do inflator ----

INPC <- read_excel("W:/GEFAPS/2023/PED/Planilha/INPC1.xlsx")

INPC <- INPC %>% 
  mutate(var = ifelse(row_number()==1,NA,(`indice geral`/100) + 1),
         base100 = ifelse(row_number()==1,100,NA),
         indice = round(cumprod(c(na.omit(base100),na.omit(var))),2)) %>% 
  filter(AAAAMM>=201607 & AAAAMM<=max(AAAAMM))%>% 
  mutate(inflator=last(indice)/indice, 
         AAMM1=as.yearmon(as.character(AAAAMM), "%Y%m"))

# %>% rename(AAAAMM=AAM1)

view(INPC)
# TABELA 14 ----
# Preparação do conjunto de dados de entrada ----
## Tratamento dos dados de rendimento ----


# TABELA 14: Tratamento dos dados e criação das categorias Assalariados, Autonomos e outros ----

arq_tab_14a<-arq_2016_2017_select %>% 
  # dplyr::rename(SIT=sit) %>%
  filter(
    AAMM<=201712 & SIT==4 & F210 !=12 & F220 !=3 & F220 !=6) %>% 
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


# TABELA 18 - Parte 2: Base somente com assalariados privados. Uso da var Setor_Cnae  ----

arq_tab_18a<-arq_tab_14a %>% 
  filter(POS==1|POS==2)



rend_ocupados<-rend_real_med(arquivo=arq_tab_14a,var="POS1",categorias=c( "Ano/mês","Ocupados","Assalariados","Autonomos","Outros"),periodo = "anual")
View(rend_ocupados)

rend_assalariados_priv_pub<-rend_real_med(arquivo=arq_tab_18, var="POS_PRIV_PUB",categorias = c("Ano/mês","Assalariados Pub.","Assalariados Priv.","Outros"), periodo="anual")
View(rend_assalariados_priv_pub)

rend_assalariados_priv<-rend_real_med(arquivo=arq_tab_18a, var="SETOR_CNAE",categorias = c("Ano/mês","Assalariados Priv.","Insdústria Transf","Cosnstrução","Comércio","Serviços Total","Não Sabe "),periodo="anual")
View(rend_assalariados_priv)



## INFORMAIS ----

# Informais/Formais ----
arq_tab_14_info_formais<-arq_tab_14a %>% 
  mutate(informais= case_when(POS==2|(POS==5 & F280==2)|(POS==6 & F280==2)~1,
                              TRUE~2))

rend_informais_formais<-rend_real_med(arquivo=arq_tab_14_info_formais,var="informais",categorias=c( "Ano/mês","Ocupados","Informais","Formais"),periodo = "anual")
View(rend_informais_formais)

# Informais  - Assalariados/Autônomos ----

arq_tab_14_info<-arq_tab_14a %>% 
  filter((POS==2|(POS==5 & F280==2)|(POS==6 & F280==2)))

rend_informais<-rend_real_med(arquivo=arq_tab_14_info,var="POS1",categorias=c( "Ano/mês","Informais","Assalariados","Autonomos"),periodo = "anual")


# Informais - Setor de atividade ----
View(rend_informais)

arq_tab_14_info<-arq_tab_14a %>% 
  filter((POS==2|(POS==5 & F280==2)|(POS==6 & F280==2)))

rend_informais_setor<-rend_real_med(arquivo=arq_tab_14_info, var="SETOR_CNAE",categorias = c("Ano/mês","Informais","Insdústria Transf","Construção","Comércio","Serviços Total","Não Sabe "),periodo="anual")
View(rend_informais_setor)


Informais_MMT<-rend_informais_formais %>% 
  left_join(rend_informais) %>% 
  left_join(rend_informais_setor) %>% 
  dplyr::select(-c("Não Sabe "))

#### Informações sobre a PED 2018/2022 ####

sql = paste0('DB_CODEPLAN.ped.NovaPEDDF', 2018:2022)

arq_2016_2017 = purrr::map_dfr(sql, 
                               function(x){
                                 dados = RODBC::sqlQuery(db, paste0("SELECT * FROM ", x))
                                 names(dados) = stringr::str_to_upper(names(dados))
                                 return(dados)
                               })

# selecionando variáveis

arq_2018_2022_select <- dplyr::select(.data = arq_2018_2022,FATOR,
                                      AAMM, SIT, F210, F220, POS, F470, F481, F270, F280)

# Tratamento da planilha INPC para cálculo do inflator ----

INPC <- read_excel("W:/GEFAPS/2023/PED/Planilha/INPC1.xlsx")

INPC <- INPC %>% 
  mutate(var = ifelse(row_number()==1,NA,(`indice geral`/100) + 1),
         base100 = ifelse(row_number()==1,100,NA),
         indice = round(cumprod(c(na.omit(base100),na.omit(var))),2)) %>% 
  filter(AAAAMM>=201607 & AAAAMM<=max(AAAAMM))%>% 
  mutate(inflator=last(indice)/indice, 
         AAMM1=as.yearmon(as.character(AAAAMM), "%Y%m"))


# TABELA 14: Tratamento dos dados e criação das categorias Assalariados, Autonomos e outros ----

arq_tab_14a<-arq_2016_2017_select %>% 
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



######## ------ #######

View(Informais_MMT)

write.csv2(Informais_MMT,"W:/GEFAPS/2023/MMT/produto/rendimentos_informais_MMT.csv")
## Testes consistência ----
table(arq_tab_14a$POS1)
table(arq_tab_14_info$POS1)


Informais_MMT_graf<-Informais_MMT %>%
  pivot_longer(!`Ano/mês`, names_to = "setor", values_to = "rendimento")

# View(Informais_MMT_graf)


#### Linhas Rendimento  ----

ggplot(data=Informais_MMT_graf[Informais_MMT_graf$setor %in% c("Ocupados","Informais","Formais"),],aes(x=`Ano/mês`, y=rendimento, group=setor, color = setor))+
  geom_point(size = 2)+ geom_line(linewidth = .5)+
  theme_minimal() +
  scale_color_manual(values = c("#84b263","#bebf64","#3e9974"))+
  # scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(expand = c(.01,.01)) +
  scale_y_continuous(limits = c(0, 6000),breaks = seq(0, 6000, 1000))+
  labs(x = "",y="Rendimento médio real (Em Reais)",fill="")+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.title=element_blank())

ggplot(data=Informais_MMT_graf[Informais_MMT_graf$setor %in% c("Informais","Assalariados","Autonomos"),],aes(x=`Ano/mês`, y=rendimento, group=setor, color = setor))+
  geom_point(size = 2)+ geom_line(linewidth = .5)+
  theme_minimal() +
  scale_color_manual(values = c("#84b263","#bebf64","#3e9974"))+
  # scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(expand = c(.01,.01)) +
  scale_y_continuous(limits = c(0, 3000),breaks = seq(0, 3000, 1000))+
  labs(x = "",y="Rendimento médio real (Em Reais)",fill="")+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.title=element_blank())

ggplot(data=Informais_MMT_graf[Informais_MMT_graf$setor %in% c("Informais","Comércio","Indústria Transf","Construção","Serviços Total"),],aes(x=`Ano/mês`, y=rendimento, group=setor, color = setor))+
  geom_point(size = 2)+ geom_line(linewidth = 0.9)+
  theme_minimal() +
  scale_color_manual(values = c("#84b263","#bebf64","#3e9974","#2e818d","#3e9974"))+
  # scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(expand = c(.01,.01)) +
  scale_y_continuous(limits = c(0, 3000),breaks = seq(0, 3000, 1000))+
  labs(x = "",y="Rendimento médio real (Em Reais)",fill="")+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.title=element_blank())

# "#064471","#2960a7","#2e818d","#3e9974"
########################################################

teste<-arq_2018_2022%>% 
  filter((POS==2|(POS==5 & F280==2)|(POS==6 & F280==2)))
nrow(teste)
nrow(arq_tab_14_info)
table(arq_2018_2022$AAMM)
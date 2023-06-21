#### Carrega funções ---- ####
# source("Z:/GEFAPS/2023/PED/INFORMALIDADE/utils.R") 


packs<-c("tidyverse", "haven","survey","readxl","realdist",
"readxl","xlsx","TTR","srvyr","RODBC","easypackages",
"memisc","zoo","reshape2","lubridate")

packs
lapply(packs, require, character.only = TRUE)

# install.packages("odbc")
# rm(list = ls())

# Fecha conexão com DB
# odbcCloseAll()



### Leitura dados PEDDF2016 -- JANEIRO A JUNHO 2016

ped_2016_encadeada<- as.data.frame(read_sav("W:/GEFAPS/2023/MMT/dados/PEDDF2016_encadeada.sav"))
dim(ped_2016_encadeada)


#PED 2016 - correção da Q280 ----


dbi <- DBI::dbConnect(odbc::odbc(), "db_codeplan",uid=('codeplan'),pwd=('codeplan'))
db <- RODBC::odbcConnect("db_codeplan",uid=('codeplan'),pwd=('codeplan'))

sql = paste0('DB_CODEPLAN.ped.PEDDF2016')


arq_2016 = purrr::map_dfr(sql,
                          function(x){
                            dados = RODBC::sqlQuery(db, paste0("SELECT * FROM ", x))
                            names(dados) = stringr::str_to_upper(names(dados))
                            return(dados)
                          })

table(arq_2016$AAMM)

table(arq_2016$Q280)

glimpse(arq_2016)

arq_2016_select <- dplyr::select(.data = arq_2016, domic=DOMIC,familia=FAMILIA,pessoa=PESSOA,aamm=AAMM, F280 = Q280)
arq_2016_select$pesquisa<-1


table(arq_2016_select$aamm)
#### Tratamento dos arquivos 2016-2017

sql = paste0('DB_CODEPLAN.ped.NovaPEDDF', 2016)

arq_2016_2 = purrr::map_dfr(sql, 
                            function(x){
                              dados = RODBC::sqlQuery(db, paste0("SELECT * FROM ", x))
                              names(dados) = stringr::str_to_upper(names(dados))
                              return(dados)
                            })

table(arq_2016_2$AAMM)



arq_2016_select_2 <- dplyr::select(.data = arq_2016_2, domic=DOMIC,familia=FAMILIA,pessoa=PESSOA,aamm=AAMM, F280 =F270)
arq_2016_select_2$pesquisa<-2

table(arq_2016_select_2$F280)

arq_2016_select_total<-rbind(arq_2016_select,
                             arq_2016_select_2)
length(arq_2016_select)
length(arq_2016_select_2)

table(arq_2016_select_total$F280)


dim(arq_2016_select_total)
dim(ped_2016_encadeada)

ped_2016_encadeada<-ped_2016_encadeada %>% 
  left_join(arq_2016_select_total) %>% 
  mutate(q280=case_when(pesquisa==1 & F280==0~10,
                          TRUE ~ as.numeric(F280)))
dim(ped_2016_encadeada)

table(ped_2016_encadeada$q280)


# ls(ped_2016_encadeada_1)


# Tratamentos PED 2016 -----
ped_2016_encadeada<-ped_2016_encadeada %>% 
  filter(!is.na(fator_anual)) %>% 
  mutate(
         q421=case_when(q421>9999999 ~ -1000,TRUE ~ q421),
         fi = 368.57,
         inflator=inflatordf,
         rendacz=0,
         rendacz=case_when(q421<=9999999~trunc((q421/inflator)*fi)),
         rendacz=case_when((pesquisa==1) & ((q290==6)|(q300==3)|(q300==8))~ -1000,
                         (pesquisa==2) & ((q290a==13)|(q300a==3)|(q300a==6)) ~ -1000,TRUE ~ rendacz),
         rendacz=case_when(
                         q421== -1000 ~ -1000,
                         rendacz<0 ~ -1000,
                         TRUE ~ rendacz),
         # rendacz=case_when((rendacz== -1000)~as.numeric(NA),
         #                 TRUE~rendacz),
         renda_at=case_when((pesquisa==1) & ((q290==1) & ((q300==1)|(q300==2)|(q300==6))& q421==0)~-1000,
                            (pesquisa==2) & ((q290==1) & ((q300==1)|(q300==4))& q421==0)~-1000,
                            TRUE~rendacz),
         renda_at=case_when((renda_at==-1000)~as.numeric(NA),TRUE~renda_at),
         POS1=case_when(POS %in% c(1,2,3,4)~1,
                        POS %in% c(5,6)~5,
                        TRUE~as.numeric(POS)),
         POS1=case_when(!(POS1 %in% c(1,5))~10,
                        TRUE~POS1),
         POS_PRIV_PUB= case_when(POS==1|POS==2~1,
                                 POS==3~2, 
                                 TRUE~3),
         SIT=sit,
        CNAE1=-1,
        CNAE1=case_when(SIT==4~trunc((RAMO_CNAE/1000)),TRUE~CNAE1),
        CNAE1=case_when((SIT==4 & RAMO_CNAE>=99998) ~ RAMO_CNAE,TRUE~CNAE1),
        CNAE1=case_when((CNAE1>=5 & CNAE1<=9) ~ 1005,
                       (CNAE1==35)~1035,
                       (CNAE1>=36 & CNAE1<=39)~1036,
                       (CNAE1>=10 & CNAE1<=33)~2000,
                       (CNAE1>=41 & CNAE1<=43)~3000,
                       (CNAE1>=45 & CNAE1<=48)~4000,
                       (CNAE1>=49 & CNAE1<=53)~5100,
                       (CNAE1>=58 & CNAE1<=66)|(CNAE1>=69 & CNAE1<=75)~5200,
                       (CNAE1>=77 & CNAE1<=82)~5300,
                       (CNAE1>=84 & CNAE1<=88)~5400,
                       ((CNAE1 %in% c(55,56))|(CNAE1>=90 & CNAE1<=96))~5500,
                       (CNAE1==68)~5600,
                       (CNAE1==97)~5900,
                       (CNAE1 %in% c(1,2,3,99,99999,100001))~9999,
                       (CNAE1==100000)~10000,
                       TRUE ~ as.numeric(CNAE1)),
        SETOR_CNAE=case_when((CNAE1 %in% c(1005,1035,1036))~9999,
                              (CNAE1>=5100 & CNAE1<=5900)~5000,
                              TRUE ~ as.numeric(CNAE1)),
         FATOR1=fator_anual/12,
         AAMM=aamm,
         ano=substr(AAMM,start = 1,stop = 4),
         AAMM1=as.yearmon(as.character(AAMM), "%Y%m")-1/12,
         F210=q290,
         F220=q300,
         F470=q421,
         F270=q280,
         F280=q280
         )
table(ped_2016_encadeada$CNAE1)

dim(ped_2016_encadeada)

table(ped_2016_encadeada$pesquisa)

# Seleção de variávies utilizadas na função "rend_real_med" para estatísticas anuais (2016) ----
# Obs.: Nesse código cosidera-se apenas os dados anuais ----
ped_2016_encadeada_select<-ped_2016_encadeada %>% 
 dplyr::select(c( "renda_at", "AAMM", "AAMM1","ano","FATOR1", "inflator","SIT","POS", "POS1", "POS_PRIV_PUB", "SETOR_CNAE","F280"))

# Leitura dos dados no dbCodeplan 2017 a 2022 ----

dbi <- DBI::dbConnect(odbc::odbc(), "db_codeplan",uid=('codeplan'),pwd=('codeplan'))
db <- RODBC::odbcConnect("db_codeplan",uid=('codeplan'),pwd=('codeplan'))

# #### Tratamento dos arquivos 2016-2017

# 2017----
sql = paste0('DB_CODEPLAN.ped.NovaPEDDF', 2017)

arq_2017 = purrr::map_dfr(sql, 
                               function(x){
                                 dados = RODBC::sqlQuery(db, paste0("SELECT * FROM ", x))
                                 names(dados) = stringr::str_to_upper(names(dados))
                                 return(dados)
                               })


table(arq_2017$F270)
table(arq_2017$F250)


arq_2017_select <- dplyr::select(.data = arq_2017,FATOR,
                                      AAMM, SIT, F210, F220, POS, F470 = F320,
                                      F481 = F331,
                                      F270, F280 = F250,SETOR_CNAE)

arq_2017_select<-arq_2017_select %>% 
  mutate(F210=case_when(F210==6~9,
                        F210==7~10,
                        F210==8~11,
                        F210==9~6,
                        F210==10~7,
                        F210==11~8,
                        F210==12~13,
                        F210==13~12,
                        TRUE~as.numeric(F210),
                        ))


         
         
                     # ,7~10,8~11,9~6,10~7,11~8,12~13,13~12))

# table(arq_2018_2022$F280)


# 2018 a 2022----

sql = paste0('DB_CODEPLAN.ped.NovaPEDDF', 2018:2022)

arq_2018_2022 = purrr::map_dfr(sql, 
                               function(x){
                                 dados = RODBC::sqlQuery(db, paste0("SELECT * FROM ", x))
                                 names(dados) = stringr::str_to_upper(names(dados))
                                 return(dados)
                               })


table(arq_2018_2022$F280)
table(arq_2018_2022$F270)

arq_2018_2022_select <- dplyr::select(.data = arq_2018_2022,FATOR,
                                      AAMM, SIT, F210, F220, POS, F470, 
                                      F481,
                                      F270, F280,SETOR_CNAE)


# Arquivo 2017 a 2022 ----
ped <- rbind(arq_2017_select,arq_2018_2022_select)

ped1<-ped %>% 
  mutate(ano=substr(AAMM,start = 1,stop = 4))

table(ped1$ano,ped1$F280)


# # Tratamento da planilha INPC para cálculo do inflator ----
# 
INPC <- read_excel("W:/GEFAPS/2023/PED/Planilha/INPC1.xlsx")

INPC <- INPC %>% 
  mutate(var = ifelse(row_number()==1,NA,(`indice geral`/100) + 1),
         base100 = ifelse(row_number()==1,100,NA),
         indice = round(cumprod(c(na.omit(base100),na.omit(var))),2)) %>% 
  filter(AAAAMM>=201512 & AAAAMM<=max(AAAAMM))%>% 
  mutate(inflator=last(indice)/indice, 
         AAMM1=as.yearmon(as.character(AAAAMM), "%Y%m"))


# TABELA 14 ----
# Preparação do conjunto de dados 2017 a 2022 ----

# TABELA 14: Tratamento dos dados e criação das categorias Assalariados, Autonomos e outros ----
 
ped_arq_tab_14a<-ped %>% 
  # dplyr::rename(SIT=sit) %>%
  filter(AAMM<=202212 & SIT==4 & F210 !=12 & F220 !=3 & F220 !=6) %>% 
  mutate(         POS1=case_when(POS %in% c(1,2,3,4)~1,
                                 POS %in% c(5,6)~5,
                                 TRUE~as.numeric(POS)),
                  POS1=case_when(!(POS1 %in% c(1,5))~10,
                                 TRUE~POS1),
                  rendacz=case_when(F470==1000000000 & AAMM>=201701 ~ F481, F470==10000001 & AAMM<201701 ~ -10, TRUE~F470),
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
         FATOR1=FATOR/12,
       POS_PRIV_PUB= case_when(POS==1|POS==2~1,
                                        POS==3~2, 
                                        TRUE~3))

# Seleção de variávies utilizadas na função "rend_real_med" para estatísticas anuais (2017-2022) ----
# Obs.: Nesse código cosidera-se apenas os dados anuais ----
 
 ped_arq_tab_14a_select<-ped_arq_tab_14a %>% 
  dplyr::select(c( "renda_at", "AAMM", "AAMM1","ano","FATOR1", "inflator","SIT","POS", "POS1", "POS_PRIV_PUB", "SETOR_CNAE","F280"))
 
 # Concatenação das bases 2016 e 2017-2022 ----

 ped_2016_2022<-rbind(ped_2016_encadeada_select,
                      ped_arq_tab_14a_select
                      )

table(ped_2016_2022$ano,ped_2016_2022$F280)

# Filtro total de ocupados ----
 
 ped_arq_tab_14a_2016_2022<-ped_2016_2022 %>% 
   filter(SIT==4)
 
 # Filtros POS ----
 
 ped_arq_tab_18_2016_2022<-ped_arq_tab_14a_2016_2022 %>% 
   filter(POS==1|POS==2|POS==3) 
 
 # TABELA 18 - Parte 2: Base somente com assalariados privados. Uso da var Setor_Cnae  ----
 
 ped_arq_tab_18a_2016_2022<-ped_arq_tab_14a_2016_2022 %>% 
   filter(POS==1|POS==2)
 
 
 # Processamento dados anuais para a tabela 14 ----
 rend_ocupados<-rend_real_med(arquivo=ped_arq_tab_14a_2016_2022,var="POS1",categorias=c( "Ano/mês","Ocupados","Assalariados","Autonomos","Outros"),periodo = "anual")
 View(rend_ocupados)
 
 # Processamentos dados anuais para a tabela 18 ----
 rend_assalariados_priv_pub<-rend_real_med(arquivo=ped_arq_tab_18_2016_2022, var="POS_PRIV_PUB",categorias = c("Ano/mês","Assalariados","Assalariados Priv.","Assalariados Pub."), periodo="anual")
 View(rend_assalariados_priv_pub)
 
 rend_assalariados_priv<-rend_real_med(arquivo=ped_arq_tab_18a_2016_2022, var="SETOR_CNAE",categorias = c("Ano/mês","Assalariados Priv.","Insdústria Transf","Cosnstrução","Comércio","Serviços Total","Não Sabe "),periodo="anual")
 View(rend_assalariados_priv)
 
 
 
 ## INFORMAIS ----

 # Informais/Formais ----
ped_arq_tab_14_informais<- ped_arq_tab_14a_2016_2022 %>%
    mutate(informais= case_when(POS==2|(POS==5 & F280==2)|(POS==6 & F280==2)~1,
                                TRUE~2))
 #
 rend_informais_formais<-rend_real_med(arquivo=ped_arq_tab_14_informais,var="informais",categorias=c( "Ano/mês","Ocupados","Informais","Formais"),periodo = "anual")
 View(rend_informais_formais)
# # Informais  - Assalariados/Autônomos ----

 arq_tab_14_info<- ped_arq_tab_14a_2016_2022 %>%
   filter((POS==2|(POS==5 & F280==2)|(POS==6 & F280==2)))
 
 table(arq_tab_14_info$ano,arq_tab_14_info$F280)
 
 table(ped_arq_tab_14a_2016_2022$ano,ped_arq_tab_14a_2016_2022$F280)

 rend_informais<-rend_real_med(arquivo=arq_tab_14_info,var="POS1",categorias=c( "Ano/mês","Informais","Assalariados","Autonomos"),periodo = "anual")


 # Informais - Setor de atividade ----
 View(rend_informais)

 arq_tab_14_info<- ped_arq_tab_14a_2016_2022 %>%
   filter((POS==2|(POS==5 & F280==2)|(POS==6 & F280==2)))

 rend_informais_setor<-rend_real_med(arquivo=arq_tab_14_info, var="SETOR_CNAE",categorias = c("Ano/mês","Informais","Insdústria Transf","Construção","Comércio","Serviços Total","Não Sabe "),periodo="anual")
 View(rend_informais_setor)


 Informais_MMT<-rend_informais_formais %>%
   left_join(rend_informais) %>%
   left_join(rend_informais_setor) %>%
   dplyr::select(-c("Não Sabe "))
# ######## ------ #######

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
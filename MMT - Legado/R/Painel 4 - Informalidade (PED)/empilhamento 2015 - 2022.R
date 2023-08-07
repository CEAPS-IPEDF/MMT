#### Carrega funções ---- ####
source("W:/GEFAPS/2023/PED/INFORMALIDADE/utils.R") 

packs<-c("tidyverse", "haven","survey","readxl","realdist",
         "readxl","xlsx","TTR","srvyr","RODBC","easypackages",
         "memisc","zoo","reshape2","lubridate")

packs
lapply(packs, require, character.only = TRUE)

# lapply(packs,install.packages, character.only = TRUE)

# install.packages("odbc")
# rm(list = ls())

# Fecha conexão com DB
# odbcCloseAll()



# #### Tratamento dos arquivos 2015

# 2015----
sql = paste0('DB_CODEPLAN.ped.PEDDF', c(2000:2015))
db <- RODBC::odbcConnect("db_codeplan",uid=('codeplan'),pwd=('codeplan'))

arq_2015 = purrr::map_dfr(sql, 
                          function(x){
                            dados = RODBC::sqlQuery(db, paste0("SELECT * FROM ", x))
                            names(dados) = stringr::str_to_upper(names(dados))
                            return(dados)
                          })


arq_2015_select <- dplyr::select(.data = arq_2015, domic=DOMIC,familia=FAMILIA,pessoa=PESSOA,aamm=AAMM, fator_anual=FATOR3, 
                                 inflatordf=INFLATORDF,
                                 sit=SIT, q290=Q290, q300=Q300, POS, q421=Q421,
                                 SETOR_CNAE, RAMO_CNAE
                                 )


#Criando a variável pesquisa que é igual a 1 para dados antigos
arq_2015_select$pesquisa<-1


# criando variável com valor NA
# Q290 <- 1 - Empregado, 2 - Conta-própria ou autônomo, 3 - Empregador, 4 - Profissional universitário autônomo
#                              5 - Dono de negócio familiar      #6 - Trabalhador familiar

# Q300 <- 1 - Assalariado sem comissão   2 - Assalariado com comissão     3 - Que recebe exclusivamente em  espécie/benefício
#            4 - Que presta serviço militar obrigatório, assistencial ou religioso com alguma  remuneração
#            5 - Que ganha exclusivamente por produção    6 - Doméstico(a) mensalista
#            7 - Doméstico(a) diarista      8 - Doméstico(a) que recebe só em  espécie/benefício  0 - Não se aplica


arq_2015_select$q290a<-NA
arq_2015_select$q300a<-NA

### Leitura dados PEDDF2016 -- JANEIRO A JUNHO 2016
# ped encadeada foi fornecidade pelo DIEESE.

ped_2016_encadeada<- as.data.frame(read_sav("W:/GEFAPS/2023/MMT/MMT - Legado/dados/PEDDF2016_encadeada.sav"))
# dim(ped_2016_encadeada)

#para visualizar toda a base
# View(ped_2016_encadeada)

# visualiza as variáveis que estão na base
# ls(ped_2016_encadeada)


#PED  - 

#lendo somente o ano de 2016 - ped antiga
sql = paste0('DB_CODEPLAN.ped.PEDDF',c(2016))


arq_2016 = purrr::map_dfr(sql,
                          function(x){
                            dados = RODBC::sqlQuery(db, paste0("SELECT * FROM ", x))
                            names(dados) = stringr::str_to_upper(names(dados))
                            return(dados)
                          })

#visualizando a quantidade de observações por mês
# a ped antiga foi encerrada em setembro/2016 e substituída por uma nova metodologia. 
# A nova metodologia começou em julho/2016, portanto durante 3 meses houve duas metodologias sendo aplicadas.
# Por esse motivo, é necessário ter um cuidado maior para trabalhar com esse ano.
# table(arq_2016$AAMM)
# 
# #mostra a quantidade de linhas e colunas e os dados.
# glimpse(arq_2016)

arq_2016_select <- dplyr::select(.data = arq_2016, domic=DOMIC,familia=FAMILIA,pessoa=PESSOA,aamm=AAMM)

#criando a variável que identifica se a pesquisa é antiga
arq_2016_select$pesquisa<-1


# table(arq_2016_select$aamm)
#### Tratamento dos arquivos 2016-2017

sql = paste0('DB_CODEPLAN.ped.NovaPEDDF', 2016)

arq_2016_2 = purrr::map_dfr(sql, 
                            function(x){
                              dados = RODBC::sqlQuery(db, paste0("SELECT * FROM ", x))
                              names(dados) = stringr::str_to_upper(names(dados))
                              return(dados)
                            })


# table(arq_2016_2$AAMM)



arq_2016_select_2 <- dplyr::select(.data = arq_2016_2, domic=DOMIC,familia=FAMILIA,pessoa=PESSOA,aamm=AAMM)
# criando variável que identifica se a metodologia  é nova.

arq_2016_select_2$pesquisa<-2

# vamos combinar os objetos de dados adicionando as linhas de um objeto ao final do outro.
# no caso dos meses 07,08,09/2016, o comando irá somar as observações, pois nesse período temos duas metodologias roando concomitantemente.
arq_2016_select_total<-rbind(arq_2016_select,
                             arq_2016_select_2)

# realizando uma junção dos objetos e salvando em um novo objeto.

ped_2016_encadeada<-ped_2016_encadeada %>%
  left_join(arq_2016_select_total)

# selecionando as variáveis de interesse.
arq_2016_encadeada_select <- dplyr::select(.data = ped_2016_encadeada, domic,familia,pessoa,aamm, fator_anual,
                                           sit, q290, q300, POS, q421, pesquisa,inflatordf,q290a, q300a,
                                           SETOR_CNAE,RAMO_CNAE)

# combinando os objetos de dados de 2005:2015 e 2016.
arq_15_16<-rbind(arq_2015_select,
                 arq_2016_encadeada_select)

# aqui é possível verificar os anos/meses que estão no nosso objeto.
table(arq_15_16$aamm)

# aqui é possível verificar as variáveis que estão no nosso objeto.
# ls(arq_15_16)


# Tratamentos PED 2016 -----
arq_15_16<-arq_15_16 %>% 
  filter(!is.na(fator_anual)) %>%  # Esta função é usada para verificar se um valor é NA (ou seja, um valor ausente). Ela retorna um vetor lógico, onde cada elemento é TRUE se o elemento correspondente no vetor é NA (ausente) e FALSE caso contrário.
  mutate(
    q421=case_when(q421>9999999 ~ -1000,TRUE ~ q421), # recodificando missings - Não se aplica e Sem declaração.
    fi = 368.57, # inflator referente a dezembro/2022
    inflator=inflatordf,
    rendacz=0,
    rendacz=case_when(q421==10000001 & aamm<201701 ~ -10, TRUE~trunc((q421/inflator)*fi)),
    rendacz=case_when((pesquisa==1) & ((q290==6)|(q300==3)|(q300==8))~ -1000, # 6-trabalhador familiar, 3e8-Que recebe exclusivamente em espécie/benefício
                      (pesquisa==2) & ((q290a==13)|(q300a==3)|(q300a==6)) ~ -1000,TRUE ~ rendacz),
    rendacz=case_when(
      q421== -1000 ~ -1000, #recodificando missing
      rendacz<0 ~ -1000, #recodificando missing
      TRUE ~ rendacz),
    # rendacz=case_when((rendacz== -1000)~as.numeric(NA),
    #                 TRUE~rendacz),
    renda_at=case_when((pesquisa==1) & ((q290==1) & ((q300==1)|(q300==2)|(q300==6))& q421==0)~-1000, # 1 - Empregado, 1/2 assalariados, q421= renda
                       (pesquisa==2) & ((q290a==1) & ((q300a==1)|(q300a==4))& q421==0)~-1000,
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
    F470=q421
    #F270=q280,
    #F280=q280
  )

table(arq_15_16$CNAE1)

dim(arq_15_16)

table(arq_15_16$pesquisa)

# Seleção de variávies utilizadas na função "rend_real_med" para estatísticas anuais (2016) ----
# Obs.: Nesse código cosidera-se apenas os dados anuais ----
arq_15_16_select<-arq_15_16 %>% 
  dplyr::select(c( "renda_at", "AAMM", "AAMM1","ano","FATOR1", "inflator","SIT","POS", "POS1", "POS_PRIV_PUB", "SETOR_CNAE"))

# Leitura dos dados no dbCodeplan 2015 a 2022 ----

# #### Tratamento dos arquivos 2016-2017

# 2017----
sql = paste0('DB_CODEPLAN.ped.NovaPEDDF', 2017)

arq_2017 = purrr::map_dfr(sql, 
                          function(x){
                            dados = RODBC::sqlQuery(db, paste0("SELECT * FROM ", x))
                            names(dados) = stringr::str_to_upper(names(dados))
                            return(dados)
                          })

arq_2017_select <- dplyr::select(.data = arq_2017,FATOR,
                                 AAMM, SIT, F210, F220, POS, F470 = F320,
                                 F481 = F331,inflatordf=INFLATORDF,
                                 SETOR_CNAE)

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
                                      F481, inflatordf=INFLATORDF,
                                      SETOR_CNAE)


## Arquivo 2017 a 2022 ----
ped <- rbind(arq_2017_select,arq_2018_2022_select)

ped1<-ped %>% 
  mutate(ano=substr(AAMM,start = 1,stop = 4))

#table(ped1$ano,ped1$F280)
# *Tratamento da planilha INPC para cálculo do inflator*
# 
# INPC <- read_excel("W:/GEFAPS/2023/PED/Planilha/INPC1.xlsx")
# 
# INPC <- INPC %>% 
#   mutate(var = ifelse(row_number()==1,NA,(`indice geral`/100) + 1),
#          base100 = ifelse(row_number()==1,100,NA),
#          indice = round(cumprod(c(na.omit(base100),na.omit(var))),2)) %>% 
#   filter(AAAAMM>=201512 & AAAAMM<=max(AAAAMM))%>% 
#   mutate(inflator=last(indice)/indice, 
#          AAMM1=as.yearmon(as.character(AAAAMM), "%Y%m"))

# TABELA 14 ----
# Preparação do conjunto de dados 2017 a 2022 ----

# TABELA 14: Tratamento dos dados e criação das categorias Assalariados, Autonomos e outros ----
#' *caso não tenha disponível o inflator nos microdados, utilizar o método abaixo com o código do INPC*

# ped_arq_tab_14a<-ped %>% 
#   # dplyr::rename(SIT=sit) %>%
#   filter(AAMM<=202212 & SIT==4 & F210 !=12 & F220 !=3 & F220 !=6) %>% 
#   mutate(         POS1=case_when(POS %in% c(1,2,3,4)~1,
#                                  POS %in% c(5,6)~5,
#                                  TRUE~as.numeric(POS)),
#                   POS1=case_when(!(POS1 %in% c(1,5))~10,
#                                  TRUE~POS1),
#                   rendacz=case_when(F470==1000000000 & AAMM>=201701 ~ F481, F470==10000001 & AAMM<201701 ~ -10, TRUE~F470),
#                   rendacz=case_when(rendacz>9999999 ~ -1,TRUE~as.numeric(rendacz)),
#                   renda=case_when(((POS1==1|POS==8)&(rendacz==0))~ -10,
#                                   TRUE~rendacz),
#                   renda=case_when((renda==-1|renda==-10)~as.numeric(NA),
#                                   TRUE~renda),
#                   ano=substr(AAMM,start = 1,stop = 4),
#                   AAMM1=as.yearmon(as.character(AAMM), "%Y%m")-1/12)%>%
#   left_join(INPC,by="AAMM1")%>%
#   mutate(rrenda=(renda),
#          renda_at=(rrenda*inflator),
#          FATOR1=FATOR/12,
#          POS_PRIV_PUB= case_when(POS==1|POS==2~1,
#                                  POS==3~2, 
#                                  TRUE~3))

### Testando o inflator -----
ped_arq_tab_14a<-ped %>% 
  # dplyr::rename(SIT=sit) %>%
  filter(AAMM<=202212 & SIT==4 & F210 !=12 & F220 !=3 & F220 !=6) %>% 
  mutate(         POS1=case_when(POS %in% c(1,2,3,4)~1,
                                 POS %in% c(5,6)~5,
                                 TRUE~as.numeric(POS)),
                  POS1=case_when(!(POS1 %in% c(1,5))~10,
                                 TRUE~POS1),
                  #q421=case_when(q421>9999999 ~ -1000,TRUE ~ q421),
                  fi = 368.57, # verificar de onde vem esse valor
                  inflator=inflatordf,
                  rendacz=0,
                  # rendacz=case_when(F470<=9999999~trunc((F470/inflator)*fi)),
                  rendacz=case_when(F470==1000000000 & AAMM>=201701 ~ trunc((F481/inflator)*fi), F470==10000001 & AAMM<201701 ~ -10, TRUE~trunc((F470/inflator)*fi)),
                  rendacz=case_when(rendacz>9999999 ~ -1,TRUE~as.numeric(rendacz)),
                  renda=case_when(((POS1==1|POS==8)&(rendacz==0))~ -10,
                                  TRUE~rendacz),
                  renda=case_when((renda==-1|renda==-10)~as.numeric(NA),
                                  TRUE~renda),
                  ano=substr(AAMM,start = 1,stop = 4),
                  AAMM1=as.yearmon(as.character(AAMM), "%Y%m")-1/12,
                  rrenda=(renda),
                  renda_at=(rrenda),
                  FATOR1=FATOR/12,
                  POS_PRIV_PUB= case_when(POS==1|POS==2~1, POS==3~2, TRUE~3))



# q421=case_when(q421>9999999 ~ -1000,TRUE ~ q421),
# fi = 368.57, # verificar de onde vem esse valor
# inflator=inflatordf,
# rendacz=0,
# rendacz=case_when(q421<=9999999~trunc((q421/inflator)*fi))

# Seleção de variávies utilizadas na função "rend_real_med" para estatísticas anuais (2017-2022) ----
# Obs.: Nesse código cosidera-se apenas os dados anuais ----

ped_arq_tab_14a_select<-ped_arq_tab_14a %>% 
  dplyr::select(c( "renda_at", "AAMM", "AAMM1","ano","FATOR1", "inflator","SIT","POS", "POS1", "POS_PRIV_PUB", "SETOR_CNAE"))

# Concatenação das bases 2016 e 2017-2022 ----

arq_2015_2022<-rbind(arq_15_16_select,
                     ped_arq_tab_14a_select
)

#table(ped_2016_2022$ano,ped_2016_2022$F280)

# Filtro total de ocupados ----

ped_arq_tab_14a_2015_2022<-arq_2015_2022 %>% 
  filter(SIT==4)

# Filtros POS ----

ped_arq_tab_18_2015_2022<-ped_arq_tab_14a_2015_2022 %>% 
  filter(POS==1|POS==2|POS==3) 

# TABELA 18 - Parte 2: Base somente com assalariados privados. Uso da var Setor_Cnae  ----

ped_arq_tab_18a_2015_2022<-ped_arq_tab_14a_2015_2022 %>% 
  filter(POS==1|POS==2)


# Processamento dados anuais para a tabela 14 ----
rend_ocupados<-rend_real_med(arquivo=ped_arq_tab_14a_2015_2022,var="POS1",categorias=c( "Ano/mês","Ocupados","Assalariados","Autonomos","Outros"),periodo = "anual")
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
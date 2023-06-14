####  CRIANDO OS DEFLATORES ####

library(dplyr)
library(readxl)
library(stringr)

# baixar o arquivo do inpc do df de dezembro #

serie_inpc <- read_excel("serie_inpcDF.xlsx")

#filtrando para manter apenas obs de 2010 a 2021

serie_inpc = serie_inpc %>%
  filter(mes=='12', ano %in% c(2010:2021))

#criando indice

serie_inpc$indice <- 100   


for (i in 1:11) {
  serie_inpc[i+1,"indice"] <- (serie_inpc[i+1,"inpc"]/100+1)*
    serie_inpc[i,"indice"]
}

#criando a coluna de deflator 

serie_inpc$precos_dez_2020 <- c(NA,rep(1,11))

# calculo do indice com o valor da inflação de cada período

for (i in 0:10) {
  serie_inpc[12-i,"precos_dez_2020"] <- serie_inpc[12,"indice"]/
    serie_inpc[12-i-1,"indice"]
}

#separando o serie_inpc para cada ano

inpc11_20 = filter(serie_inpc, ano=='2011')
inpc12_20 = filter(serie_inpc, ano=='2012')
inpc13_20 = filter(serie_inpc, ano=='2013')
inpc14_20 = filter(serie_inpc, ano=='2014')
inpc15_20 = filter(serie_inpc, ano=='2015')
inpc16_20 = filter(serie_inpc, ano=='2016')
inpc17_20 = filter(serie_inpc, ano=='2017')
inpc18_20 = filter(serie_inpc, ano=='2018')
inpc19_20 = filter(serie_inpc, ano=='2019')
inpc20_20 = filter(serie_inpc, ano=='2020')
inpc21_20 = filter(serie_inpc, ano=='2021')

#criando deflatores para cada ano tendo como base os preços de dez 2020

deflator11_20 <- inpc11_20$precos_dez_2020
deflator12_20 <- inpc12_20$precos_dez_2020
deflator13_20 <- inpc13_20$precos_dez_2020
deflator14_20 <- inpc14_20$precos_dez_2020
deflator15_20 <- inpc15_20$precos_dez_2020
deflator16_20 <- inpc16_20$precos_dez_2020
deflator17_20 <- inpc17_20$precos_dez_2020
deflator18_20 <- inpc18_20$precos_dez_2020
deflator19_20 <- inpc19_20$precos_dez_2020
deflator20_20 <- inpc20_20$precos_dez_2020
deflator21_20 <- inpc21_20$precos_dez_2020

##############################################################################

#### CARREGANDO A BASE DA RAIS PRO DF  ####


# Conexão ao banco de dados com o pacote DBI
db <- DBI::dbConnect(odbc::odbc(), "db_codeplan",uid=('05524213164'),pwd=('G6SDB6fVDC'))



##trazer a base pro DF

rais_2011 <- DBI::dbGetQuery(db,"SELECT 
                             referencia,
                             vinculoativo3112,
                             tipovinculo,
                             escolaridade,
                             sexotrabalhador,
                             racacor,
                             vlremundezembronom,
                             vlremundezembrosm,
                             tiposalario,
                             tempoemprego,
                             qtdhoracontr,
                             vlsalariocontratual,
                             cboocupacao2002,
                             cnae20subclasse,
                             idade,
                             indtrabintermitente  FROM rais_id.Vinc WHERE municipio = 530010 and referencia > 2010 and vinculoativo3112 = 1")



## dividindo rais_df em rais por ano 

rais_df_2011 = filter(rais_2011, referencia=='2011')
rais_2012 = filter(rais_2011, referencia=='2012')
rais_2013 = filter(rais_2011, referencia=='2013')
rais_2014 = filter(rais_2011, referencia=='2014')
rais_2015 = filter(rais_2011, referencia=='2015')
rais_2016 = filter(rais_2011, referencia=='2016')
rais_2017 = filter(rais_2011, referencia=='2017')
rais_2018 = filter(rais_2011, referencia=='2018')
rais_2019 = filter(rais_2011, referencia=='2019')
rais_2020 = filter(rais2020, referencia=='2020')


## multiplicando o deflator correspondente criando uma nova var na base 

rais_df_2011 = rais_df_2011 %>%
  transmute(rais_df_2011,
            salario_dez_defl = lapply(vlremundezembronom, "*", deflator11_20))

rais_2012 = rais_2012 %>%
  transmute(rais_2012,
            salario_dez_defl = lapply(vlremundezembronom, "*", deflator12_20))

rais_2013 = rais_2013 %>%
  transmute(rais_2013,
            salario_dez_defl = lapply(vlremundezembronom, "*", deflator13_20))


rais_2014 = rais_2014 %>%
  transmute(rais_2014,
            salario_dez_defl = lapply(vlremundezembronom, "*", deflator14_20))

rais_2015 = rais_2015 %>%
  transmute(rais_2015,
            salario_dez_defl = lapply(vlremundezembronom, "*", deflator15_20))

rais_2016 = rais_2016 %>%
  transmute(rais_2016,
            salario_dez_defl = lapply(vlremundezembronom, "*", deflator16_20))

rais_2017 = rais_2017 %>%
  transmute(rais_2017,
            salario_dez_defl = lapply(vlremundezembronom, "*", deflator17_20))

rais_2018 = rais_2018 %>%
  transmute(rais_2018,
            salario_dez_defl = lapply(vlremundezembronom, "*", deflator18_20))

rais_2019 = rais_2019 %>%
  transmute(rais_2019,
            salario_dez_defl = lapply(vlremundezembronom, "*", deflator19_20))

rais_2020 = rais_2020 %>%
  transmute(rais_2020,
            salario_dez_defl = lapply(vlremundezembronom, "*", deflator20_20))

## juntar todas as rais em único arquivo ###

rais_df_tecnicos = rbind(rais_df_2011, rais_2012, rais_2013, rais_2014, rais_2015,
                         rais_2016, rais_2017, rais_2018, rais_2019, rais_2020)


## ajustar coluna de salário real ##

rais_df_tecnicos$salario_dez_defl = as.numeric(rais_df_tecnicos$salario_dez_defl)

round(rais_df_tecnicos$salario_dez_defl, digits = 2)

##### criando coluna de salário hora #####

rais_df_tecnicos = rais_df_tecnicos %>% mutate(horas_mensais = qtdhoracontr*4) #criando variável qnt de horas mensais trabalhadas

rais_df_tecnicos = rais_df_tecnicos %>% mutate(salario_hora = salario_dez_defl/horas_mensais) #criando variável de salário hora

################################################################################

rm(serie_inpc,inpc11_20,inpc12_20,inpc13_20,inpc14_20,inpc15_20,inpc16_20,inpc17_20,inpc18_20,inpc19_20,inpc20_20,inpc21_20,rais_df)
rm(rais_df_2011,rais_2012,rais_2013,rais_2014,rais_2015,rais_2016,rais_2017,rais_2018,rais_2019,rais_2020)

### CRIANDO DUMMIES E FILTROS ###

### filtrando vínculos que possuem SM>=0.5 e <=200 ###

rais_df_tecnicos = rais_df_tecnicos %>%
  filter(vlremundezembrosm>=0.5 & vlremundezembrosm <=200)


### criando as dummies ####

cbotecnica_nivelmedio = read.csv("cbotecnica_nivelmedio.csv", header = TRUE)
cbotecnica_nivelsuperior = read.csv("cbotecnica_nivelsuperior.csv", header = TRUE)
ocupacoes_protegidas = read.csv("ocupacoes_protegidas.csv", header = TRUE)


### criando dummy sem os filtros ### ----

### nivel médio técnico ###
rais_df_tecnicos$cbo_tec_em = ifelse(rais_df_tecnicos$cboocupacao2002 %in% cbotecnica_nivelmedio$cbo_em, 1,0)

### nível médio superior ###
rais_df_tecnicos$cbo_tec_es = ifelse(rais_df_tecnicos$cboocupacao2002 %in% cbotecnica_nivelsuperior$cbo_superior, 1,0)


## criando dummy de vínculos em CBOs técnicas com os filtros

rais_df_tecnicos = rais_df_tecnicos %>%
  transmute(rais_df_tecnicos,
            cbo_tec_em_complet = ifelse(cbo_tec_em==1 & escolaridade %in% c(7:11) & tipovinculo!='55', 1,0),
            cbo_tec_sup = ifelse(cbo_tec_es==1 & escolaridade %in% c(9:11) & tipovinculo!='55', 1,0))

### dummy para tec médio e superior

rais_df_tecnicos = rais_df_tecnicos %>%
  transmute(rais_df_tecnicos, cbo_tec = ifelse(cbo_tec_em_complet==1 | cbo_tec_sup==1, 1,0))

### retirando da base as colunas criadas de cbo técnica que não serão utilizadas ###

rais_df_tecnicos = rais_df_tecnicos %>% mutate(cbo_tec_em = NULL)

rais_df_tecnicos = rais_df_tecnicos %>% mutate(cbo_tec_es = NULL)


### criando dummies para tipo de vinculo ###

rais_df_tecnicos = rais_df_tecnicos %>%
  transmute(rais_df_tecnicos,
            celetistas = ifelse(tipovinculo %in% c(10,15,20,25,60,65,70,75), 1,0),
            estatutarios = ifelse(tipovinculo %in% c(30,31,35), 1,0),
            aprendiz = ifelse(tipovinculo=='55', 1,0),
            outros = ifelse(tipovinculo %in% c(40,50,80,90,95,96,97,-1),1,0))

### criando dummies para grau de escolaridade ###

rais_df_tecnicos = rais_df_tecnicos %>%
  transmute(rais_df_tecnicos,
            medio_incompleto = ifelse(escolaridade %in% c(2,3,4,5,6), 1,0),
            medio_completo = ifelse(escolaridade %in% c(7,8), 1,0),
            superior_completo = ifelse(tipovinculo %in% c(9,10,11),1,0),
            analfabeto = ifelse(escolaridade=='1', 1,0))


### retirando da base os vínculos em ocupações protegidas (militares) ###

ocupacoes_protegidas$cbo_protegidas <-as.numeric(ocupacoes_protegidas$cbo_protegidas)

cbos_protegidas = formatC(ocupacoes_protegidas$cbo_protegidas, width=6, format = "d", flag="0")

rais_df_tecnicos = rais_df_tecnicos[!rais_df_tecnicos$cboocupacao2002%in%cbos_protegidas,]

## EIXOS TECNOLÓGICOS PARA CBOs NÍVEL MÉDIO ###

Eixo1 <- read_delim("Eixo1.csv", delim = ";", 
                    escape_double = FALSE, trim_ws = TRUE)

Eixo2 <- read_delim("Eixo2.csv", delim = ";", 
                    escape_double = FALSE, trim_ws = TRUE)

Eixo3 <- read_delim("Eixo3.csv", delim = ";", 
                    escape_double = FALSE, trim_ws = TRUE)

Eixo4 <- read_delim("Eixo4.csv", delim = ";", 
                    escape_double = FALSE, trim_ws = TRUE)

Eixo5 <- read_delim("Eixo5.csv", delim = ";", 
                    escape_double = FALSE, trim_ws = TRUE)

Eixo6 <- read_delim("Eixo6.csv", delim = ";", 
                    escape_double = FALSE, trim_ws = TRUE)

Eixo7 <- read_delim("Eixo7.csv", delim = ";", 
                    escape_double = FALSE, trim_ws = TRUE)

Eixo8 <- read_delim("Eixo8.csv", delim = ";", 
                    escape_double = FALSE, trim_ws = TRUE)

Eixo9 <- read_delim("Eixo9.csv", delim = ";", 
                    escape_double = FALSE, trim_ws = TRUE)

Eixo10 <- read_delim("Eixo10.csv", delim = ";", 
                     escape_double = FALSE, trim_ws = TRUE)

Eixo11 <- read_delim("Eixo11.csv", delim = ";", 
                     escape_double = FALSE, trim_ws = TRUE)


Eixo12 <- read_delim("Eixo12.csv", delim = ";", 
                     escape_double = FALSE, trim_ws = TRUE)

Eixo13 <- read_delim("Eixo13.csv", delim = ";", 
                     escape_double = FALSE, trim_ws = TRUE)


### criando dummies para cada eixo com filtro

rais_df_tecnicos = rais_df_tecnicos %>%
  transmute(rais_df_tecnicos,
            eixo_ambesaude_em = ifelse(rais_df_tecnicos$cboocupacao2002 %in% Eixo1$`Eixo de Ambiente e Saúde` & cbo_tec_em_complet==1, 1,0),
            eixo_conteprocessos_em = ifelse(rais_df_tecnicos$cboocupacao2002 %in% Eixo2$`Eixo de Controle e Processos Industriais` & cbo_tec_em_complet==1, 1, 0),
            eixo_desedusoc_em = ifelse(rais_df_tecnicos$cboocupacao2002 %in% Eixo3$`Eixo de Desenvolvimento Educacional e Social` & cbo_tec_em_complet==1, 1,0),
            eixo_negocios_em = ifelse(rais_df_tecnicos$cboocupacao2002 %in% Eixo4$`Eixo de Gestão e Negócios` & cbo_tec_em_complet==1,1 ,0),
            eixo_infoecomunic_em = ifelse(rais_df_tecnicos$cboocupacao2002 %in% Eixo5$`Eixo de Informação e Comunicação` & cbo_tec_em_complet==1,1,0),
            eixo_infraestrutura_em = ifelse(rais_df_tecnicos$cboocupacao2002 %in% Eixo6$`Eixo de Infraestrutura` & cbo_tec_em_complet==1,1,0),
            eixo_prodaliment_em = ifelse(rais_df_tecnicos$cboocupacao2002 %in% Eixo7$`Eixo de Produção Alimentícia` & cbo_tec_em_complet==1,1,0),
            eixo_prodcult_em = ifelse(rais_df_tecnicos$cboocupacao2002 %in% Eixo8$`Eixo de Produção Cultural e Design` & cbo_tec_em_complet==1,1,0),
            eixo_prodindust_em = ifelse(rais_df_tecnicos$cboocupacao2002 %in% Eixo9$`Eixo de Produção Industrial` & cbo_tec_em_complet==1,1,0),
            eixo_recnaturais_em = ifelse(rais_df_tecnicos$cboocupacao2002 %in% Eixo10$`Eixo de Recursos Naturais` & cbo_tec_em_complet==1,1,0),
            eixo_seguranca_em = ifelse(rais_df_tecnicos$cboocupacao2002 %in% Eixo11$`Eixo de Segurança` & cbo_tec_em_complet==1,1,0),
            eixo_hospelazer_em = ifelse(rais_df_tecnicos$cboocupacao2002 %in% Eixo12$`Eixo de Turismo, Hospitalidade e Lazer` & cbo_tec_em_complet==1,1,0),
            eixo_militar_em = ifelse(rais_df_tecnicos$cboocupacao2002 %in% Eixo13$`Eixo Militar` & cbo_tec_em_complet==1,1,0))

## EIXOS TECNOLÓGICOS PARA CBOs NÍVEL SUPERIOR ###

Eixo1_sup <- read_delim("Eixo1_sup.csv", delim = ";", 
                        escape_double = FALSE, trim_ws = TRUE)

Eixo2_sup <- read_delim("Eixo2_sup.csv", delim = ";", 
                        escape_double = FALSE, trim_ws = TRUE)

Eixo4_sup <- read_delim("Eixo4_sup.csv", delim = ";", 
                        escape_double = FALSE, trim_ws = TRUE)

Eixo5_sup <- read_delim("Eixo5_sup.csv", delim = ";", 
                        escape_double = FALSE, trim_ws = TRUE)

Eixo6_sup <- read_delim("Eixo6_sup.csv", delim = ";", 
                        escape_double = FALSE, trim_ws = TRUE)

Eixo7_sup <- read_delim("Eixo7_sup.csv", delim = ";", 
                        escape_double = FALSE, trim_ws = TRUE)

Eixo8_sup <- read_delim("Eixo8_sup.csv", delim = ";", 
                        escape_double = FALSE, trim_ws = TRUE)

Eixo9_sup <- read_delim("Eixo9_sup.csv", delim = ";", 
                        escape_double = FALSE, trim_ws = TRUE)

Eixo10_sup <- read_delim("Eixo10_sup.csv", delim = ";", 
                         escape_double = FALSE, trim_ws = TRUE)

Eixo11_sup <- read_delim("Eixo11_sup.csv", delim = ";", 
                         escape_double = FALSE, trim_ws = TRUE)

Eixo12_sup <- read_delim("Eixo12_sup.csv", delim = ";", 
                         escape_double = FALSE, trim_ws = TRUE)

Eixo13_sup <- read_delim("Eixo13_sup.csv", delim = ";", 
                         escape_double = FALSE, trim_ws = TRUE)


### criando dummies para cada eixo com filtro

rais_df_tecnicos = rais_df_tecnicos %>%
  transmute(rais_df_tecnicos,
            eixo_ambesaude_sup = ifelse(rais_df_tecnicos$cboocupacao2002 %in% Eixo1_sup$`Ambiente_saude` & cbo_tec_sup==1, 1,0),
            eixo_conteprocessos_sup = ifelse(rais_df_tecnicos$cboocupacao2002 %in% Eixo2_sup$`Controle e Processos Industriais` & cbo_tec_sup==1, 1, 0),
            eixo_negocios_sup = ifelse(rais_df_tecnicos$cboocupacao2002 %in% Eixo4_sup$`gestao_neg` & cbo_tec_sup==1,1 ,0),
            eixo_infoecomunic_sup = ifelse(rais_df_tecnicos$cboocupacao2002 %in% Eixo5_sup$`info_comu` & cbo_tec_sup==1,1,0),
            eixo_infraestrutura_sup = ifelse(rais_df_tecnicos$cboocupacao2002 %in% Eixo6_sup$`Infraestrutura` & cbo_tec_sup==1,1,0),
            eixo_prodaliment_sup = ifelse(rais_df_tecnicos$cboocupacao2002 %in% Eixo7_sup$`prod_alim` & cbo_tec_sup==1,1,0),
            eixo_prodcult_sup = ifelse(rais_df_tecnicos$cboocupacao2002 %in% Eixo8_sup$`prod_cul_design` & cbo_tec_sup==1,1,0),
            eixo_prodindust_sup = ifelse(rais_df_tecnicos$cboocupacao2002 %in% Eixo9_sup$`prod_ind` & cbo_tec_sup==1,1,0),
            eixo_recnaturais_sup = ifelse(rais_df_tecnicos$cboocupacao2002 %in% Eixo10_sup$`Recursos Naturais` & cbo_tec_sup==1,1,0),
            eixo_seguranca_sup = ifelse(rais_df_tecnicos$cboocupacao2002 %in% Eixo11_sup$`seguranca` & cbo_tec_sup==1,1,0),
            eixo_hospelazer_sup = ifelse(rais_df_tecnicos$cboocupacao2002 %in% Eixo12_sup$`Tur_hosp_lazer` & cbo_tec_sup==1,1,0),
            eixo_militar_sup = ifelse(rais_df_tecnicos$cboocupacao2002 %in% Eixo13_sup$`Eixo Militar` & cbo_tec_sup==1,1,0))

rm (Eixo1, Eixo2, Eixo3, Eixo4, Eixo5, Eixo6, Eixo7, Eixo8, Eixo9, Eixo10, Eixo11, Eixo12, Eixo13)
rm (Eixo1_sup, Eixo2_sup, Eixo4_sup, Eixo5_sup, Eixo6_sup, Eixo7_sup, Eixo8_sup, Eixo9_sup, Eixo10_sup, Eixo11_sup, Eixo12_sup, Eixo13_sup)
rm(ocupacoes_protegidas,cbotecnica_nivelmedio,cbotecnica_nivelsuperior)

rais_df_tecnicos$cbo_tec_em_complet <-as.factor(rais_df_tecnicos$cbo_tec_em_complet)

rais_df_tecnicos$cbo_tec_sup <-as.factor(rais_df_tecnicos$cbo_tec_sup)
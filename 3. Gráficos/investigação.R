library(tidyverse)# Manipulação dos dados
library(imputeTS) # Substituir NA's
library(readxl)   # Leitura de Arquivos em .xlsx
library(stringr)  # Manipulação de dados em formato de caracter
library(data.table)

`%notin%` <- Negate(`%in%`)           # Função de filtro
options(readr.show_col_types = FALSE)

estrutura_cbo <- readRDS("Dados/Dicionário CBO.RDS")
estrutura_cnae <- readRDS("Dados/Dicionário CNAE.RDS")

db <- DBI::dbConnect(odbc::odbc(),
                     "db_codeplan", 
                     uid = Sys.getenv("matricula"),
                     pwd = Sys.getenv("senha")) 

dados <- DBI::dbGetQuery(db,
"SELECT
referencia,
cbo2002 as cboocupacao2002, cnae20subclasse,
vlremdeznm as vlremundezembronom,
vlremdezsm as vlremundezembrosm,
vl_salario_contrato as vlsalariocontratual,
tipovinculo
FROM rais_id.vinc_2016
WHERE municipio = 530010 
and vinculoativo3112 = 1
and idade >= 18
and vlremdezsm > 10and  cnae20subclasse between 8600000 and 8699999
union all
SELECT
referencia,
cbo2002 as cboocupacao2002, cnae20subclasse,
vlremdeznm as vlremundezembronom,
vlremdezsm as vlremundezembrosm,
vl_salario_contrato as vlsalariocontratual,
tipovinculo
FROM rais_id.vinc_2017
WHERE municipio = 530010 
and vinculoativo3112 = 1
and idade >= 18
and  cnae20subclasse between 8600000 and 8699999
union all
SELECT
referencia,
cbo2002 as cboocupacao2002, cnae20subclasse,
vlremdeznm as vlremundezembronom,
vlremdezsm as vlremundezembrosm,
vl_salario_contrato as vlsalariocontratual,
tipovinculo
FROM rais_id.vinc_2018
WHERE municipio = 530010 
and vinculoativo3112 = 1
and idade >= 18
and  cnae20subclasse between 8600000 and 8699999
union all
SELECT
referencia,
cbo2002 as cboocupacao2002, cnae20subclasse,
vlremdeznm as vlremundezembronom,
vlremdezsm as vlremundezembrosm,
vl_salario_contrato as vlsalariocontratual,
tipovinculo
FROM rais_id.vinc_2019
WHERE municipio = 530010 
and vinculoativo3112 = 1
and idade >= 18
and  cnae20subclasse between 8600000 and 8699999
union all
SELECT
referencia,
cbo2002 as cboocupacao2002, cnae20subclasse,
vlremdeznm as vlremundezembronom,
vlremdezsm as vlremundezembrosm,
vl_salario_contrato as vlsalariocontratual,
tipovinculo
FROM rais_id.vinc_2020
WHERE municipio = 530010 
and vinculoativo3112 = 1
and idade >= 18
and  cnae20subclasse between 8600000 and 8699999
union all
SELECT
referencia,
cbo2002 as cboocupacao2002, cnae20subclasse,
vlremdeznm as vlremundezembronom,
vlremdezsm as vlremundezembrosm,
vl_salario_contrato as vlsalariocontratual,
tipovinculo
FROM rais_id.vinc_2021
WHERE municipio = 530010 
and vinculoativo3112 = 1
and idade >= 18
and  cnae20subclasse between 8600000 and 8699999")


dados <- dados |>
   mutate(tipovinculo = case_when(tipovinculo %in% c(10, 15, 20, 25, 60, 65, 70, 75) ~ 1,  # Celetista - 1
                                  tipovinculo %in% c(30, 31, 35) ~ 2,                      # Estatutário - 2
                                  tipovinculo == 55 ~ 3,                                   # Aprendiz - 3
                                  tipovinculo %in% c(40, 50, 80, 90, 95, 96, 97, -1) ~ 4))

dados |>filter(vlremundezembronom==0) |>  group_by(referencia,tipovinculo) |> summarise(n=n()) |> ggplot(aes(x = referencia,y = n,col = factor(tipovinculo)))+geom_line()+geom_point()+theme_bw()+ggtitle("Vínculos com salário igual a zero para CNAE Divisão 86")

dados |>
   mutate(cnae20subclasse = as.character(cnae20subclasse)) |> 
   left_join(estrutura_cbo,by = "cboocupacao2002") |> 
   left_join(estrutura_cnae,by = "cnae20subclasse") |> 
   filter(vlremundezembronom==0) |>  group_by(referencia,tipovinculo,nome_cbo_ocupacao) |> summarise(n=n()) |> view()



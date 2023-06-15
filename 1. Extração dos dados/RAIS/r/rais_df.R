# Carregar pacotes ----

library(odbc)
library(tidyverse)
library(sidrar)
library(readxl)

options(readr.show_col_types = FALSE)

# Importação dos dados ----

ano <- 2021

## CBOs técnicas ----

cbotecnica_nivel_medio <- read_csv("Dados/cbotecnica_nivelmedio.csv")
cbotecnica_nivel_superior <- read_csv("Dados/cbotecnica_nivelsuperior.csv")
ocupacoes_protegidas <- read_csv("Dados/ocupacoes_protegidas.csv")

## Eixos ----

eixos <- lapply(paste0("Dados/Eixos - nível médio/", dir("Dados/Eixos - nível médio")[str_detect(dir("Dados/Eixos - nível médio"), "Eixo[0-9]{1,2}\\.")]), function(x) {read_delim(x, delim = ";", locale = locale(encoding = "UTF-8"))})
eixos_superior <- lapply(paste0("Dados/Eixos - nível superior/", dir("Dados/Eixos - nível superior")[str_detect(dir("Dados/Eixos - nível superior"), "Eixo[0-9]{1,2}\\_")]), function(x) {read_delim(x, delim = ";", locale = locale(encoding = "ISO-8859-1"))})

names(eixos) <- vapply(eixos, function(x){names(x)[[2]]}, "eixo")
names(eixos_superior) <- vapply(eixos_superior, function(x){names(x)[[2]]}, "eixo")

## INPC ----

inpc <- readRDS("Dados/INPC.RDS") |>
  rbind(get_sidra(api = "/t/7063/n6/5300108/v/44/p/all/c315/7169/d/v44%202") |>
          select(data = "Mês (Código)",
                 inpc = "Valor") |>
          mutate(data = ym(data))) |>
  mutate(ano_ref = year(data)) |>
  filter(ano_ref > 2009 & ano_ref < 2022) |>
  group_by(ano_ref) |>
  summarise(agreg = prod(1 + inpc)) |>
  mutate(ipca_anual = rev(cumprod(rev(agreg))))

#'*João*
inpc <- inpc |> rename(referencia = ano_ref)

## Conexão ao SQL ----

db <- DBI::dbConnect(odbc(),
                     "db_codeplan", 
                     uid = Sys.getenv("matricula"), 
                     pwd = Sys.getenv("senha"))

# === TROCAR ANO QUANDO FOR ATUALIZAR ===

rais <- NULL

for (i in 2011:ano) {
  
  temp <- DBI::dbGetQuery(db, paste0("SELECT referencia, vinculoativo3112, tipovinculo, escolaridade_2006_atual as escolaridade, sexotrabalhador, racacor, vlremdeznm as vlremundezembronom, vlremdezsm as vlremundezembrosm, tiposal as tiposalario, tempoemprego, qtdhoracontr, vl_salario_contrato as vlsalariocontratual, cbo2002 as cboocupacao2002, cnae20subclasse, idade, null as indtrabintermitente  FROM DB_CODEPLAN.rais_id.vinc_", i, " WHERE municipio = 530010 and vinculoativo3112 = 1"))
  rais <- rbind(rais, temp)
  
}

#'*João*
dados <- left_join(rais, inpc, by = "referencia")



### Une as rais
dados = rbind(dados, rais2020)

# Limpa base ----
## Une base com inpc ----
dados = merge(dados, serie_inpc, all.x = TRUE)

## Filtra base ----
dados = dados[dados$vlremundezembrosm >= 0.5 & dados$vlremundezembrosm <= 200,]

## Calcula novas variaveis ----
dados$salario_dez_defl = dados$vlremundezembronom * dados$inpc_acum
dados$horas_mensais = qtdhoracontr*4
dados$salario_hora = salario_dez_defl/horas_mensais

## Dummys cbos
dados$cbo_tec_em_complet = ifelse(dados$cboocupacao2002 %in% cbotecnica_nivel_medio$cbo_em & dados$escolaridade %in% c(7:11) & dados$tipovinculo!="55", 1,0)
dados$cbo_tec_sup = ifelse(dados$cboocupacao2002 %in% cbotecnica_nivel_superior$cbo_superior & dados$escolaridade %in% c(9:11) & dados$tipovinculo!="55", 1,0)
dados$cbo_tec = ifelse(dados$dados$cbo_tec_em_complet == 1 | dados$dados$cbo_tec_sup == 1, 1,0)

### Dummy vinculo

dados$celetistas = ifelse(dados$tipovinculo %in% c(10,15,20,25,60,65,70,75), 1,0)
dados$estatutarios = ifelse(dados$tipovinculo %in% c(30,31,35), 1,0)
dados$aprendiz = ifelse(dados$tipovinculo=="55", 1,0)
dados$outros = ifelse(dados$tipovinculo %in% c(40,50,80,90,95,96,97,-1), 1, 0)

### Dummy escolaridade

dados$medio_incompleto = ifelse(dados$escolaridade %in% c(2,3,4,5,6), 1,0)
dados$medio_completo = ifelse(dados$escolaridade %in% c(7,8), 1,0)
dados$superior_completo = ifelse(dados$escolaridade %in% c(9,10,11), 1, 0)
dados$analfabeto = ifelse(dados$escolaridade=="1", 1,0)

### retirando da base os vínculos em ocupações protegidas (militares) ###

cbos_protegidas = ocupacoes_protegidas$cbo_protegidas[!is.na(ocupacoes_protegidas$cbo_protegidas)]

cbos_protegidas = formatC(cbos_protegidas, width=6, format = "d", flag="0")

dados = dados[!dados$cboocupacao2002 %in% cbos_protegidas,]

### Dummy eixo ----
dados$eixo_amb_saude_em = ifelse(dados$cboocupacao2002 %in% eixos$`Eixo de Ambiente e Saúde`$`Eixo de Ambiente e Saúde` & dados$cbo_tec_em_complet == 1, 1,0)

dados$eixo_conteprocessos_em = ifelse(dados$cboocupacao2002 %in% eixos$`Eixo de Controle e Processos Industriais`$`Eixo de Controle e Processos Industriais` & dados$cbo_tec_em_complet == 1, 1, 0)

dados$eixo_desedusoc_em = ifelse(dados$cboocupacao2002 %in% eixos$`Eixo de Desenvolvimento Educacional e Social`$`Eixo de Desenvolvimento Educacional e Social` & dados$cbo_tec_em_complet == 1, 1,0)

dados$eixo_negocios_em = ifelse(dados$cboocupacao2002 %in% eixos$`Eixo de Gestão e Negócios`$`Eixo de Gestão e Negócios` & dados$cbo_tec_em_complet == 1,1 ,0)

dados$eixo_infoecomunic_em = ifelse(dados$cboocupacao2002 %in% eixos$`Eixo de Informação e Comunicação`$`Eixo de Informação e Comunicação` & dados$cbo_tec_em_complet == 1, 1, 0)

dados$eixo_infraestrutura_em = ifelse(dados$cboocupacao2002 %in% eixos$`Eixo de Infraestrutura`$`Eixo de Infraestrutura` & dados$cbo_tec_em_complet == 1, 1, 0)

dados$eixo_prodaliment_em = ifelse(dados$cboocupacao2002 %in% eixos$`Eixo de Produção Alimentícia`$`Eixo de Produção Alimentícia` & dados$cbo_tec_em_complet == 1, 1, 0)

dados$eixo_prodcult_em = ifelse(dados$cboocupacao2002 %in% eixos$`Eixo de Produção Cultural e Design`$`Eixo de Produção Cultural e Design` & dados$cbo_tec_em_complet == 1, 1, 0)

dados$eixo_prodindust_em = ifelse(dados$cboocupacao2002 %in% eixos$`Eixo de Produção Industrial`$`Eixo de Produção Industrial` & dados$cbo_tec_em_complet == 1, 1, 0)

dados$eixo_recnaturais_em = ifelse(dados$cboocupacao2002 %in% eixos$`Eixo de Recursos Naturais`$`Eixo de Recursos Naturais` & dados$cbo_tec_em_complet == 1, 1, 0)

eixo_seguranca_em = ifelse(dados$cboocupacao2002 %in% eixos$`Eixo de Segurança`$`Eixo de Segurança` & dados$cbo_tec_em_complet == 1, 1, 0)

eixo_hospelazer_em = ifelse(dados$cboocupacao2002 %in% eixos$`Eixo de Turismo, Hospitalidade e Lazer`$`Eixo de Turismo, Hospitalidade e Lazer` & dados$cbo_tec_em_complet == 1, 1, 0)

eixo_militar_em = ifelse(dados$cboocupacao2002 %in% eixos$`Eixo Militar`$`Eixo Militar` & dados$cbo_tec_em_complet == 1, 1, 0)

### Dummy eixo superior ----
dados$eixo_ambesaude_sup = ifelse(dados$cboocupacao2002 %in% eixos_superior$`Ambiente_saude`$`Ambiente_saude` & dados$cbo_tec_sup == 1, 1,0)

dados$eixo_conteprocessos_sup = ifelse(dados$cboocupacao2002 %in% eixos_superior$`Controle e Processos Industriais`$`Controle e Processos Industriais` & dados$cbo_tec_sup == 1, 1, 0)

dados$eixo_negocios_sup = ifelse(dados$cboocupacao2002 %in% eixos_superior$`gestao_neg`$`gestao_neg` & dados$cbo_tec_sup == 1,1 ,0)

dados$eixo_infoecomunic_sup = ifelse(dados$cboocupacao2002 %in% eixos_superior$`info_comu`$`info_comu` & dados$cbo_tec_sup == 1, 1, 0)

dados$eixo_infraestrutura_sup = ifelse(dados$cboocupacao2002 %in% eixos_superior$`Infraestrutura`$`Infraestrutura` & dados$cbo_tec_sup == 1, 1, 0)

dados$eixo_prodaliment_sup = ifelse(dados$cboocupacao2002 %in% eixos_superior$`prod_alim`$`prod_alim` & dados$cbo_tec_sup == 1, 1, 0)

dados$eixo_prodcult_sup = ifelse(dados$cboocupacao2002 %in% eixos_superior$`prod_cul_design`$`prod_cul_design` & dados$cbo_tec_sup == 1, 1, 0)

dados$eixo_prodindust_sup = ifelse(dados$cboocupacao2002 %in% eixos_superior$`prod_ind`$`prod_ind` & dados$cbo_tec_sup == 1, 1, 0)

dados$eixo_recnaturais_sup = ifelse(dados$cboocupacao2002 %in% eixos_superior$`Recursos Naturais`$`Recursos Naturais` & dados$cbo_tec_sup == 1, 1, 0)

dados$eixo_seguranca_sup = ifelse(dados$cboocupacao2002 %in% eixos_superior$`seguranca`$`seguranca` & dados$cbo_tec_sup == 1, 1, 0)

dados$eixo_hospelazer_sup = ifelse(dados$cboocupacao2002 %in% eixos_superior$`Tur_hosp_lazer`$`Tur_hosp_lazer` & dados$cbo_tec_sup == 1, 1, 0)

dados$eixo_militar_sup = ifelse(dados$cboocupacao2002 %in% eixos_superior$`Eixo Militar`$`Eixo Militar` & dados$cbo_tec_sup == 1, 1, 0)
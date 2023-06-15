# Carregar pacotes ----

library(odbc)
library(tidyverse)
library(sidrar)
library(readxl)

options(readr.show_col_types = FALSE)

# Importação dos dados ----

ano <- 2021

## CBOs técnicas ----

cbotecnica_nivel_medio <- read_csv("Dados/cbotecnica_nivelmedio.csv")[[1]]
cbotecnica_nivel_superior <- read_csv("Dados/cbotecnica_nivelsuperior.csv")[[1]]
cbos_protegidas <- read_csv("Dados/ocupacoes_protegidas.csv")[[1]]

## Eixos ----

eixos <- lapply(paste0("Dados/Eixos - nível médio/", dir("Dados/Eixos - nível médio")[str_detect(dir("Dados/Eixos - nível médio"), "Eixo[0-9]{1,2}\\.")]), function(x) {read_delim(x, delim = ";", locale = locale(encoding = "UTF-8"))})
eixos_superior <- lapply(paste0("Dados/Eixos - nível superior/", dir("Dados/Eixos - nível superior")[str_detect(dir("Dados/Eixos - nível superior"), "Eixo[0-9]{1,2}\\_")]), function(x) {read_delim(x, delim = ";", locale = locale(encoding = "ISO-8859-1"))})

names(eixos) <- vapply(eixos, function(x){names(x)[[2]]}, "eixo")
names(eixos_superior) <- vapply(eixos_superior, function(x){names(x)[[2]]}, "eixo")

## INPC ----

inpc <- get_sidra(api = "/t/2951/n6/5300108/v/44/p/all/c315/7169/d/v44%202") |>  # Até 2011
  rbind(get_sidra(api = "/t/1100/n6/5300108/v/44/p/all/c315/7169/d/v44%202")) |> # De 2012 até 2019
  rbind(get_sidra(api = "/t/7063/n6/5300108/v/44/p/all/c315/7169/d/v44%202")) |> # De 2020 em diante
  select(data = "Mês (Código)",
         inpc = "Valor") |>
  mutate(data = ym(data),
         inpc = inpc / 100,
         referencia = year(data)) |>
  filter(referencia > 2009) |>
  group_by(referencia) |>
  summarise(agreg = prod(1 + inpc)) |>
  mutate(inpc_anual = cumprod(agreg),
         inpc_anual = inpc_anual / nth(inpc_anual, -2))

## RAIS ----

db <- DBI::dbConnect(odbc(),
                     "db_codeplan", 
                     uid = Sys.getenv("matricula"), 
                     pwd = Sys.getenv("senha")) 

rais <- NULL

for (i in 2011:ano) {
  
  temp <- DBI::dbGetQuery(db, paste0("SELECT referencia, vinculoativo3112, tipovinculo, escolaridade_2006_atual as escolaridade, sexotrabalhador, racacor, vlremdeznm as vlremundezembronom, vlremdezsm as vlremundezembrosm, tiposal as tiposalario, tempoemprego, qtdhoracontr, vl_salario_contrato as vlsalariocontratual, cbo2002 as cboocupacao2002, cnae20subclasse, idade, null as indtrabintermitente  FROM DB_CODEPLAN.rais_id.vinc_", i, " WHERE municipio = 530010 and vinculoativo3112 = 1"))
  rais <- rbind(rais, temp)
  
}

remove(temp, i)

dados <- left_join(rais, inpc, by = "referencia")

# Tratamento dos dados ----

## Filtros da base ----

### Salários ----

dados <- dados |>
  filter(vlremundezembrosm >= 0.5 & vlremundezembrosm <= 200)

### Retirar ocupações protegidas ----

dados <- dados |>
  filter(cboocupacao2002 != cbos_protegidas)

## Cálculo de novas variáveis ----

dados <- dados |>
  mutate(salario_dez_defl = vlremundezembronom * inpc_anual,
         horas_mensais = qtdhoracontr * 4,
         salario_hora = salario_dez_defl / horas_mensais)

## Dummies ----

### CBOs ----

dados <- dados |>
  mutate(cbo_tec_em_complet = case_when(cboocupacao2002 %in% cbotecnica_nivel_medio & escolaridade %in% 7:11 & tipovinculo != 55 ~ 1, TRUE ~ 0),
         cbo_tec_sup = case_when(cboocupacao2002 %in% cbotecnica_nivel_superior & escolaridade %in% 9:11 & tipovinculo != 55 ~ 1, TRUE ~ 0),
         cbo_tec = case_when(cbo_tec_em_complet == 1 | cbo_tec_sup == 1 ~ 1, TRUE ~ 0))

### Tipo vínculo ----

dados <- dados |>
  mutate(tipovinculo = case_when(tipovinculo %in% c(10, 15, 20, 25, 60, 65, 70, 75) ~ 1,  # Celetista - 1
                                 tipovinculo %in% c(30, 31, 35) ~ 2,                      # Estatutário - 2
                                 tipovinculo == 55 ~ 3,                                   # Aprendiz - 3
                                 tipovinculo %in% c(40, 50, 80, 90, 95, 96, 97, -1) ~ 4)) # Outros - 4

### Escolaridade ----

dados <- dados |>
  mutate(escolaridade = case_when(escolaridade == 1 ~ 1,       # Analfabeto - 1
                                  escolaridade %in% 2:6 ~ 2,   # Médio incompleto - 2
                                  escolaridade %in% 7:8 ~ 3,   # Médio completo - 3
                                  escolaridade %in% 9:11 ~ 4)) # Superior completo - 4

### Eixos - nível médio ----

dados <- dados |>
  mutate(eixo_amb_saude_em      = case_when(cboocupacao2002 %in% eixos$`Eixo de Ambiente e Saúde`[[2]] & cbo_tec_em_complet == 1 ~ 1, TRUE ~ 0),
         eixo_conteprocessos_em = case_when(cboocupacao2002 %in% eixos$`Eixo de Controle e Processos Industriais`[[2]] & cbo_tec_em_complet == 1 ~ 1, TRUE ~ 0),
         eixo_desedusoc_em      = case_when(cboocupacao2002 %in% eixos$`Eixo de Desenvolvimento Educacional e Social`[[2]] & cbo_tec_em_complet == 1 ~ 1, TRUE ~ 0),
         eixo_negocios_em       = case_when(cboocupacao2002 %in% eixos$`Eixo de Gestão e Negócios`[[2]] & cbo_tec_em_complet == 1 ~ 1, TRUE ~ 0),
         eixo_infoecomunic_em   = case_when(cboocupacao2002 %in% eixos$`Eixo de Informação e Comunicação`[[2]] & cbo_tec_em_complet == 1 ~ 1, TRUE ~ 0),
         eixo_infraestrutura_em = case_when(cboocupacao2002 %in% eixos$`Eixo de Infraestrutura`[[2]] & cbo_tec_em_complet == 1 ~ 1, TRUE ~ 0),
         eixo_prodaliment_em    = case_when(cboocupacao2002 %in% eixos$`Eixo de Produção Alimentícia`[[2]] & cbo_tec_em_complet == 1 ~ 1, TRUE ~ 0),
         eixo_prodcult_em       = case_when(cboocupacao2002 %in% eixos$`Eixo de Produção Cultural e Design`[[2]] & cbo_tec_em_complet == 1 ~ 1, TRUE ~ 0),
         eixo_prodindust_em     = case_when(cboocupacao2002 %in% eixos$`Eixo de Produção Industrial`[[2]] & cbo_tec_em_complet == 1 ~ 1, TRUE ~ 0),
         eixo_recnaturais_em    = case_when(cboocupacao2002 %in% eixos$`Eixo de Recursos Naturais`[[2]] & cbo_tec_em_complet == 1 ~ 1, TRUE ~ 0),
         eixo_seguranca_em      = case_when(cboocupacao2002 %in% eixos$`Eixo de Segurança`[[2]] & cbo_tec_em_complet == 1 ~ 1, TRUE ~ 0),
         eixo_hospelazer_em     = case_when(cboocupacao2002 %in% eixos$`Eixo de Turismo, Hospitalidade e Lazer`[[2]] & cbo_tec_em_complet == 1 ~ 1, TRUE ~ 0),
         eixo_militar_em        = case_when(cboocupacao2002 %in% eixos$`Eixo Militar`[[2]] & cbo_tec_em_complet == 1 ~ 1, TRUE ~ 0))

### Eixos - nível superior ----

dados <- dados |>
  mutate(eixo_amb_saude_sup      = case_when(cboocupacao2002 %in% eixos_superior$`Eixo de Ambiente e Saúde`[[2]] & cbo_tec_sup == 1 ~ 1, TRUE ~ 0),
         eixo_conteprocessos_sup = case_when(cboocupacao2002 %in% eixos_superior$`Eixo de Controle e Processos Industriais`[[2]] & cbo_tec_sup == 1 ~ 1, TRUE ~ 0),
         eixo_desedusoc_sup      = case_when(cboocupacao2002 %in% eixos_superior$`Eixo de Desenvolvimento Educacional e Social`[[2]] & cbo_tec_sup == 1 ~ 1, TRUE ~ 0),
         eixo_negocios_sup       = case_when(cboocupacao2002 %in% eixos_superior$`Eixo de Gestão e Negócios`[[2]] & cbo_tec_sup == 1 ~ 1, TRUE ~ 0),
         eixo_infoecomunic_sup   = case_when(cboocupacao2002 %in% eixos_superior$`Eixo de Informação e Comunicação`[[2]] & cbo_tec_sup == 1 ~ 1, TRUE ~ 0),
         eixo_infraestrutura_sup = case_when(cboocupacao2002 %in% eixos_superior$`Eixo de Infraestrutura`[[2]] & cbo_tec_sup == 1 ~ 1, TRUE ~ 0),
         eixo_prodaliment_sup    = case_when(cboocupacao2002 %in% eixos_superior$`Eixo de Produção Alimentícia`[[2]] & cbo_tec_sup == 1 ~ 1, TRUE ~ 0),
         eixo_prodcult_sup       = case_when(cboocupacao2002 %in% eixos_superior$`Eixo de Produção Cultural e Design`[[2]] & cbo_tec_sup == 1 ~ 1, TRUE ~ 0),
         eixo_prodindust_sup     = case_when(cboocupacao2002 %in% eixos_superior$`Eixo de Produção Industrial`[[2]] & cbo_tec_sup == 1 ~ 1, TRUE ~ 0),
         eixo_recnaturais_sup    = case_when(cboocupacao2002 %in% eixos_superior$`Eixo de Recursos Naturais`[[2]] & cbo_tec_sup == 1 ~ 1, TRUE ~ 0),
         eixo_seguranca_sup      = case_when(cboocupacao2002 %in% eixos_superior$`Eixo de Segurança`[[2]] & cbo_tec_sup == 1 ~ 1, TRUE ~ 0),
         eixo_hospelazer_sup     = case_when(cboocupacao2002 %in% eixos_superior$`Eixo de Turismo, Hospitalidade e Lazer`[[2]] & cbo_tec_sup == 1 ~ 1, TRUE ~ 0),
         eixo_militar_sup        = case_when(cboocupacao2002 %in% eixos_superior$`Eixo Militar`[[2]] & cbo_tec_sup == 1 ~ 1, TRUE ~ 0))

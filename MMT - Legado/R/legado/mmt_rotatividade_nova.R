
# Configurações iniciais ----

## Carrega pacotes ----
library(data.table)

## Carrega dados
rais = fread('dados/primario/rais-panorama.csv')
rotatividade_antigo_bruto = fread('dados/primario/caged_rotatividade.csv')
rotativadade_novo_bruto = fread('dados/primario/caged-rotatividade-2020.csv')
cbotecnica_nivel_medio = data.table::fread("dados/cbotecnica_nivelmedio.csv", header = TRUE)
cbotecnica_nivel_superior = data.table::fread("dados/cbotecnica_nivelsuperior.csv", header = TRUE)

rais_antes <- rais


# Limpeza ----
## Rais ----
rais[, `:=`(escolaridade_rotatividade = fcase(escolaridade  == 1, 'analfabeto',escolaridade  %in% c(2:6), 'em_incompleto', escolaridade   %in% c(7,8), 'em_completo', escolaridade  <= 11, 'sup_completo'))]

rais_rotatividade = rbind(
  rais[,.(.N), by = .(referencia, cboocupacao2002)][,escolaridade_rotatividade := "geral"],
  rais[,.(.N), by = .(referencia, cboocupacao2002, escolaridade_rotatividade)])

## Caged
### Até 2019
rotatividade_antigo = rotatividade_antigo_bruto[salariomensal >= vl_sm * .5 & salariomensal <= vl_sm*200,]
rotatividade_antigo[, `:=`(admitidosdesligados = ifelse(admitidosdesligados == 1, "admitidos", "desligados"),
                           escolaridade_rotatividade = fcase(grauinstrucao == 1, 'analfabeto',grauinstrucao %in% c(2:6), 'em_incompleto',grauinstrucao  %in% c(7,8), 'em_completo',grauinstrucao  <= 11, 'sup_completo', grauinstrucao == 99, NA_character_))]

rotatividade_caged_antigo = rbind(
  rotatividade_antigo[tipomovdesagregado %in% c(1, 2, 4, 5, 10, 11, 25, 43, 90), .(rotacao = .N), by = .(anodeclarado, cbo2002ocupacao, admitidosdesligados)][, escolaridade_rotatividade := 'geral'],
  rotatividade_antigo[tipomovdesagregado %in% c(1, 2, 4, 5, 10, 11, 25, 43, 90), .(rotacao = .N), by = .(anodeclarado, cbo2002ocupacao, admitidosdesligados, escolaridade_rotatividade)])


rotatividade_caged_antigo = dcast(rotatividade_caged_antigo, anodeclarado + cbo2002ocupacao + escolaridade_rotatividade ~ admitidosdesligados, value.var = 'rotacao')

### 2020
rotatividade_novo = rotativadade_novo_bruto[competencia %/% 100 == 2020 & salario > 1045/2 & salario < 1045 * 200 & tipomovimentacao  %in% c(10, 20, 25, 31, 32, 35, 43, 90),]

rotatividade_novo[,`:=`(admitidosdesligados = ifelse(saldomovimentacao  == 1, "admitidos", "desligados"),
                        escolaridade_rotatividade = fcase(graudeinstrucao == 1, 'analfabeto',graudeinstrucao %in% c(2:6), 'em_incompleto',graudeinstrucao  %in% c(7,8), 'em_completo',graudeinstrucao <= 11, 'sup_completo',graudeinstrucao == 99, NA_character_))]

rotatividade_caged_novo = rbind(
  rotatividade_novo[, .(rotacao = .N), by = .(cbo2002ocupacao, admitidosdesligados)][, escolaridade_rotatividade := 'geral'],
  rotatividade_novo[, .(rotacao = .N), by = .(cbo2002ocupacao, admitidosdesligados, escolaridade_rotatividade)])[,anodeclarado := 2020]

rotatividade_caged_novo = rotatividade_caged_novo[!is.na(escolaridade_rotatividade)]

rotatividade_caged_novo = dcast(rotatividade_caged_novo, anodeclarado + cbo2002ocupacao + escolaridade_rotatividade ~ admitidosdesligados, value.var = 'rotacao')

# Unindo bases
names(rais_rotatividade) = c('anodeclarado', 'cbo2002ocupacao', 'n_trabalhadores', 'escolaridade_rotatividade')

rotatividade = merge(
  rais_rotatividade,
  rbind(rotatividade_caged_novo, rotatividade_caged_antigo), all.x = TRUE)

rotatividade[, `:=`(admitidos = ifelse(is.na(admitidos), 0, admitidos),
                    desligados = ifelse(is.na(desligados), 0, desligados))]

rotatividade = rotatividade[, c('admitidos_anterior', 'desligados_anterior') := shift(.SD, 1, type = 'lag'),
                            .SDcols = c('admitidos', 'desligados'),
                            by = .(cbo2002ocupacao, escolaridade_rotatividade)
]

rotatividade[, rotatividade := ifelse(admitidos_anterior > desligados_anterior, desligados_anterior, admitidos_anterior)/n_trabalhadores]

rotatividade[, cbo_tecnica := fcase(cbo2002ocupacao %in% cbotecnica_nivel_medio$cbo_em, 'tec_em', cbo2002ocupacao %in% cbotecnica_nivel_superior$cbo_superior, "tec_sup", default = "nao_tec")]

rotatividade[, `:=`(tipo = fcase(escolaridade_rotatividade == "geral" & cbo_tecnica %in% c("tec_em","tec_sup"),"tec_geral",
                                 escolaridade_rotatividade == "geral" & cbo_tecnica == "nao_tec","nao_tec_geral",
                                 escolaridade_rotatividade %in% c("analfabeto","em_incompleto","em_completo","sup_completo") & cbo_tecnica == "tec_em", "tec_em",
                                 escolaridade_rotatividade %in% c("analfabeto","em_incompleto","em_completo","sup_completo") & cbo_tecnica == "tec_sup", "tec_sup",
                                 escolaridade_rotatividade == "analfabeto"  & cbo_tecnica == "nao_tec", "analfabeto",
                                 escolaridade_rotatividade == "em_incompleto"  & cbo_tecnica == "nao_tec","em_incompleto",
                                 escolaridade_rotatividade == "em_completo"  & cbo_tecnica == "nao_tec","em_completo",
                                 escolaridade_rotatividade == "sup_completo"  & cbo_tecnica == "nao_tec","sup_completo"),
                    filtro = ifelse(cbo_tecnica %in% c("tec_em","tec_sup"),"tec","nao_tec"),
                    geral = ifelse(escolaridade_rotatividade %in% c("geral"),"geral","subgrupo"))]

rotatividade$escolaridade_rotatividade <- NULL
rotatividade$cbo_tecnica <- NULL

library(tidyverse)

nova_rotatividade <- rotatividade %>% 
  group_by(anodeclarado, tipo, filtro, geral) %>% 
  summarise(rotatividade_media = mean(rotatividade, na.rm = T))

readr::write_excel_csv2(nova_rotatividade, "dados/mmt-rotatividade-nova.csv")

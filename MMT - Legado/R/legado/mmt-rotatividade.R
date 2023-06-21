
# Configurações iniciais ----

## Carrega pacotes ----
library(data.table)

## Carrega dados
rais = fread('dados/primario/rais-panorama.csv')
rotatividade_antigo_bruto = fread('dados/primario/caged_rotatividade.csv')
rotativadade_novo_bruto = fread('dados/primario/caged-rotatividade-2020.csv')
cbotecnica_nivel_medio = data.table::fread("dados/cbotecnica_nivelmedio.csv", header = TRUE)
cbotecnica_nivel_superior = data.table::fread("dados/cbotecnica_nivelsuperior.csv", header = TRUE)

savehistory(file = "bases.Rhistory")

# Limpeza ----
## Rais ----
rais[, `:=`(escolaridade_rotatividade = fcase(escolaridade  <= 6, 'até medio incompleto', escolaridade  <= 8, 'até superior incompleto', escolaridade  <= 11, 'superior'))]

rais_rotatividade = rbind(
  rais[,.(.N), by = .(referencia, cboocupacao2002)][,escolaridade_rotatividade := "total"],
  rais[,.(.N), by = .(referencia, cboocupacao2002, escolaridade_rotatividade)])

## Caged
### Até 2019
rotatividade_antigo = rotatividade_antigo_bruto[salariomensal >= vl_sm * .5 & salariomensal <= vl_sm*200,]
rotatividade_antigo[, `:=`(admitidosdesligados = ifelse(admitidosdesligados  == 1, "admitidos", "desligados"),
                    escolaridade_rotatividade = fcase(grauinstrucao   <= 6, 'até medio incompleto', grauinstrucao   <= 8, 'até superior incompleto', grauinstrucao   <= 11, 'superior'))]

rotatividade_caged_antigo = rbind(
  rotatividade_antigo[tipomovdesagregado %in% c(1, 2, 4, 5, 10, 11, 25, 43, 90), .(rotacao = .N), by = .(anodeclarado, cbo2002ocupacao, admitidosdesligados)][, escolaridade_rotatividade := 'Total'],
  rotatividade_antigo[tipomovdesagregado %in% c(1, 2, 4, 5, 10, 11, 25, 43, 90), .(rotacao = .N), by = .(anodeclarado, cbo2002ocupacao, admitidosdesligados, escolaridade_rotatividade)])


rotatividade_caged_antigo = dcast(rotatividade_caged_antigo, anodeclarado + cbo2002ocupacao + escolaridade_rotatividade ~ admitidosdesligados, value.var = 'rotacao')

### 2020
rotatividade_novo = rotativadade_novo_bruto[competencia %/% 100 == 2020 & salario > 1045/2 & salario < 1045 * 200 & tipomovimentacao  %in% c(10, 20, 25, 31, 32, 35, 43, 90),]

rotatividade_novo[,`:=`(admitidosdesligados = ifelse(saldomovimentacao  == 1, "admitidos", "desligados"),
                                                                                              escolaridade_rotatividade = fcase(graudeinstrucao    <= 6, 'até medio incompleto', graudeinstrucao    <= 8, 'até superior incompleto', graudeinstrucao    <= 11, 'superior', graudeinstrucao == 99, NA_character_))]

rotatividade_caged_novo = rbind(
  rotatividade_novo[, .(rotacao = .N), by = .(cbo2002ocupacao, admitidosdesligados)][, escolaridade_rotatividade := 'Total'],
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

rotatividade[, cbo_tecnica := fcase(cbo2002ocupacao %in% cbotecnica_nivel_medio$cbo_em, 'Técnica Média', cbo2002ocupacao %in% cbotecnica_nivel_superior$cbo_superior, "Técnica Superior", default = "Outra CBO")]

# Salvando resultados
readr::write_excel_csv2(rotatividade, "dados/mmt-rotatividade.csv")

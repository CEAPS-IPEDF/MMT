# Gera dados por tipo de emprego (clt, estatutario) e educação (superior e medio)

# Carregando pacotes
library(data.table)

# Carregando dados
ocupacoes_em = readxl::read_excel('dados/Ocupações e cursos.xlsx', sheet = "cursosXcbomedio")
ocupacoes_sup = readxl::read_excel('dados/Ocupações e cursos.xlsx', sheet = "cursosXcbosuperior")
rais = data.table::fread('dados/rais-panorama.csv')
ano_referencia = 2020
rais = rais[referencia <= ano_referencia,]

# Definindo funcoes
extrai_tabela = function(dados, nomes, conjunto_colunas){ 
  lista = vector(conjunto_colunas, mode = 'list')
  tamanho = length(nomes)
  
  for (i in 0:(conjunto_colunas - 1)) {
    lista[[i+1]] = dados[,names(dados[,-2])[((i*tamanho)+1):((i*tamanho)+tamanho)]]
    names(lista[[i+1]]) = nomes
    lista[[i+1]]$ocupacao = dados$ocupação
    lista[[i+1]] = lista[[i+1]][!is.na(lista[[i+1]]$cbo),]
  }
  
  dados = data.table::rbindlist(lista)
  
  return(dados)
}

faz_avaliacao_vinculos = function(dados, classificacao = NULL, tipo_selecionado = NULL){
  temp = dados[cbo_tec == TRUE,]
  
  if (is.null(classificacao) == FALSE){
    temp = temp[classificacao_vinculo %in% classificacao,]
  }
  
  if (is.null(tipo_selecionado) == FALSE){
    if (tipo_selecionado == 'superior'){
      temp = temp[cbo_tec_sup == TRUE,]
    }
    
    if (tipo_selecionado == 'medio'){
      temp = temp[cbo_tec_em_complet == TRUE,]
    }
    }
  
  dados_rais = merge(
    merge(
      temp[, .(.N), by = .(referencia, cboocupacao2002)][, .(mediana_vinculos = median(N)), by = cboocupacao2002],
      temp[qtdhoracontr != 0, .(mediana_salario_hora = median(salario_hora)), by = cboocupacao2002]),
    merge(
      temp[referencia == ano_referencia, .(vinculos_referencia = .N), by = cboocupacao2002][, proporcao := vinculos_referencia/sum(vinculos_referencia)],
      temp[qtdhoracontr != 0 & referencia == ano_referencia, .(mediana_salario_hora_referencia = median(salario_hora)), by = cboocupacao2002]))
  
  dados_rais[, cboocupacao2002 := as.integer(cboocupacao2002)]
  
  return(dados_rais)
}

# Extraindo informações das tabelas
base = rbind(
  extrai_tabela(ocupacoes_em, c('cbo', 'codigo_curso', 'curso'), 8)[, tipo := 'medio'],
  extrai_tabela(ocupacoes_sup, c('cbo', 'curso'), 13)[, tipo := 'superior'], fill = TRUE)

rais[, classificacao_vinculo := fcase(
  tipovinculo %in% c(10,15,20,25,60,65,70,75), 'celetistas',
  tipovinculo %in% c(30,31,35), 'estatutarios',
  tipovinculo =='55', 'aprendiz',
  tipovinculo %in% c(40,50,80,90,95,96,97,-1), 'outros')
  ]

opcoes = tidyr::expand_grid(classificacao = list(c('estatutarios'), c('celetistas', 'estatutarios', 'aprendiz', 'outros')),
                   nivel_educacional = c('medio', 'superior', 'total'))

opcoes$nome = paste(c(rep('estaturarios', 3), rep('total', 3)), opcoes$nivel_educacional)

dados_vinculos = purrr::map2(opcoes$classificacao,
                             opcoes$nivel_educacional,
                             ~faz_avaliacao_vinculos(rais, classificacao = .x,tipo_selecionado =  .y))

# Adicionando a base
dados_vinculos[[1]] = dplyr::right_join(dados_vinculos[[1]], base[tipo == 'medio',], by = c('cboocupacao2002' = 'cbo'))

dados_vinculos[[2]] = dplyr::right_join(dados_vinculos[[2]], base[tipo == 'superior',], by = c('cboocupacao2002' = 'cbo'))
dados_vinculos[[3]] = dplyr::right_join(dados_vinculos[[3]], base, by = c('cboocupacao2002' = 'cbo'))
dados_vinculos[[4]] =  dplyr::right_join(dados_vinculos[[4]], base[tipo == 'medio',], by = c('cboocupacao2002' = 'cbo'))

dados_vinculos[[5]] =  dplyr::right_join(dados_vinculos[[5]], base[tipo == 'superior',], by = c('cboocupacao2002' = 'cbo'))
dados_vinculos[[6]] = dplyr::right_join(dados_vinculos[[6]], base, by = c('cboocupacao2002' = 'cbo'))

# Salvando resultados
wb = openxlsx::createWorkbook()

purrr::walk(opcoes$nome, ~openxlsx::addWorksheet(wb, .x))
purrr::walk2(opcoes$nome, dados_vinculos, ~openxlsx::writeData(wb, .x, .y))

openxlsx::saveWorkbook(wb, "dados/curso-vinculos-salarios-2020.xlsx", overwrite = TRUE)
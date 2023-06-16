#'*Script que faz a limpeza dos dados do CAGED para o MMT*

# Carregar pacotes ----

library(odbc)
library(tidyverse)
library(readxl)

`%notin%` <- Negate(`%in%`)
options(readr.show_col_types = FALSE)

# Importação dos dados ----

db <- DBI::dbConnect(odbc(),
                     "db_codeplan", 
                     uid = Sys.getenv("matricula"), 
                     pwd = Sys.getenv("senha")) 

### Novo CAGED ----

novo_caged <- DBI::dbGetQuery(db, 
"SELECT
  CAST(SUBSTRING(CAST(competencia AS CHAR(6)), 1, 4) AS INT) AS anodeclarado,
  CAST(SUBSTRING(CAST(competencia AS CHAR(6)), 5, 2) AS INT) AS mesdeclarado,
  competencia,
  cbo2002ocupacao,
  graudeinstrucao AS grauinstrucao,
  racacor,
  sexo,
  idade,
  indtrabintermitente,
  indicadoraprendiz AS indaprendiz,
  CAST(REPLACE(valorsalariofixo, ',', '.') AS FLOAT) AS salariomensal,
  tipomovimentacao AS tipomovdesagregado,
  CASE WHEN saldo = 1 THEN 1 ELSE 2 END AS admitidosdesligados,
  saldo AS saldomov,
  'dp' AS tipo
FROM
  caged.caged_202001_atual_MOV
WHERE
  UF = 53
UNION ALL
SELECT
  CAST(SUBSTRING(CAST(competencia AS CHAR(6)), 1, 4) AS INT) AS anodeclarado,
  CAST(SUBSTRING(CAST(competencia AS CHAR(6)), 5, 2) AS INT) AS mesdeclarado,
  competencia,
  cbo2002ocupacao,
  graudeinstrucao AS grauinstrucao,
  racacor,
  sexo,
  idade,
  indtrabintermitente,
  indicadoraprendiz AS indaprendiz,
  CAST(REPLACE(valorsalariofixo, ',', '.') AS FLOAT) AS salariomensal,
  tipomovimentacao AS tipomovdesagregado,
  CASE WHEN saldo = 1 THEN 1 ELSE 2 END AS admitidosdesligados,
  saldo AS saldomov,
  'fp' AS tipo
FROM
  caged.caged_202002_atual_FOR
WHERE
  UF = 53") |>
  left_join(data.frame(anodeclarado = c(2011:2023),
                       vl_sm = c(545, 622, 678, 724, 788, 880, 937, 954, 998, 1045, 1100, 1212, 1320)))

### CAGED ----

caged <- DBI::dbGetQuery(db, 
"SELECT
  CAST(SUBSTRING(CAST(competenciadeclarada AS CHAR(6)), 1, 4) AS INT) AS anodeclarado,
  CAST(SUBSTRING(CAST(competenciadeclarada AS CHAR(6)), 5, 2) AS INT) AS mesdeclarado,
  competenciadeclarada AS competencia,
  cbo2002ocupacao,
  grauinstrucao,
  racacor,
  sexo,
  idade,
  indtrabintermitente,
  indaprendiz,
  salariomensal,
  tipomovdesagregado,
  admitidosdesligados,
  saldomov,
  'dp' AS tipo
FROM
  caged.caged_200701_atual_DP
WHERE
  UF = 53 AND competenciadeclarada > 201001
UNION ALL
SELECT
  CAST(SUBSTRING(CAST(competenciamovimentacao AS CHAR(6)), 1, 4) AS INT) AS anodeclarado,
  CAST(SUBSTRING(CAST(competenciamovimentacao AS CHAR(6)), 5, 2) AS INT) AS mesdeclarado,
  competenciamovimentacao AS competencia,
  cbo2002ocupacao,
  grauinstrucao,
  CAST(NULL AS INT) AS racacor,
  sexo,
  idade,
  indtrabintermitente,
  indaprendiz,
  salariomensal,
  tipomovdesagregado,
  admitidosdesligados,
  saldomov,
  'fp' AS tipo
FROM
  caged.caged_200701_atual_fp
WHERE
  UF = 53 AND competenciamovimentacao >= 201001") |>
  left_join(data.frame(anodeclarado = c(2011:2023),
                       vl_sm = c(545, 622, 678, 724, 788, 880, 937, 954, 998, 1045, 1100, 1212, 1320)))

dbDisconnect(db)

### RAIS ----

rais <- readRDS("Dados/RAIS.RDS")

### CBOs técnicas ----

cbotec_em <- as.character(read_csv("Dados/cbotecnica_nivelmedio.csv")[[1]])
cbotec_sup <- as.character(read_csv("Dados/cbotecnica_nivelsuperior.csv")[[1]])

# Tratamento dos dados ----

## Rotatividade ----

### RAIS ----

rotatividade_rais <- rbind(rais |>
                             group_by(referencia, cboocupacao2002) |>
                             summarise(n_trabalhadores = n(),
                                       escolaridade = 0),
                           rais |>
                             group_by(referencia, cboocupacao2002, escolaridade) |>
                             summarise(n_trabalhadores = n())) |>
  mutate(cboocupacao2002 = as.character(cboocupacao2002),
         escolaridade = case_when(escolaridade == 0 ~ "Geral",
                                  escolaridade == 1 ~ "Analfabeto",
                                  escolaridade == 2 ~ "Médio incompleto",
                                  escolaridade == 3 ~ "Médio completo",
                                  escolaridade == 4 ~ "Superior completo"))

### CAGED ----

#' Admissão e desligamentos:
#' 1 - Admitidos
#' 2 - Desligados

#' Tipo de movimento:
#' 1 - Admissão por primeiro emprego
#' 2 - Admissão por reemprego
#' 4 - Desligamento por demissão sem justa causa
#' 5 - Desligamento por demissão com justa causa
#' 10 - Admissão por reintegraçao
#' 11 - Desligamento por término de contrato
#' 25 - Contrato trabalho prazo determinado
#' 43 - Término contrato trabalho prazo determinado
#' 90 - Desliamento por acordo empregado e empregador

rotatividade_caged <- caged |>
  filter(salariomensal >= vl_sm * .5 & salariomensal <= vl_sm * 200) |>
  mutate(cbo2002ocupacao = as.character(cbo2002ocupacao),
         admitidosdesligados = case_when(admitidosdesligados == 1 ~ "Admitidos",
                                         admitidosdesligados == 2 ~ "Desligados"),
         escolaridade = case_when(grauinstrucao == 1 ~ "Analfabeto",
                                  grauinstrucao %in% 2:6 ~ "Médio incompleto",
                                  grauinstrucao %in% 7:8 ~ "Médio completo",
                                  grauinstrucao %in% 9:11 ~ "Superior completo",
                                  grauinstrucao == 99 ~ NA))

rotatividade_caged_antigo <- rbind(rotatividade_caged |>
                                     filter(tipomovdesagregado %in% c(1, 2, 4, 5, 10, 11, 25, 43, 90)) |>
                                     group_by(anodeclarado, cbo2002ocupacao, admitidosdesligados) |>
                                     summarise(rotacao = n(),
                                               escolaridade = "Geral"),
                                   rotatividade_caged |>
                                     filter(tipomovdesagregado %in% c(1, 2, 4, 5, 10, 11, 25, 43, 90)) |>
                                     group_by(anodeclarado, cbo2002ocupacao, admitidosdesligados, escolaridade) |>
                                     summarise(rotacao = n())) |>
  pivot_wider(names_from = admitidosdesligados,
              values_from = rotacao) |>
  arrange(anodeclarado, cbo2002ocupacao)

### Novo CAGED ----

#' Admissão e desligamentos:
#' 1 - Admitidos
#' 2 - Desligados

#' Tipo de movimento:
#' 10 - Admissão por primeiro emprego
#' 20 - Admissão por reemprego
#' 25 - Admissão por contrato trabalho prazo determinado
#' 31 - Desligamento por demissão sem justa causa
#' 32 - Desligamento por demissão com justa causa
#' 35 - Admissão por reintegração
#' 43 - Término contrato trabalho prazo determinado
#' 45 - Desligamento por término de contrato
#' 90 - Desligamento por acordo entre empregado e empregador
#' 97 - Admissão de tipo Ignorado
#' 98 - Desligamento de tipo Ignorado

rotatividade_novo <- novo_caged |>
  filter(salariomensal >= vl_sm * .5 & salariomensal <= vl_sm * 200) |>
  mutate(cbo2002ocupacao = as.character(cbo2002ocupacao),
         admitidosdesligados = case_when(admitidosdesligados == 1 ~ "Admitidos",
                                         admitidosdesligados == 2 ~ "Desligados"),
         escolaridade = case_when(grauinstrucao == 1 ~ "Analfabeto",
                                  grauinstrucao %in% 2:6 ~ "Médio incompleto",
                                  grauinstrucao %in% 7:8 ~ "Médio completo",
                                  grauinstrucao %in% 9:11 ~ "Superior completo",
                                  grauinstrucao == 99 ~ NA))

rotatividade_caged_novo <- rbind(rotatividade_novo |>
                                   filter(tipomovdesagregado %in% c(10, 20, 25, 31, 32, 35, 43, 45, 90, 97, 98)) |>
                                   group_by(anodeclarado, cbo2002ocupacao, admitidosdesligados) |>
                                   summarise(rotacao = n(),
                                             escolaridade = "Geral"),
                                 rotatividade_caged |>
                                   filter(tipomovdesagregado %in% c(10, 20, 25, 31, 32, 35, 43, 45, 90, 97, 98)) |>
                                   group_by(anodeclarado, cbo2002ocupacao, admitidosdesligados, escolaridade) |>
                                   summarise(rotacao = n())) |>
  pivot_wider(names_from = admitidosdesligados,
              values_from = rotacao) |>
  arrange(anodeclarado, cbo2002ocupacao)


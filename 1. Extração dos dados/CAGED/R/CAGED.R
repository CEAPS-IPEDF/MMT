#'*Script que faz a limpeza dos dados do CAGED para o MMT*

# Carregar pacotes ----

library(odbc)      # Conexão ao Banco de Dados
library(tidyverse) # Manipulação dos dados
library(readxl)    # Leitura de Arquivos em .xlsx

`%notin%` <- Negate(`%in%`)           # Função de filtro
options(readr.show_col_types = FALSE) # Omitir formato das variáveis na visualização

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

rais <- readRDS("Dados/RAIS.RDS") |>
  mutate(escolaridade = case_when(escolaridade == 1 ~ "Analfabeto",
                                  escolaridade == 2 ~ "Médio incompleto",
                                  escolaridade == 3 ~ "Médio completo",
                                  escolaridade == 4 ~ "Superior completo"))

### CBOs técnicas ----

cbotec_em <- as.character(read_csv("Dados/cbotecnica_nivelmedio.csv")[[1]])
cbotec_sup <- as.character(read_csv("Dados/cbotecnica_nivelsuperior.csv")[[1]])

# Tratamento dos dados ----

## Rotatividade ----

### RAIS ----

rotatividade_rais <- rbind(rais |>
                             group_by(referencia, cboocupacao2002) |>
                             summarise(n_trabalhadores = n(),
                                       escolaridade = "Geral"),
                           rais |>
                             group_by(referencia, cboocupacao2002, escolaridade) |>
                             summarise(n_trabalhadores = n())) |>
  mutate(cboocupacao2002 = as.character(cboocupacao2002)) |>
  rename(anodeclarado = referencia,
         cbo2002ocupacao = cboocupacao2002)

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
         admitidosdesligados = case_when(admitidosdesligados == 1 ~ "admitidos",
                                         admitidosdesligados == 2 ~ "desligados"),
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

remove(rotatividade_caged)

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
         admitidosdesligados = case_when(admitidosdesligados == 1 ~ "admitidos",
                                         admitidosdesligados == 2 ~ "desligados"),
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
                                 rotatividade_novo |>
                                   filter(tipomovdesagregado %in% c(10, 20, 25, 31, 32, 35, 43, 45, 90, 97, 98)) |>
                                   group_by(anodeclarado, cbo2002ocupacao, admitidosdesligados, escolaridade) |>
                                   summarise(rotacao = n())) |>
  pivot_wider(names_from = admitidosdesligados, 
              values_from = rotacao,
              names_glue = "{admitidosdesligados}") |>
  arrange(anodeclarado, cbo2002ocupacao)

remove(rotatividade_novo)

# Merge das bases ----

rotatividade <- merge(rotatividade_rais,
                      rbind(rotatividade_caged_novo, 
                            rotatividade_caged_antigo), all.x = TRUE) |>
  mutate(admitidos = case_when(is.na(admitidos) ~ 0,
                               TRUE ~ admitidos),
         desligados = case_when(is.na(desligados) ~ 0,
                                TRUE ~ desligados))

remove(rotatividade_rais, rotatividade_caged_antigo, rotatividade_caged_novo)

## Criação dos lags ----

rotatividade <- rotatividade |>
  group_by(cbo2002ocupacao, escolaridade) |>
  mutate(admitidos_anterior = lag(admitidos),
         desligados_anterior = lag(desligados))

## Cálculo da rotatividade ----

rotatividade <- rotatividade |>
  mutate(rotatividade = case_when(admitidos_anterior > desligados_anterior ~ desligados_anterior,
                                  TRUE ~ admitidos_anterior) / n_trabalhadores)

## CBOs técnicas ----

rotatividade <- rotatividade |>
  mutate(cbo_tecnica = case_when(cbo2002ocupacao %in% cbotec_em ~ "tec_em",
                                 cbo2002ocupacao %in% cbotec_sup ~ "tec_sup",
                                 TRUE ~ "nao_tec"),
         tipo = case_when(escolaridade == "Geral" & cbo_tecnica %in% c("tec_em", "tec_sup") ~ "Média dos trabalhadores técnicos",
                          escolaridade == "Geral" & cbo_tecnica == "nao_tec" ~ "Média dos trabalhadores não técnicos",
                          escolaridade %in% c("Analfabeto", "Médio incompleto", "Médio completo", "Superior completo") & cbo_tecnica == "tec_em" ~ "Técnicos de nível médio",
                          escolaridade %in% c("Analfabeto", "Médio incompleto", "Médio completo", "Superior completo") & cbo_tecnica == "tec_sup" ~ "Técnicos de nível superior",
                          escolaridade %in% c("Analfabeto", "Médio incompleto") & cbo_tecnica == "nao_tec" ~ "Até fundamental completo",
                          escolaridade == "Médio completo" & cbo_tecnica == "nao_tec" ~ "Médio completo",
                          escolaridade == "Superior completo" & cbo_tecnica == "nao_tec" ~ "Superior completo"),
         filtro = case_when(cbo_tecnica %in% c("tec_em", "tec_sup") ~ "Técnico",
                            TRUE ~ "Não técnico"),
         geral = case_when(escolaridade == "Geral" ~ "Geral",
                           TRUE ~ "Subgrupo"))

## Rotatividade média ----

rotatividade_media <- rotatividade %>% 
  group_by(anodeclarado, tipo, filtro, geral) %>% 
  summarise(rotatividade_media = mean(rotatividade, na.rm = T)) |>
  filter(anodeclarado > 2011)

rotatividade_media |>
  ggplot(aes(x = anodeclarado, y = rotatividade_media, col = tipo)) +
  geom_line(linewidth = 1) + 
  geom_point(size = 2) + 
  scale_color_manual(values = c("#46a462","#3e9974","#2e818d","#2b7398", "#2960a7","#2b597a","#f2cb64","#a6a6a6")) +
  labs(x = "", 
       y = "Taxa de Rotatividade (%)",
       col = "",
       title = "Rotatividade no Distrito Federal (Todos)", ) +
  scale_y_continuous(label = scales::percent,
                     breaks = seq(0, .7, by = .1)) +
  scale_x_continuous(breaks = seq(2011, 2021, by = 1), expand = c(0.01,0.01)) +
  theme(legend.position = "bottom") +
  theme_classic()

paleta_mmt <- c("#d92335","#bd3928","#f87d28","#064471","#2b597a","#0a78c7","#fb8e80")

# Exportar CSV ----

write_excel_csv2(rotatividade_media, "Dados/Rotatividade.csv")


# Script que gera base de rotatividade para o MMT

# Limpa Diretório
rm(list=ls(all=T))
# Dados sem notação científica
options(scipen=100)

# Carregando pacotes ----
library(easypackages)
libraries("dplyr", "data.table","tidyverse","DBI","odbc","RODBC")


# Extende Número de Linhas das visualizações
rstudioapi::writeRStudioPreference("data_viewer_max_columns", 300L)

# Conexão ao banco de dados ----
# DBI e odbc exibem conexões na aba Environment, mas não funcional para CAGED antigo
#db <- DBI::dbConnect(odbc::odbc(), "db_codeplan",uid=('codeplan'),pwd=('codeplan'))
db <- RODBC::odbcConnect("db_codeplan",uid=('codeplan'),pwd=('codeplan'))

# Bases
## Salario Mínimo ----
vl_sm <- data.frame(anodeclarado = c(2011:2022),
                    vl_sm = c(545,622,678,724,788,880,937,954,998,1045,1100,1212))

## Bases CAGED ----
### Novo CAGED ----
# Programação SQL Novo CAGED Dentro e Fora do prazo
#novo_caged_sql <- 
#  "select
#cast(substring(cast(competencia as varchar(6)),1,4) as int) as anodeclarado,
#cast(substring(cast(competencia as varchar(6)),5,2) as int) as mesdeclarado,
#competencia,
#cbo2002ocupacao,
#graudeinstrucao as grauinstrucao,
#racacor,
#sexo,
#idade,
#indtrabintermitente,
#indicadoraprendiz as indaprendiz,
#--vl_sm,
#cast(replace(valorsalariofixo ,',','.') as float) as salariomensal,
#tipomovimentacao as tipomovdesagregado,
#case when saldo = 1 then 1 else 2 end as admitidosdesligados,
#saldo as saldomov,
#tipo = 'dp'
#from caged.caged_202001_atual_MOV where UF = 53"


novo_caged_sql <- 
"select
cast(substring(cast(competencia as varchar(6)),1,4) as int) as anodeclarado,
cast(substring(cast(competencia as varchar(6)),5,2) as int) as mesdeclarado,
competencia,
cbo2002ocupacao,
graudeinstrucao as grauinstrucao,
racacor,
sexo,
idade,
indtrabintermitente,
indicadoraprendiz as indaprendiz,
--vl_sm,
cast(replace(valorsalariofixo ,',','.') as float) as salariomensal,
tipomovimentacao as tipomovdesagregado,
case when saldo = 1 then 1 else 2 end as admitidosdesligados,
saldo as saldomov,
tipo = 'dp'
from caged.caged_202001_atual_MOV where UF = 53
union all
select
cast(substring(cast(competencia as varchar(6)),1,4) as int) as anodeclarado,
cast(substring(cast(competencia as varchar(6)),5,2) as int) as mesdeclarado,
competencia,
cbo2002ocupacao,
graudeinstrucao as grauinstrucao,
racacor,
sexo,
idade,
indtrabintermitente,
indicadoraprendiz as indaprendiz,
--vl_sm,
cast(replace(valorsalariofixo ,',','.') as float) as salariomensal,
tipomovimentacao as tipomovdesagregado,
case when saldo = 1 then 1 else 2 end as admitidosdesligados,
saldo as saldomov,
tipo = 'fp'
from caged.caged_202002_atual_FOR where UF = 53"

# base novo caged
novo_caged <- RODBC::sqlQuery(db, novo_caged_sql)

novo_caged <- novo_caged %>%
  #converte variavel de salário em número (alterando vírgulas para pontos antes da conversão)
  #mutate(salariomensal = as.numeric(sub(",", ".",novo_caged$salariomensal, fixed = TRUE))) %>%
  #une base de salario mínimo 
  left_join(vl_sm, by = "anodeclarado")

#saveRDS(novo_caged, "dados/novo_caged.RDS")

### CAGED Antigo ----
# Programação SQL CAGED antigo Dentro do Prazo e Fora do Prazo
caged_sql <- 
"SELECT
cast(substring(cast(competenciadeclarada as varchar(6)),1,4) as int) as anodeclarado,
cast(substring(cast(competenciadeclarada as varchar(6)),5,2) as int) as mesdeclarado,
competenciadeclarada as competencia,
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
tipo = 'dp'
FROM caged_200701_atual_DP WHERE UF = 53 and anodeclarado > 2010
union all
SELECT 
cast(substring(cast(competenciamovimentacao  as varchar(6)),1,4) as int) as anodeclarado,
cast(substring(cast(competenciamovimentacao as varchar(6)),5,2) as int) as mesdeclarado,
competenciamovimentacao as competencia,
cbo2002ocupacao,
grauinstrucao,
cast(null as int) as racacor,
sexo,
idade,
indtrabintermitente,
indaprendiz,
salariomensal,
tipomovdesagregado,
admitidosdesligados,
saldomov,
tipo = 'fp'
FROM caged_200701_atual_fp WHERE UF = 53 and cast(substring(cast(competenciamovimentacao as varchar(6)),1,4) as int) > 2010"



# base caged dp
caged <- RODBC::sqlQuery(db, caged_sql)

caged <- caged %>% 
  #une base de salario mínimo 
  left_join(vl_sm, by = "anodeclarado")

## Conferindo Saldo CAGED ----
caged %>% filter(tipo == "dp") %>% select(saldomov) %>% sum()
caged %>% filter(tipo == "fp") %>% select(saldomov) %>% sum()

novo_caged %>% filter(anodeclarado == "2020",tipo == "dp") %>% select(saldomov) %>% sum()
novo_caged %>% filter(anodeclarado == "2020",tipo == "fp") %>% select(saldomov) %>% sum()

## RAIS ----
rais = data.table::fread('dados/rais-panorama-2021.csv',encoding = "Latin-1")

## CBOs Tec ----
cbotec_em = data.table::fread("dados/cbotecnica_nivelmedio.csv", header = TRUE)
cbotec_sup = data.table::fread("dados/cbotecnica_nivelsuperior.csv", header = TRUE)


# Limpeza ----
## RAIS ----
# criandovariavel de escolaridade
rais <- rais %>% 
  mutate(escolaridade_rotatividade = fcase(escolaridade  == 1,'analfabeto',
                                           escolaridade  %in% c(2:6), 'em_incompleto', 
                                           escolaridade   %in% c(7,8), 'em_completo', 
                                           escolaridade  <= 11, 'sup_completo'))
# base rais para rotatividade
## geral
rais_rotatividade = rbind(
  rais %>% 
    group_by(referencia,cboocupacao2002) %>% 
    summarise(n_trabalhadores = n(),
              escolaridade_rotatividade = "geral"),
## por escolaridade
  rais %>% 
    group_by(referencia,cboocupacao2002, escolaridade_rotatividade) %>% 
    summarise(n_trabalhadores = n())
)

rais_rotatividade$cboocupacao2002 <- as.character(rais_rotatividade$cboocupacao2002)

## Caged Antigo ----
# Limpeza 
rotatividade_caged <- caged %>%
  # filtrando Salários muito altos ou muito baixos (0,5 Salários Mínimos ou mais de 200 Salários Mínimos)
  filter(salariomensal >= vl_sm * .5 & salariomensal <= vl_sm*200) %>%
  # montando coluna de admitidos e desligados e grau de escolaridade
  mutate(admitidosdesligados = ifelse(admitidosdesligados == 1, "admitidos", "desligados"),
         escolaridade_rotatividade = fcase(grauinstrucao == 1,'analfabeto',
                                           grauinstrucao %in% c(2:6),'em_incompleto',
                                           grauinstrucao  %in% c(7,8),'em_completo',
                                           grauinstrucao  <= 11,'sup_completo',
                                           grauinstrucao == 99, NA_character_))
# Base inicial 
rotatividade_caged_antigo <- rbind(
  rotatividade_caged %>% 
    # Filtrando para desligamentos e admissões adequados
    # 1	Admissão por primeiro emprego
    # 2	Admissão por reemprego
    # 25	Admissão por contrato trabalho prazo determinado
    # 4	Desligamento por demissão sem justa causa
    # 5	Desligamento por demissão com justa causa
    # 10	Admissão por reintegração
    # 43	Término contrato trabalho prazo determinado
    # 11	Desligamento por Término de contrato
    # 90	Desligamento por Acordo entre empregado e empregador
    filter(tipomovdesagregado %in% c(1, 2, 4, 5, 10, 11, 25, 43, 90)) %>%
    # Agrupando por ano, CBO e Admitidos
    group_by(anodeclarado, cbo2002ocupacao, admitidosdesligados) %>% 
    summarise(rotacao = n(),
              escolaridade_rotatividade = "geral"),
  rotatividade_caged %>%
    # Filtrando para desligamentos e admissões adequados
    filter(tipomovdesagregado %in% c(1, 2, 4, 5, 10, 11, 25, 43, 90)) %>%
    # Agrupando por ano, CBO e Admitidos
    group_by(anodeclarado, cbo2002ocupacao, admitidosdesligados, escolaridade_rotatividade) %>% 
    summarise(rotacao = n()))

# Reorganização dos dados para coluna de admitidos e desligados por ano, escolaridade e e CBO
rotatividade_caged_antigo = dcast(rotatividade_caged_antigo, anodeclarado + cbo2002ocupacao + escolaridade_rotatividade ~ admitidosdesligados,
                                  value.var = 'rotacao')

## Novo CAGED ----
rotatividade_novo <- novo_caged %>%
  filter(salariomensal >= vl_sm*0.5 & salariomensal <= vl_sm*200) %>% 
  # montando coluna de admitidos e desligados e grau de escolaridade
  mutate(admitidosdesligados = ifelse(admitidosdesligados == 1, "admitidos", "desligados"),
         escolaridade_rotatividade = fcase(grauinstrucao == 1,'analfabeto',
                                           grauinstrucao %in% c(2:6),'em_incompleto',
                                           grauinstrucao  %in% c(7,8),'em_completo',
                                           grauinstrucao  <= 11,'sup_completo',
                                           grauinstrucao == 99, NA_character_))


rotatividade_caged_novo <- rbind(
  rotatividade_novo %>% 
    #Filtrando para desligamentos e admissões adequados
    # 10	Admissão por primeiro emprego
    # 20	Admissão por reemprego
    # 25	Admissão por contrato trabalho prazo determinado
    # 31	Desligamento por demissão sem justa causa
    # 32	Desligamento por demissão com justa causa
    # 35	Admissão por reintegração
    # 43	Término contrato trabalho prazo determinado
    # 45	Desligamento por Término de contrato
    # 90	Desligamento por Acordo entre empregado e empregador
    # 97	Admissão de Tipo Ignorado
    # 98	Desligamento de Tipo Ignorado
    filter(tipomovdesagregado %in% c(10,20, 25, 31, 32, 35, 43,45,90,97,98)) %>%
    # Agrupando por ano, CBO e Admitidos
    group_by(anodeclarado, cbo2002ocupacao, admitidosdesligados) %>% 
    summarise(rotacao = n(),
              escolaridade_rotatividade = "geral"),
  rotatividade_novo %>%
    # Filtrando para desligamentos e admissões adequados
    filter(tipomovdesagregado %in% c(10,20, 25, 31, 32, 35, 43,45,90,97,98)) %>%
    # Agrupando por ano, CBO e Admitidos
    group_by(anodeclarado, cbo2002ocupacao, admitidosdesligados, escolaridade_rotatividade) %>% 
    summarise(rotacao = n())) #%>% 
  # eliminando escolaridades com NAs
  #filter(!is.na(escolaridade_rotatividade))

rotatividade_caged_novo$cbo2002ocupacao <- as.character(rotatividade_caged_novo$cbo2002ocupacao)

# Reorganização dos dados para coluna de admitidos e desligados por ano, escolaridade e e CBO
rotatividade_caged_novo = data.table::dcast(rotatividade_caged_novo, anodeclarado + cbo2002ocupacao + escolaridade_rotatividade ~ admitidosdesligados, 
                                value.var = 'rotacao')

# Unindo bases ----
# Nomes Base RAIS
names(rais_rotatividade) = c('anodeclarado', 'cbo2002ocupacao', 'n_trabalhadores', 'escolaridade_rotatividade')

# Bases Empilhadas
rotatividade = merge(
  #Rais
  rais_rotatividade,
  #Caged
  rbind(rotatividade_caged_novo, 
        rotatividade_caged_antigo), all.x = TRUE) %>% 
  mutate(admitidos = ifelse(is.na(admitidos), 0, admitidos),
         desligados = ifelse(is.na(desligados), 0, desligados))

# Rotatividade ----
# Transforma base em formato aceito pelo data.table para criar lags com a função shift()
rotatividade <- as.data.table(rotatividade)

# Ciando Lags
rotatividade_lags = rotatividade[, c('admitidos_anterior', 'desligados_anterior') := shift(.SD, 1, type = 'lag'),
                            .SDcols = c('admitidos', 'desligados'),
                            by = .(cbo2002ocupacao, escolaridade_rotatividade)]

# Cálculo da Rotatividade
rotatividade_calculo <- rotatividade_lags[, rotatividade := ifelse(admitidos_anterior > desligados_anterior, 
                                                                        desligados_anterior, 
                                                                        admitidos_anterior)/n_trabalhadores]

# Variáveis de CBOS Tec
rotatividade_calculo[, cbo_tecnica := fcase(cbo2002ocupacao %in% cbotec_em$cbo_em, 'tec_em', 
                                            cbo2002ocupacao %in% cbotec_sup$cbo_superior, "tec_sup", 
                                            default = "nao_tec")]

rotatividade_calculo[, `:=`(tipo = fcase(escolaridade_rotatividade == "geral" & cbo_tecnica %in% c("tec_em","tec_sup"),"Média dos trabalhadores técnicos",
                                 escolaridade_rotatividade == "geral" & cbo_tecnica == "nao_tec","Média dos trabalhadores não técnicos",
                                 escolaridade_rotatividade %in% c("analfabeto","em_incompleto","em_completo","sup_completo") & cbo_tecnica == "tec_em", "Técnicos de nível médio",
                                 escolaridade_rotatividade %in% c("analfabeto","em_incompleto","em_completo","sup_completo") & cbo_tecnica == "tec_sup", "Técnicos de nível superior",
                                 escolaridade_rotatividade %in% c("analfabeto","em_incompleto") & cbo_tecnica == "nao_tec", "Até fundamental completo",
                                 escolaridade_rotatividade == "em_completo"  & cbo_tecnica == "nao_tec","Médio completo",
                                 escolaridade_rotatividade == "sup_completo"  & cbo_tecnica == "nao_tec","Superior completo"),
                    filtro = ifelse(cbo_tecnica %in% c("tec_em","tec_sup"),"Técnico","Não Técnico"),
                    geral = ifelse(escolaridade_rotatividade %in% c("geral"),"Geral","Subgrupo"))]

rotatidade_geral <- rotatividade_calculo

rotatividade_media <- rotatividade_calculo %>% 
  group_by(anodeclarado, tipo, filtro, geral) %>% 
  summarise(rotatividade_media = mean(rotatividade, na.rm = T))

readr::write_excel_csv2(rotatividade_media, "produto/csv/base-rotatividade-media-escolaridade.csv")


rotatividade_media %>% filter(anodeclarado != 2011) %>%
  ggplot(aes(x = anodeclarado, y = rotatividade_media, col = tipo)) +
  geom_line(linewidth = 1) + geom_point(size = 2) + theme_classic() + 
  scale_color_manual(values = c("#46a462","#3e9974","#2e818d","#2b7398", "#2960a7","#2b597a","#f2cb64","#a6a6a6")) +
  labs(title = "Rotatividade no Distrito Federal (Todos)", x = "", 
       y = "Taxa de Rotatividade (%) ", col = "") +
  scale_y_continuous(label = scales::percent,
                     breaks = seq(0, .7, by = .1)) +
  scale_x_continuous(breaks = seq(2011, 2021, by = 1), expand = c(0.01,0.01)) +
  theme(legend.position = "bottom")
  

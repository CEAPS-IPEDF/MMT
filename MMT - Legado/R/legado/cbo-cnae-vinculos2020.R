# Gera tabelas para painel do MMT

# Carrega pacotes
library(data.table)
library(tidyverse)
library(readxl)

#Diretorio
diretorio <- c("P:/RECENTES/DIEPS/GEFAPS/GEFAPS/2022/Panorama da Capacitação/rais-panorama/dados/primario")

rais <- c("/rais-panorama.csv")

#Ler os dados
dados <- data.table::fread(paste0(diretorio, rais))

nomes_cnae <- read_excel('dados/estrutura_cbo/cnae_subclasse.xlsx', 
                         sheet = 1, 
                         col_names = TRUE)
nomes_cbo <- read_excel('dados/estrutura_cbo/cbo_ocupacao.xlsx', 
                         sheet = 1, 
                         col_names = TRUE)

# Filtrando pra 2020 e selecionando variáveis de interesse
base <- dados %>%
  filter(referencia == 2020) %>%
  select(cboocupacao2002, cnae20subclasse, cbo_tec)

# Recortando os dígitos das CBOs
dados[, cbo_subgrupo := as.integer(stringr::str_pad(stringr::str_remove(cboocupacao2002, '-'), width = 6, side = 'left', pad = '0'))  %/% 1e4]

# Nomes das CBOs Sub Grupo Principal ####
dados[, cbo := fcase(
  cbo_subgrupo	 ==	1	  ,	"MEMBROS DAS FORÇAS ARMADAS",
  cbo_subgrupo	 ==	2	  ,	"POLICIAIS MILITARES",
  cbo_subgrupo	 ==	3	  ,	"BOMBEIROS MILITARES",
  cbo_subgrupo	 ==	11	,	"MEMBROS SUPERIORES E DIRIGENTES DO PODER PÚBLICO",
  cbo_subgrupo	 ==	12	,	"DIRIGENTES DE EMPRESAS E ORGANIZAÇÕES (EXCETO DE INTERESSE PÚBLICO)",
  cbo_subgrupo	 ==	13	,	"DIRETORES E GERENTES EM EMPRESA DE SERVIÇOS DE SAÚDE, DA EDUCAÇÃO, OU DE SERVIÇOS CULTURAIS, SOCIAIS OU PESSOAIS",
  cbo_subgrupo	 ==	14	,	"GERENTES",
  cbo_subgrupo	 ==	20	,	"PESQUISADORES E PROFISSIONAIS POLICIENTÍFICOS",
  cbo_subgrupo	 ==	21	,	"PROFISSIONAIS DAS CIÊNCIAS EXATAS, FÍSICAS E DA ENGENHARIA",
  cbo_subgrupo	 ==	22	,	"PROFISSIONAIS DAS CIÊNCIAS BIOLÓGICAS, DA SAÚDE E AFINS",
  cbo_subgrupo	 ==	23	,	"PROFISSIONAIS DO ENSINO",
  cbo_subgrupo	 ==	24	,	"PROFISSIONAIS DAS CIÊNCIAS JURÍDICAS",
  cbo_subgrupo	 ==	25	,	"PROFISSIONAIS DAS CIÊNCIAS SOCIAIS E HUMANAS",
  cbo_subgrupo	 ==	26	,	"COMUNICADORES, ARTISTAS E RELIGIOSOS",
  cbo_subgrupo	 ==	27	,	"PROFISSIONAIS EM GASTRONOMIA",
  cbo_subgrupo	 ==	30	,	"TÉCNICOS POLIVALENTES",
  cbo_subgrupo	 ==	31	,	"TÉCNICOS DE NÍVEL MÉDIO DAS CIÊNCIAS FÍSICAS, QUÍMICAS, ENGENHARIA E AFINS",
  cbo_subgrupo	 ==	32	,	"TÉCNICOS DE NÍVEL MÉDIO DAS CIÊNCIAS BIOLÓGICAS, BIOQUÍMICAS, DA SAÚDE E AFINS",
  cbo_subgrupo	 ==	33	,	"PROFESSORES LEIGOS E DE NÍVEL MÉDIO",
  cbo_subgrupo	 ==	34	,	"TÉCNICOS DE NÍVEL MÉDIO EM SERVIÇOS DE TRANSPORTES",
  cbo_subgrupo	 ==	35	,	"TÉCNICOS DE NIVEL MÉDIO NAS CIÊNCIAS ADMINISTRATIVAS",
  cbo_subgrupo	 ==	37	,	"TÉCNICOS EM NIVEL MÉDIO DOS SERVIÇOS CULTURAIS, DAS COMUNICAÇÕES E DOS DESPORTOS",
  cbo_subgrupo	 ==	39	,	"OUTROS TÉCNICOS DE NÍVEL MÉDIO",
  cbo_subgrupo	 ==	41	,	"ESCRITURÁRIOS",
  cbo_subgrupo	 ==	42	,	"TRABALHADORES DE ATENDIMENTO AO PÚBLICO",
  cbo_subgrupo	 ==	51	,	"TRABALHADORES DOS SERVIÇOS",
  cbo_subgrupo	 ==	52	,	"VENDEDORES E PRESTADORES DE SERVIÇOS DO COMÉRCIO",
  cbo_subgrupo	 ==	61	,	"PRODUTORES NA EXPLORAÇÃO AGROPECUÁRIA",
  cbo_subgrupo	 ==	62	,	"TRABALHADORES NA EXPLORAÇÃO AGROPECUÁRIA",
  cbo_subgrupo	 ==	63	,	"PESCADORES E EXTRATIVISTAS FLORESTAIS",
  cbo_subgrupo	 ==	64	,	"TRABALHADORES DA MECANIZAÇÃO AGROPECUÁRIA E FLORESTAL",
  cbo_subgrupo	 ==	71	,	"TRABALHADORES DA INDÚSTRIA EXTRATIVA E DA CONSTRUÇÃO CIVIL",
  cbo_subgrupo	 ==	72	,	"TRABALHADORES DA TRANSFORMAÇÃO DE METAIS E DE COMPÓSITOS",
  cbo_subgrupo	 ==	73	,	"TRABALHADORES DA FABRICAÇÃO E INSTALAÇÃO ELETROELETRÔNICA",
  cbo_subgrupo	 ==	74	,	"MONTADORES DE APARELHOS E INSTRUMENTOS DE PRECISÃO E MUSICAIS",
  cbo_subgrupo	 ==	75	,	"JOALHEIROS, VIDREIROS, CERAMISTAS E AFINS",
  cbo_subgrupo	 ==	76	,	"TRABALHADORES NAS INDÚSTRIAS TÊXTIL, DO CURTIMENTO, DO VESTÚARIO E DAS ARTES GRÁFICAS",
  cbo_subgrupo	 ==	77	,	"TRABALHADORES DAS INDÚSTRIAS DE MADEIRA E DO MOBILIÁRIO",
  cbo_subgrupo	 ==	78	,	"TRABALHADORES DE FUNÇÕES TRANSVERSAIS",
  cbo_subgrupo	 ==	79	,	"TRABALHADORES DO ARTESANATO",
  cbo_subgrupo	 ==	81	,	"TRABALHADORES EM INDÚSTRIAS DE PROCESSOS CONTÍNUOS E OUTRAS INDÚSTRIAS",
  cbo_subgrupo	 ==	82	,	"TRABALHADORES DE INSTALAÇÕES SIDERÚRGICAS E DE MATERIAIS DE CONSTRUÇÃO",
  cbo_subgrupo	 ==	83	,	"TRABALHADORES DE INSTALAÇÕES E MÁQUINAS DE FABRICAÇÃO DE CELULOSE E PAPEL",
  cbo_subgrupo	 ==	84	,	"TRABALHADORES DA FABRICAÇÃO DE ALIMENTOS, BEBIDAS E FUMO",
  cbo_subgrupo	 ==	86	,	"OPERADORES DE PRODUÇÃO, CAPTAÇÃO, TRATAMENTO E DISTRIBUIÇÃO (ENERGIA, ÁGUA E UTILIDADES)",
  cbo_subgrupo	 ==	87	,	"OPERADORES DE OUTRAS INSTALAÇÕES INDUSTRIAIS",
  cbo_subgrupo	 ==	91	,	"TRABALHADORES EM SERVIÇOS DE REPARAÇÃO E MANUTENÇÃO MECÂNICA",
  cbo_subgrupo	 ==	95	,	"POLIMANTENEDORES",
  cbo_subgrupo	 ==	99	,	"OUTROS TRABALHADORES DA CONSERVAÇÃO, MANUTENÇÃO E REPARAÇÃO"
)]


# Corrijindo Cnae 2.3 da base ####

dados[dados$cnae20subclasse ==1091101, "cnae20subclasse"] <-1091100
dados[dados$cnae20subclasse ==1091102, "cnae20subclasse"] <-4721101
dados[dados$cnae20subclasse ==1610203, "cnae20subclasse"] <-1610201
dados[dados$cnae20subclasse ==1822901, "cnae20subclasse"] <-1822900
dados[dados$cnae20subclasse ==1822902, "cnae20subclasse"] <-1822900
dados[dados$cnae20subclasse ==1822999, "cnae20subclasse"] <-1822900
dados[dados$cnae20subclasse ==2013401, "cnae20subclasse"] <-2013400
dados[dados$cnae20subclasse ==2539001, "cnae20subclasse"] <-2539000
dados[dados$cnae20subclasse ==3091101, "cnae20subclasse"] <-3091100
dados[dados$cnae20subclasse ==3250709, "cnae20subclasse"] <-3250707
dados[dados$cnae20subclasse ==3511501, "cnae20subclasse"] <-3511500
dados[dados$cnae20subclasse ==3511502, "cnae20subclasse"] <-3511500
dados[dados$cnae20subclasse ==4520008, "cnae20subclasse"] <-4520001
dados[dados$cnae20subclasse ==4541206, "cnae20subclasse"] <-4541205
dados[dados$cnae20subclasse ==4713004, "cnae20subclasse"] <-4713001
dados[dados$cnae20subclasse ==4713005, "cnae20subclasse"] <-4713001
dados[dados$cnae20subclasse ==4729602, "cnae20subclasse"] <-4729699
dados[dados$cnae20subclasse ==4744006, "cnae20subclasse"] <-4744005
dados[dados$cnae20subclasse ==4751201, "cnae20subclasse"] <-4751200
dados[dados$cnae20subclasse ==4751202, "cnae20subclasse"] <-4751200
dados[dados$cnae20subclasse ==5611204, "cnae20subclasse"] <-5611202
dados[dados$cnae20subclasse ==5611205, "cnae20subclasse"] <-5611202
dados[dados$cnae20subclasse ==5812301, "cnae20subclasse"] <-5812300
dados[dados$cnae20subclasse ==5812302, "cnae20subclasse"] <-5812300
dados[dados$cnae20subclasse ==5822101, "cnae20subclasse"] <-5822100
dados[dados$cnae20subclasse ==6201501, "cnae20subclasse"] <-6201500
dados[dados$cnae20subclasse ==6201502, "cnae20subclasse"] <-6201500
dados[dados$cnae20subclasse ==6810203, "cnae20subclasse"] <-6810202
dados[dados$cnae20subclasse ==7410299, "cnae20subclasse"] <-7410202
dados[dados$cnae20subclasse ==8020001, "cnae20subclasse"] <-8020000
dados[dados$cnae20subclasse ==8020002, "cnae20subclasse"] <-8020000
dados[dados$cnae20subclasse ==8690903, "cnae20subclasse"] <-8690901
dados[dados$cnae20subclasse ==8690904, "cnae20subclasse"] <-8690999
dados[dados$cnae20subclasse ==9412001, "cnae20subclasse"] <-9412000
dados[dados$cnae20subclasse ==9412099, "cnae20subclasse"] <-9412000
dados[dados$cnae20subclasse ==9609206, "cnae20subclasse"] <-9609299
dados[dados$cnae20subclasse ==9609207, "cnae20subclasse"] <-9609203
dados[dados$cnae20subclasse ==9609208, "cnae20subclasse"] <-9609203
dados[dados$cnae20subclasse ==4713004, "cnae20subclasse"] <-4713001

# Juntando base com Nomes Cnae e CBO (para usos futuros)####
base <- left_join(dados, nomes_cnae, by = "cnae20subclasse") %>%
  arrange(cnae20subclasse)

base$cboocupacao2002 <- as.character(dados$cboocupacao2002)
nomes_cbo$cboocupacao2002 <- as.character(nomes_cbo$cboocupacao2002)
base <- left_join(base, nomes_cbo, by = "cboocupacao2002")

# Criando as matrizes de CBO-Cnae ####
## Geral
base_geral <- as.data.frame.matrix(table(base$cboocupacao2002, base$nome_cnae)) 
base_geral$tipo <- "geral"

## Técnicas
base_tec <- base[cbo_tec == 1]
base_tec <- as.data.frame.matrix(table(base_tec$cboocupacao2002, base_tec$nome_cnae))
base_tec$tipo <- "técnica"

# Juntando as Bases ####
## Igualanado o número de linhas e Colunas das Bases (para junção) 
base_geral[setdiff(names(base_tec), names(base_geral))] <- NA
base_tec[setdiff(names(base_geral), names(base_tec))] <- NA

## Juntando
join <- rbind(base_geral, base_tec)
## Nomeando primeira coluna que estava sem nome pelo processo de criação da base)
join <- rownames_to_column(join, var = "cboocupacao2002")
join$cboocupacao2002 <- as.numeric(join$cboocupacao2002)

# Inserindo nome e Códigos para as CBOs ####
## Base de códigos e nomes das CBOs
base_cbo <- unique(base[,-c(2,3,6)])

## Juntando 
base_final <- left_join(join, base_cbo, by = "cboocupacao2002")

## Ajeitando ordem das colunas
base_final <- base_final %>%
  select(tipo,cboocupacao2002,nome_cbo,cbo_subgrupo,cbo, everything()) %>%
  rename(nomes_cbo_subgrupo = cbo) 

## Inserindo número de vínculos por CBO
base_final$vinculos = rowSums(base_final[,-c(1:5)])
base_final <- base_final %>% select(vinculos, everything())

## Exportando base
readr::write_excel_csv2(base_final, 'dados/cbo-cnae-vinculos2020.csv')

base2 <- base_final %>%
  group_by()




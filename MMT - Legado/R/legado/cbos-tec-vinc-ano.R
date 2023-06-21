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

nomes_cbo <- read_excel('P:/RECENTES/DIEPS/GEFAPS/GEFAPS/2022/Panorama da Capacitação/rais-panorama/dados/estrutura_cbo/cbo_ocupacao.xlsx', 
                        sheet = 1, 
                        col_names = TRUE)

nomes_cbo$cboocupacao2002 <- as.character(nomes_cbo$cboocupacao2002)

# Recortando os dígitos das CBOs
dados[, cbo := as.integer(stringr::str_pad(stringr::str_remove(cboocupacao2002, '-'), width = 6, side = 'left', pad = '0'))  %/% 1e4]

# Nomes das CBOs Sub Grupo Principal ####
dados[, cbo_subgrupo := fcase(
  cbo	 ==	1	  ,	"MEMBROS DAS FORÇAS ARMADAS",
  cbo	 ==	2	  ,	"POLICIAIS MILITARES",
  cbo	 ==	3	  ,	"BOMBEIROS MILITARES",
  cbo	 ==	11	,	"MEMBROS SUPERIORES E DIRIGENTES DO PODER PÚBLICO",
  cbo	 ==	12	,	"DIRIGENTES DE EMPRESAS E ORGANIZAÇÕES (EXCETO DE INTERESSE PÚBLICO)",
  cbo	 ==	13	,	"DIRETORES E GERENTES EM EMPRESA DE SERVIÇOS DE SAÚDE, DA EDUCAÇÃO, OU DE SERVIÇOS CULTURAIS, SOCIAIS OU PESSOAIS",
  cbo	 ==	14	,	"GERENTES",
  cbo	 ==	20	,	"PESQUISADORES E PROFISSIONAIS POLICIENTÍFICOS",
  cbo	 ==	21	,	"PROFISSIONAIS DAS CIÊNCIAS EXATAS, FÍSICAS E DA ENGENHARIA",
  cbo	 ==	22	,	"PROFISSIONAIS DAS CIÊNCIAS BIOLÓGICAS, DA SAÚDE E AFINS",
  cbo	 ==	23	,	"PROFISSIONAIS DO ENSINO",
  cbo	 ==	24	,	"PROFISSIONAIS DAS CIÊNCIAS JURÍDICAS",
  cbo	 ==	25	,	"PROFISSIONAIS DAS CIÊNCIAS SOCIAIS E HUMANAS",
  cbo	 ==	26	,	"COMUNICADORES, ARTISTAS E RELIGIOSOS",
  cbo	 ==	27	,	"PROFISSIONAIS EM GASTRONOMIA",
  cbo	 ==	30	,	"TÉCNICOS POLIVALENTES",
  cbo	 ==	31	,	"TÉCNICOS DE NÍVEL MÉDIO DAS CIÊNCIAS FÍSICAS, QUÍMICAS, ENGENHARIA E AFINS",
  cbo	 ==	32	,	"TÉCNICOS DE NÍVEL MÉDIO DAS CIÊNCIAS BIOLÓGICAS, BIOQUÍMICAS, DA SAÚDE E AFINS",
  cbo	 ==	33	,	"PROFESSORES LEIGOS E DE NÍVEL MÉDIO",
  cbo	 ==	34	,	"TÉCNICOS DE NÍVEL MÉDIO EM SERVIÇOS DE TRANSPORTES",
  cbo	 ==	35	,	"TÉCNICOS DE NIVEL MÉDIO NAS CIÊNCIAS ADMINISTRATIVAS",
  cbo	 ==	37	,	"TÉCNICOS EM NIVEL MÉDIO DOS SERVIÇOS CULTURAIS, DAS COMUNICAÇÕES E DOS DESPORTOS",
  cbo	 ==	39	,	"OUTROS TÉCNICOS DE NÍVEL MÉDIO",
  cbo	 ==	41	,	"ESCRITURÁRIOS",
  cbo	 ==	42	,	"TRABALHADORES DE ATENDIMENTO AO PÚBLICO",
  cbo	 ==	51	,	"TRABALHADORES DOS SERVIÇOS",
  cbo	 ==	52	,	"VENDEDORES E PRESTADORES DE SERVIÇOS DO COMÉRCIO",
  cbo	 ==	61	,	"PRODUTORES NA EXPLORAÇÃO AGROPECUÁRIA",
  cbo	 ==	62	,	"TRABALHADORES NA EXPLORAÇÃO AGROPECUÁRIA",
  cbo	 ==	63	,	"PESCADORES E EXTRATIVISTAS FLORESTAIS",
  cbo	 ==	64	,	"TRABALHADORES DA MECANIZAÇÃO AGROPECUÁRIA E FLORESTAL",
  cbo	 ==	71	,	"TRABALHADORES DA INDÚSTRIA EXTRATIVA E DA CONSTRUÇÃO CIVIL",
  cbo	 ==	72	,	"TRABALHADORES DA TRANSFORMAÇÃO DE METAIS E DE COMPÓSITOS",
  cbo	 ==	73	,	"TRABALHADORES DA FABRICAÇÃO E INSTALAÇÃO ELETROELETRÔNICA",
  cbo	 ==	74	,	"MONTADORES DE APARELHOS E INSTRUMENTOS DE PRECISÃO E MUSICAIS",
  cbo	 ==	75	,	"JOALHEIROS, VIDREIROS, CERAMISTAS E AFINS",
  cbo	 ==	76	,	"TRABALHADORES NAS INDÚSTRIAS TÊXTIL, DO CURTIMENTO, DO VESTÚARIO E DAS ARTES GRÁFICAS",
  cbo	 ==	77	,	"TRABALHADORES DAS INDÚSTRIAS DE MADEIRA E DO MOBILIÁRIO",
  cbo	 ==	78	,	"TRABALHADORES DE FUNÇÕES TRANSVERSAIS",
  cbo	 ==	79	,	"TRABALHADORES DO ARTESANATO",
  cbo	 ==	81	,	"TRABALHADORES EM INDÚSTRIAS DE PROCESSOS CONTÍNUOS E OUTRAS INDÚSTRIAS",
  cbo	 ==	82	,	"TRABALHADORES DE INSTALAÇÕES SIDERÚRGICAS E DE MATERIAIS DE CONSTRUÇÃO",
  cbo	 ==	83	,	"TRABALHADORES DE INSTALAÇÕES E MÁQUINAS DE FABRICAÇÃO DE CELULOSE E PAPEL",
  cbo	 ==	84	,	"TRABALHADORES DA FABRICAÇÃO DE ALIMENTOS, BEBIDAS E FUMO",
  cbo	 ==	86	,	"OPERADORES DE PRODUÇÃO, CAPTAÇÃO, TRATAMENTO E DISTRIBUIÇÃO (ENERGIA, ÁGUA E UTILIDADES)",
  cbo	 ==	87	,	"OPERADORES DE OUTRAS INSTALAÇÕES INDUSTRIAIS",
  cbo	 ==	91	,	"TRABALHADORES EM SERVIÇOS DE REPARAÇÃO E MANUTENÇÃO MECÂNICA",
  cbo	 ==	95	,	"POLIMANTENEDORES",
  cbo	 ==	99	,	"OUTROS TRABALHADORES DA CONSERVAÇÃO, MANUTENÇÃO E REPARAÇÃO"
)]


# Corrijindo erros de Cnae ####
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


# Criação da Base ####
dados$cboocupacao2002 <- as.character(dados$cboocupacao2002)

base_cbo <- dados %>%
  select(cboocupacao2002, cbo_subgrupo) %>%
  left_join(nomes_cbo, by = "cboocupacao2002") %>%
  unique()

base_geral <- dados %>%
  group_by(referencia, cboocupacao2002) %>%
  summarise(vinculos = n(),
            tipo = "geral")

base_tec <- dados %>%
  filter(cbo_tec == 1) %>%
  group_by(referencia, cboocupacao2002) %>%
  summarise(vinculos = n(),
            tipo = "técnica")

join <- rbind(base_geral, base_tec)

base_final <- left_join(join, nomes_cbo, by = "cboocupacao2002")

## Exportando base
readr::write_excel_csv2(base_final, 'dados/cbo-vinculos2020.csv')




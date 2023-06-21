# Carregando pacotes
library(data.table)

# Carregando dados
rais = data.table::fread('dados/primario/rais-panorama.csv', encoding = "UTF-8")
cbos = data.table::fread('dados/CBO2002 - Ocupacao.csv', encoding = "Latin-1")

# Limpeza ----
## Limpando CBO ----
cbos = cbos[, .(cboocupacao2002 = CODIGO, nome_cbo = TITULO)]

## Dados 2020 ----
dados_2020 = rais[referencia == 2020, .(vinculos_2020 = .N, mean_rendimento_2020 = mean(salario_dez_defl, na.rm = TRUE), mean_salario_hora_2020 = mean(salario_hora, na.rm = TRUE)), by = cboocupacao2002]

## Dados 2011  ----
### Para calcular variação
dados_2011 = rais[referencia == 2011, .(vinculos_2011 = .N, mean_rendimento_2011 = mean(salario_dez_defl, na.rm = TRUE), mean_salario_hora_2011 = mean(salario_hora, na.rm = TRUE)), by = cboocupacao2002]

## Dados 2011-2020 ----
dados_periodo = rais[,.(vinculos = .N, mean_rendimento = mean(vlremundezembronom, na.rm = TRUE)), by = .(referencia, cboocupacao2002)][,.(mediana_vinculos = median(vinculos, na.rm = TRUE),  median_rendimento = mean(mean_rendimento, na.rm = TRUE)), by = cboocupacao2002]

## Unindo dados ----
dados = dplyr::left_join(dados_periodo,
                 dplyr::left_join(dados_2020, dados_2011, by = "cboocupacao2002"), 
                 by = "cboocupacao2002")

dados[, cboocupacao2002 := as.integer(stringr::str_remove(cboocupacao2002, '-'))]

dados = dplyr::left_join(cbos, dados, by = "cboocupacao2002")

## Calculando variação ----
dados = data.table(dados)
dados[, `:=`(variacao_rendimento = mean_rendimento_2020/mean_rendimento_2011,
             variacao_vinculos = vinculos_2020/vinculos_2011)]

# Recortando os dígitos das CBOs
dados[, cbo := as.integer(stringr::str_pad(stringr::str_remove(cboocupacao2002, '-'), width = 6, side = 'left', pad = '0'))  %/% 1e4]


dados$codigo_cbo <- dados$cbo

# Nomes das CBOs Sub Grupo Principal ####
dados[, cbo := fcase(
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

dados$cboocupacao2002 <- as.character(dados$cboocupacao2002)
dados$codigo_cbo <- as.character(dados$codigo_cbo)

# Salvando resultados
readr::write_excel_csv2(dados, 'cbos-info-atualizado.csv')

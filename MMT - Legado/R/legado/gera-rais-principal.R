# Cria base de dados da RAIS com as considerações do Panorama

# Configuração inicial ----
## Carrega pacotes ----
library(keyring)
library(dplyr)
library(odbc)
library(stringr)

## Carrega arquivos ----
cbotecnica_nivel_medio = data.table::fread("dados/cbotecnica_nivelmedio.csv", header = TRUE)
cbotecnica_nivel_superior = data.table::fread("dados/cbotecnica_nivelsuperior.csv", header = TRUE)
ocupacoes_protegidas = data.table::fread("dados/ocupacoes_protegidas.csv", header = TRUE)
eixos = lapply(paste0('dados/', dir('dados')[str_detect(dir('dados'), "Eixo[0-9]{1,2}\\.")]), function(x) {data.table::fread(x, encoding = "UTF-8", sep = ';')})
eixos_superior = lapply(paste0('dados/', dir('dados')[str_detect(dir('dados'), "Eixo[0-9]{1,2}\\_")]), function(x) {data.table::fread(x)})


## Colocan omes em eixos
names(eixos) = vapply(eixos, function(x){names(x)[[2]]}, 'eixo')
names(eixos_superior) = vapply(eixos_superior, function(x){names(x)[[2]]}, 'eixo')

## Carrega arquivo do inpc do df ----

serie_inpc = readxl::read_excel("dados/serie_inpcDF.xlsx")

# Conecta a base de dados ----
## Cria usuário e senha para a conexão ao DB_CODEPLAN, sem deixar registrado a senha de acesso ----
key_set(service = "usuario", username = "36420")

## Cria conexão com o banco de dados DB_CODEPLAN ----
db <- DBI::dbConnect(odbc(),'db_codeplan', 
                     uid = keyring::key_list('usuario')[1,2], 
                     pwd = keyring::key_get('usuario', username =  '36420'))

## Carrega dados ----
rais2019 <- DBI::dbGetQuery(db, "SELECT referencia, vinculoativo3112, tipovinculo, escolaridade, sexotrabalhador, racacor, vlremundezembronom, vlremundezembrosm, tiposalario, tempoemprego, qtdhoracontr, vlsalariocontratual, cboocupacao2002, cnae20subclasse, idade, indtrabintermitente  FROM rais_id.Vinc WHERE municipio = 530010 and referencia > 2010 and vinculoativo3112 = 1")

rais2020 <- DBI::dbGetQuery(db,"SELECT referencia, vinculoativo3112, tipovinculo, escolaridade, sexotrabalhador, racacor, vlremundezembronom, vlremundezembrosm, tiposalario, tempoemprego, qtdhoracontr, vlsalariocontratual, cboocupacao2002, cnae20subclasse, idade, indtrabintermitente  FROM rais_id.vinc_2020 WHERE municipio = 530010 and referencia = 2020 and vinculoativo3112 = 1")

### Une as rais
dados = rbind(rais2019, rais2020)
rm(rais2019, rais2020)

# Limpeza inpc inicial ----
serie_inpc = serie_inpc[serie_inpc$ano %in% 2010:2020,]

serie_inpc = aggregate(serie_inpc$inpc, list(referencia = serie_inpc$ano), function(x) (prod(1 + x)))

serie_inpc$inpc_acum = rev(cumprod(rev(serie_inpc$x)))

names(serie_inpc)[[2]] = 'inpc_anual'

# Limpa base ----
## Une base com inpc ----
dados = merge(dados, serie_inpc, all.x = TRUE)

## Filtra base ----
dados = dados[dados$vlremundezembrosm >= 0.5 & dados$vlremundezembrosm <= 200,]

## Calcula novas variaveis ----
dados$salario_dez_defl = dados$vlremundezembronom * dados$inpc_acum
dados$horas_mensais = dados$qtdhoracontr*4
dados$salario_hora = ifelse(dados$horas_mensais == 0,
                            NA_real_,
                            dados$salario_dez_defl/dados$horas_mensais)

## Filtra base de dados
dados = dados[dados$qtdhoracontr > 10 & dados$idade >= 18,]

## Dummys cbos
dados$cbo_tec_em_complet = ifelse(dados$cboocupacao2002 %in% cbotecnica_nivel_medio$cbo_em & dados$escolaridade %in% c(7:11) & dados$tipovinculo!='55', 1,0)
dados$cbo_tec_sup = ifelse(dados$cboocupacao2002 %in% cbotecnica_nivel_superior$cbo_superior & dados$escolaridade %in% c(9:11) & dados$tipovinculo!='55', 1,0)
dados$cbo_tec = ifelse(dados$cbo_tec_em_complet ==1 | dados$cbo_tec_sup==1, 1,0)

### Dummy vinculo

dados$celetistas = ifelse(dados$tipovinculo %in% c(10,15,20,25,60,65,70,75), 1,0)
dados$estatutarios = ifelse(dados$tipovinculo %in% c(30,31,35), 1,0)
dados$aprendiz = ifelse(dados$tipovinculo=='55', 1,0)
dados$outros = ifelse(dados$tipovinculo %in% c(40,50,80,90,95,96,97,-1), 1, 0)

### Dummy escolaridade

dados$medio_incompleto = ifelse(dados$escolaridade %in% c(2,3,4,5,6), 1,0)
dados$medio_completo = ifelse(dados$escolaridade %in% c(7,8), 1,0)
dados$superior_completo = ifelse(dados$escolaridade %in% c(9,10,11), 1, 0)
dados$analfabeto = ifelse(dados$escolaridade=='1', 1,0)

### retirando da base os vínculos em ocupações protegidas (militares) ###

cbos_protegidas = ocupacoes_protegidas$cbo_protegidas[!is.na(ocupacoes_protegidas$cbo_protegidas)]

cbos_protegidas = formatC(cbos_protegidas, width=6, format = "d", flag="0")

dados = dados[!dados$cboocupacao2002 %in% cbos_protegidas,]

### Dummy eixo ----
dados$eixo_amb_saude_em = ifelse(dados$cboocupacao2002 %in% eixos$`Eixo de Ambiente e Saúde`$`Eixo de Ambiente e Saúde` & dados$cbo_tec_em_complet == 1, 1,0)

dados$eixo_conteprocessos_em = ifelse(dados$cboocupacao2002 %in% eixos$`Eixo de Controle e Processos Industriais`$`Eixo de Controle e Processos Industriais` & dados$cbo_tec_em_complet == 1, 1, 0)

dados$eixo_desedusoc_em = ifelse(dados$cboocupacao2002 %in% eixos$`Eixo de Desenvolvimento Educacional e Social`$`Eixo de Desenvolvimento Educacional e Social` & dados$cbo_tec_em_complet == 1, 1,0)

dados$eixo_negocios_em = ifelse(dados$cboocupacao2002 %in% eixos$`Eixo de Gestão e Negócios`$`Eixo de Gestão e Negócios` & dados$cbo_tec_em_complet == 1,1 ,0)

dados$eixo_infoecomunic_em = ifelse(dados$cboocupacao2002 %in% eixos$`Eixo de Informação e Comunicação`$`Eixo de Informação e Comunicação` & dados$cbo_tec_em_complet == 1, 1, 0)

dados$eixo_infraestrutura_em = ifelse(dados$cboocupacao2002 %in% eixos$`Eixo de Infraestrutura`$`Eixo de Infraestrutura` & dados$cbo_tec_em_complet == 1, 1, 0)

dados$eixo_prodaliment_em = ifelse(dados$cboocupacao2002 %in% eixos$`Eixo de Produção Alimentícia`$`Eixo de Produção Alimentícia` & dados$cbo_tec_em_complet == 1, 1, 0)

dados$eixo_prodcult_em = ifelse(dados$cboocupacao2002 %in% eixos$`Eixo de Produção Cultural e Design`$`Eixo de Produção Cultural e Design` & dados$cbo_tec_em_complet == 1, 1, 0)

dados$eixo_prodindust_em = ifelse(dados$cboocupacao2002 %in% eixos$`Eixo de Produção Industrial`$`Eixo de Produção Industrial` & dados$cbo_tec_em_complet == 1, 1, 0)

dados$eixo_recnaturais_em = ifelse(dados$cboocupacao2002 %in% eixos$`Eixo de Recursos Naturais`$`Eixo de Recursos Naturais` & dados$cbo_tec_em_complet == 1, 1, 0)

dados$eixo_seguranca_em = ifelse(dados$cboocupacao2002 %in% eixos$`Eixo de Segurança`$`Eixo de Segurança` & dados$cbo_tec_em_complet == 1, 1, 0)

dados$eixo_hospelazer_em = ifelse(dados$cboocupacao2002 %in% eixos$`Eixo de Turismo, Hospitalidade e Lazer`$`Eixo de Turismo, Hospitalidade e Lazer` & dados$cbo_tec_em_complet == 1, 1, 0)

dados$eixo_militar_em = ifelse(dados$cboocupacao2002 %in% eixos$`Eixo Militar`$`Eixo Militar` & dados$cbo_tec_em_complet == 1, 1, 0)

### Dummy eixo superior ----
dados$eixo_ambesaude_sup = ifelse(dados$cboocupacao2002 %in% eixos_superior$`Ambiente_saude`$`Ambiente_saude` & dados$cbo_tec_sup == 1, 1,0)

dados$eixo_conteprocessos_sup = ifelse(dados$cboocupacao2002 %in% eixos_superior$`Controle e Processos Industriais`$`Controle e Processos Industriais` & dados$cbo_tec_sup == 1, 1, 0)

dados$eixo_negocios_sup = ifelse(dados$cboocupacao2002 %in% eixos_superior$`gestao_neg`$`gestao_neg` & dados$cbo_tec_sup == 1,1 ,0)

dados$eixo_infoecomunic_sup = ifelse(dados$cboocupacao2002 %in% eixos_superior$`info_comu`$`info_comu` & dados$cbo_tec_sup == 1, 1, 0)

dados$eixo_infraestrutura_sup = ifelse(dados$cboocupacao2002 %in% eixos_superior$`Infraestrutura`$`Infraestrutura` & dados$cbo_tec_sup == 1, 1, 0)

dados$eixo_prodaliment_sup = ifelse(dados$cboocupacao2002 %in% eixos_superior$`prod_alim`$`prod_alim` & dados$cbo_tec_sup == 1, 1, 0)

dados$eixo_prodcult_sup = ifelse(dados$cboocupacao2002 %in% eixos_superior$`prod_cul_design`$`prod_cul_design` & dados$cbo_tec_sup == 1, 1, 0)

dados$eixo_prodindust_sup = ifelse(dados$cboocupacao2002 %in% eixos_superior$`prod_ind`$`prod_ind` & dados$cbo_tec_sup == 1, 1, 0)

dados$eixo_recnaturais_sup = ifelse(dados$cboocupacao2002 %in% eixos_superior$`Recursos Naturais`$`Recursos Naturais` & dados$cbo_tec_sup == 1, 1, 0)

dados$eixo_seguranca_sup = ifelse(dados$cboocupacao2002 %in% eixos_superior$`seguranca`$`seguranca` & dados$cbo_tec_sup == 1, 1, 0)

dados$eixo_hospelazer_sup = ifelse(dados$cboocupacao2002 %in% eixos_superior$`Tur_hosp_lazer`$`Tur_hosp_lazer` & dados$cbo_tec_sup == 1, 1, 0)

dados$eixo_militar_sup = ifelse(dados$cboocupacao2002 %in% eixos_superior$`Eixo Militar`$`Eixo Militar` & dados$cbo_tec_sup == 1, 1, 0)

# Salva resultados
data.table::fwrite(dados, file = 'dados/rais-panorama.csv', sep = ';')

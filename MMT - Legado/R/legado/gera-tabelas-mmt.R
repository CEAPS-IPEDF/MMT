# Gera tabelas para painel do MMT

# Carrega pacotes
library(data.table)

# Carregando pacotes
library(data.table)
library(ggplot2)

# Carregando dados
dados = fread('dados/rais-panorama.csv')

# Definindo cnaes
dados[, cnae := as.integer(stringr::str_pad(cnae20subclasse, width = 7, side = 'left', pad = '0'))  %/% 1e5]

dados[, cbo := as.integer(stringr::str_pad(stringr::str_remove(cboocupacao2002, '-'), width = 6, side = 'left', pad = '0'))  %/% 1e5]

dados[, cbo := fcase(
  cbo == 0, 'Membros das forças armadas, policiais e bombeiros militares',
  cbo == 1, 'Membros superiores do poder público, dirigentes de organizações de interesse público e de empresas, gerentes',
  cbo == 2, 'Profissionais das ciências e das artes',
  cbo == 3, 'Técnicos de nivel médio',
  cbo == 6, 'Trabalhadores agropecuários, florestais e da pesca',
  cbo == 7, 'Trabalhadores da produção de bens e serviços industriais',
  cbo == 8, 'Trabalhadores da produção de bens e serviços industriais',
  cbo == 4, 'Trabalhadores de serviços administrativos',
  cbo == 5, 'Trabalhadores dos serviços, vendedores do comércio em lojas e mercados',
  cbo == 9, 'Trabalhadores em serviços de reparação e manutenção'
)]

dados[, cnae := fcase(
  cnae %in% 1:3, 'AGRICULTURA, PECUÁRIA, PRODUÇÃO FLORESTAL, PESCA E AQÜICULTURA',
  cnae %in% 5:9, 'INDÚSTRIAS EXTRATIVAS',
  cnae %in% 10:33, 'INDÚSTRIAS DE TRANSFORMAÇÃO',
  cnae %in% 35, 'ELETRICIDADE E GÁS',
  cnae %in% 36:39, 'ÁGUA, ESGOTO, ATIVIDADES DE GESTÃO DE RESÍDUOS E DESCONTAMINAÇÃO',
  cnae %in% 41:43, "CONSTRUÇÃO",
  cnae %in% 45:47, 'COMÉRCIO; REPARAÇÃO DE VEÍCULOS AUTOMOTORES E MOTOCICLETAS',
  cnae %in% 49:53, 'TRANSPORTE, ARMAZENAGEM E CORREIO',
  cnae %in% 55:56, 'ALOJAMENTO E ALIMENTAÇÃO',
  cnae %in% 58:63, 'INFORMAÇÃO E COMUNICAÇÃO',
  cnae %in% 64:66, 'ATIVIDADES FINANCEIRAS, DE SEGUROS E SERVIÇOS RELACIONADOS',
  cnae %in% 68, 'ATIVIDADES IMOBILIÁRIAS',
  cnae %in% 69:75, 'ATIVIDADES PROFISSIONAIS, CIENTÍFICAS E TÉCNICAS',
  cnae %in% 77:82, 'ATIVIDADES ADMINISTRATIVAS E SERVIÇOS COMPLEMENTARES',
  cnae %in% 84, 'ADMINISTRAÇÃO PÚBLICA, DEFESA E SEGURIDADE SOCIAL',
  cnae %in% 85, 'EDUCAÇÃO',
  cnae %in% 86:88, 'SAÚDE HUMANA E SERVIÇOS SOCIAIS',
  cnae %in% 90:93, 'ARTES, CULTURA, ESPORTE E RECREAÇÃO',
  cnae %in% 94:96, 'OUTRAS ATIVIDADES DE SERVIÇOS',
  cnae %in% 97, 'SERVIÇOS DOMÉSTICOS',
  cnae %in% 99, 'ORGANISMOS INTERNACIONAIS E OUTRAS INSTITUIÇÕES EXTRATERRITORIAIS',
  default = 'erro'
)]

dados[, tipo_educ := fcase(
  cbo_tec == 1, "Técnico",
  superior_completo == 1 & cbo_tec != 1, 'Superior',
  medio_completo == 1 & cbo_tec != 1, 'Médio',
  default = 'Outros'
)]

dados$tipo_educ = factor(dados$tipo_educ, levels = c("Outros", "Médio", "Técnico", "Superior"), ordered = TRUE)

dados[, tipo_emprego := fcase(
  estatutarios  == 1, "Estatutário",
  celetistas  == 1, 'Celetista',
  default = 'Outros'
)]

# Tabela para o MMT
## Definindo tecnicos nao tecnicos
vinculos_ano = rbind(
  dados[, .(vinculos = .N), by = .(cbo_tec, referencia, tipo_emprego)][, cbo_tec := ifelse(cbo_tec == 1, 'Técninas', 'Não Técnicas')],
   dados[, .(vinculos = .N), by = .(cbo_tec, referencia)][, `:=`(cbo_tec = ifelse(cbo_tec == 1, 'Técninas', 'Não Técnicas'), tipo_emprego = "Total")])
                     
readr::write_excel_csv2(vinculos_ano, 'dados/mmt-vinculos-ano.csv')

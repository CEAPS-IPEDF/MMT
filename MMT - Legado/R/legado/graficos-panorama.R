# Carregando pacotes
library(data.table)
library(ggplot2)

# Carregando dados
dados = fread('dados/rais-panorama.csv')
anos_referencia = 10

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

# Vinculo tecnico-nao tecnico por ano

## Definindo tecnicos nao tecnicos
vinculos_ano = dados[, .(vinculos = .N), by = .(cbo_tec, referencia)][, cbo_tec := ifelse(cbo_tec == 1, 'Técninas', 'Não Técnicas')]

vinculos_ano_jovens = dados[idade %in% 18:27, .(vinculos = .N), by = .(cbo_tec, referencia)][, cbo_tec := ifelse(cbo_tec == 1, 'Técninas', 'Não Técnicas')]

vinculos_ano[, prop := vinculos/sum(vinculos), by = referencia]
vinculos_ano_jovens[, prop := vinculos/sum(vinculos), by = referencia]

## Grafico
graf_vinculo_ano = ggplot(vinculos_ano, aes(x = referencia, y = prop, fill = cbo_tec)) +
  geom_col() +
  scale_x_continuous(breaks = 2011:2020) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c('#4079bc', '#00a85a')) +
  geom_text(aes(x = referencia, y = prop,  group = cbo_tec, label = scales::percent(prop, accuracy = 1)), position = position_stack(vjust = .5)) +
  labs(x = '', y = "", fill = '') +
  theme_classic() +
  theme(text = element_text(size = 20), legend.position = 'bottom')

# Tipo de vinculo
## Definindo tipo de vinculos
tipo_tec_ano = dados[cbo_tec == 1,][, tipo := fcase(estatutarios == 1, 'Estaturários', celetistas == 1, "Celetistas", default = "Outros")]

tipo_tec_ano = tipo_tec_ano[,.(vinculos = .N), by = .(tipo, referencia)][, prop := vinculos/sum(vinculos), by = referencia]

## Grafico
graf_tipo_ano = ggplot(tipo_tec_ano, aes(x = referencia, y = prop, color = tipo)) +
  geom_line() +
  geom_point(show.legend = FALSE) +
  scale_x_continuous(breaks = 2011:2020) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_color_manual(values = c('#00a85a', '#4079bc', '#333333')) +
  geom_text(aes(x = referencia, y = prop +.05,  group = tipo, label = scales::percent(prop, accuracy = .01)), size = 5, show.legend = FALSE) +
  labs(x = '', y = "", color = '') +
  theme_classic() +
  theme(text = element_text(size = 20), legend.position = 'bottom')

# Vínculos por CNAE
##  Corringo niveis
vinculos_cnae = dados[, .(vinculos = .N/anos_referencia), by = cnae][, cnae := forcats::fct_lump(cnae, n = 10, w = vinculos, other_level = "Outros")][, .(vinculos = sum(vinculos)), by = cnae]

levels(vinculos_cnae$cnae) = stringr::str_wrap(stringr::str_to_sentence(levels(vinculos_cnae$cnae)), width = 30)

vinculos_cnae[, prop := vinculos/sum(vinculos)]

## Gráfico
graf_cnae = ggplot(vinculos_cnae, aes(x = forcats::fct_reorder(cnae, prop), y = prop)) +
  geom_col(fill = '#00a85a') +
  coord_flip() +
  geom_text(aes(label = scales::percent(prop, accuracy = .01, decimal.mark = ',', big.mark = '.'), y = prop + .0175), size = 6) +
  scale_y_continuous(labels = scales::percent_format(big.mark = '.', decimal.mark = ',')) +
  labs(y = '', x = "") +
  theme_classic() +
  theme(text = element_text(size = 20))

# Vínculos por CBO
##  Corringo niveis
vinculos_cbo = dados[, .(vinculos = .N/anos_referencia), by = cbo]
vinculos_cbo = vinculos_cbo[, cbo :=  stringr::str_wrap(stringr::str_to_sentence(cbo), width = 30)]

vinculos_cbo[, prop := vinculos/sum(vinculos)]

## Gráfico
graf_cbo = ggplot(vinculos_cbo, aes(x = forcats::fct_reorder(cbo, prop), y = prop)) +
  geom_col(fill = '#00a85a') +
  coord_flip() +
  geom_text(aes(label = scales::percent(prop, accuracy = .01, decimal.mark = ',', big.mark = '.'), y = prop + .0175), size = 6) +
  scale_y_continuous(labels = scales::percent_format(big.mark = '.', decimal.mark = ','), limits = c(0, .33)) +
  labs(y = '', x = "") +
  theme_classic() +
  theme(text = element_text(size = 20))

# CNAE por escolaridade
principais_cnaes = dados[referencia == 2020, .(vinculos = .N), by = .(cnae)][vinculos >20000 ,]$cnae

grupos_cnae = dados[ cnae %in% principais_cnaes, .(vinculos = .N/anos_referencia), by = .(tipo_educ, cnae)]

grupos_cnae[, prop := vinculos/sum(vinculos), by = cnae]
grupos_cnae[, cnae := stringr::str_wrap(cnae, 40), by = cnae]


## Gráfico
graf_grupos_cnae = ggplot(grupos_cnae, aes(x = tipo_educ, y = prop, fill = tipo_educ)) +
  geom_col() +
  facet_wrap(~cnae) +
  scale_fill_manual(values = c( '#4079bc', '#999999','#e6bb2e', '#00a85a')) +
  geom_text(aes(label = scales::percent(prop, accuracy = .1, decimal.mark = ',', big.mark = '.'), y = prop + .07), size = 5) +
  scale_y_continuous(labels = scales::percent_format(big.mark = '.', decimal.mark = ','), limits = c(0, .8)) +
  labs(y = '', x = "", fill = "Nível Educacional:") +
  theme_classic() +
  theme(text = element_text(size = 14), legend.position = 'bottom')

# CNAE por escolaridade
grupos_cbo = dados[, .(vinculos = .N/anos_referencia), by = .(tipo_educ, cbo)]

grupos_cbo[, prop := vinculos/sum(vinculos), by = cbo]
grupos_cbo[, cbo := stringr::str_wrap(cbo, 40), by = cbo]

## Gráfico
graf_grupos_cbo = ggplot(grupos_cbo, aes(x = tipo_educ, y = prop, fill = tipo_educ)) +
  geom_col() +
  facet_wrap(~cbo) +
  scale_fill_manual(values = c( '#4079bc', '#999999','#e6bb2e', '#00a85a')) +
  geom_text(aes(label = scales::percent(prop, accuracy = .1, decimal.mark = ',', big.mark = '.'), y = prop + .07), size = 5) +
  scale_y_continuous(labels = scales::percent_format(big.mark = '.', decimal.mark = ','), limits = c(0, .85)) +
  labs(y = '', x = "", fill = "Nível Educacional:") +
  theme_classic() +
  theme(text = element_text(size = 14), legend.position = 'bottom')

# Jovens

# CNAE por escolaridade
principais_cnaes_jovens = dados[, .(vinculos = .N/anos_referencia), by = .(cnae)][vinculos >23000 ,]$cnae

grupos_cnae_jovens = dados[idade %in% 18:27 & cnae %in% principais_cnaes, .(vinculos = .N/anos_referencia), by = .(tipo_educ, cnae)]

grupos_cnae_jovens[, prop := vinculos/sum(vinculos), by = cnae]
grupos_cnae_jovens[, cnae := stringr::str_wrap(cnae, 40), by = cnae]


## Gráfico
graf_grupos_cnae_jovens = ggplot(grupos_cnae_jovens, aes(x = tipo_educ, y = prop, fill = tipo_educ)) +
  geom_col() +
  facet_wrap(~cnae) +
  scale_fill_manual(values = c( '#4079bc', '#999999','#e6bb2e', '#00a85a')) +
  geom_text(aes(label = scales::percent(prop, accuracy = .1, decimal.mark = ',', big.mark = '.'), y = prop + .07), size = 5) +
  scale_y_continuous(labels = scales::percent_format(big.mark = '.', decimal.mark = ','), limits = c(0, .8)) +
  labs(y = '', x = "", fill = "Nível Educacional:") +
  theme_classic() +
  theme(text = element_text(size = 14), legend.position = 'bottom')

# CNAE por escolaridade
grupos_cbo_jovens = dados[idade %in% 18:27, .(vinculos = .N/anos_referencia), by = .(tipo_educ, cbo)]

grupos_cbo_jovens[, prop := vinculos/sum(vinculos), by = cbo]
grupos_cbo_jovens[, cbo := stringr::str_wrap(cbo, 40), by = cbo]


## Gráfico
graf_grupos_cbo_jovens = ggplot(grupos_cbo_jovens, aes(x = tipo_educ, y = prop, fill = tipo_educ)) +
  geom_col() +
  facet_wrap(~cbo) +
  scale_fill_manual(values = c( '#4079bc', '#999999','#e6bb2e', '#00a85a')) +
  geom_text(aes(label = scales::percent(prop, accuracy = .1, decimal.mark = ',', big.mark = '.'), y = prop + .07), size = 5) +
  scale_y_continuous(labels = scales::percent_format(big.mark = '.', decimal.mark = ','), limits = c(0, .85)) +
  labs(y = '', x = "", fill = "Nível Educacional:") +
  theme_classic() +
  theme(text = element_text(size = 14), legend.position = 'bottom')

# Cbos e cnaes
cbo_bienio = dados[referencia %in% 2019:2020, .(vinculos = .N/anos_referencia), by = .(referencia, cbo)][, prop := vinculos/sum(vinculos), by = referencia]
cbo_bienio[, cbo := stringr::str_wrap(cbo, 40), by = cbo]
cnae_bienio = dados[referencia %in% 2019:2020, .(vinculos = .N/anos_referencia), by = .(referencia, cnae)][, prop := vinculos/sum(vinculos), by = referencia]
cnae_bienio[, cnae := stringr::str_wrap(stringr::str_to_sentence(cnae), 40), by = cnae]

cnae_bienio<-cnae_bienio %>% mutate(cnae = case_when(prop<0.01 ~"Outro",TRUE~cnae))

cnae_bienio<- cnae_bienio %>% group_by(cnae,referencia) %>% summarise (cnae=cnae,prop=sum(prop))

graf_cbo_bienio = ggplot(cbo_bienio, aes(x = forcats::fct_reorder(factor(cbo), prop), y = prop, fill = factor(referencia))) +
  geom_col(position = position_dodge()) +
  scale_fill_manual(values = c('#4079bc', '#00a85a')) +
  coord_flip() +
  geom_text(aes(label = scales::percent(prop, accuracy = .1, decimal.mark = ',', big.mark = '.'), y = prop + .02), size = 5, position = position_dodge(width = 1)) +
  scale_y_continuous(labels = scales::percent_format(big.mark = '.', decimal.mark = ','), limits = c(0, .35)) +
  labs(y = '', x = "", fill = "Ano:") +
  theme_classic() +
  theme(text = element_text(size = 14), legend.position = 'bottom')

graf_cnae_bienio = ggplot(cnae_bienio, aes(x = forcats::fct_reorder(factor(cnae), prop), y = prop, fill = factor(referencia))) +
  geom_col(position = position_dodge()) +
  scale_fill_manual(values = c('#4079bc', '#00a85a')) +
  coord_flip() +
  geom_text(aes(label = scales::percent(prop, accuracy = .1, decimal.mark = ',', big.mark = '.'), y = prop + .02), size = 5, position = position_dodge(width = 1)) +
  scale_y_continuous(labels = scales::percent_format(big.mark = '.', decimal.mark = ','), limits = c(0, .35)) +
  labs(y = '', x = "", fill = "Ano:") +
  theme_classic() +
  theme(text = element_text(size = 14), legend.position = 'bottom')

# Salva resultados
graf_tipo_ano
purrr::walk2(list(graf_tipo_ano, graf_vinculo_ano, graf_cnae, graf_cbo_bienio),
             c('graficos/tipo-ano.png', 'graficos/vinculo-ano.png', 'graficos/cnae.png', 'graficos/cbo-bienio.png'),
             ~ggsave(plot = .x, filename = .y, scale = 1.5))

purrr::walk2(list(graf_grupos_cnae, graf_grupos_cbo, graf_grupos_cnae_jovens, graf_grupos_cbo_jovens, graf_cnae_bienio),
             c('graficos/cnae-grupos.png', 'graficos/cbo-grupos.png', 'graficos/cnae-grupos_jovens.png', 'graficos/cbo-grupos_jovens.png', 'graficos/cnae-bienio.png'),
             ~ggsave(plot = .x, filename = .y, scale = 2))            
             
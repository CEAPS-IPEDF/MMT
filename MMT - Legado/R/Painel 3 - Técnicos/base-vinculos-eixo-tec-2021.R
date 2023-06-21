#### Scrip gera dados de variação de salário e de vículos no período 2012 - 2021 e no período 2012 - 2021
### Rbind com ocupações gerais e técnicas utilizando medianas na fórmula de cálculo da variação

# Carregando pacotes ----
library(data.table)
library(dplyr)


rstudioapi::writeRStudioPreference("data_viewer_max_columns", 300L)

# Carregando dados ----
#rais
rais = data.table::fread('dados/rais-panorama-2021.csv',encoding = "Latin-1")

dados <- rais

## Definindo função ----
calcula_eixo = function(nome){
  temp = copy(dados)
  
  temp = temp[get(nome) == 1,]
  
  conta = temp[referencia == 2021, .(vinculos = .N), by = cboocupacao2002]
  
  conta[, eixo := nome]
}

## Definindo Espaço do loop (dummies de eixo) ----
nomes_superior = names(dados)[46:57]
nomes_medio = names(dados)[33:45]

# Loop ----
dados_eixo = rbind(
  rbindlist(lapply(nomes_medio, calcula_eixo)),
  rbindlist(lapply(nomes_superior, calcula_eixo))
)

# Diferenciando Nível Médio de Nível Superior ----
dados_eixo$nivel <- case_when(stringr::str_detect(dados_eixo$eixo, "_em")~"Nível Médio",
                                TRUE~"Nível Superior")

# Alterando nomes dos Eixos ----
dados_eixo[dados_eixo$eixo == "eixo_amb_saude_em", "eixo"] <-"Ambiente e Saúde"
dados_eixo[dados_eixo$eixo == "eixo_conteprocessos_em", "eixo"] <-"Controle e Processos Industriais"
dados_eixo[dados_eixo$eixo == "eixo_desedusoc_em", "eixo"] <-"Desenvolvimento Educacional e Social"
dados_eixo[dados_eixo$eixo == "eixo_negocios_em", "eixo"] <-"Gestão e Negócios"
dados_eixo[dados_eixo$eixo == "eixo_infoecomunic_em", "eixo"] <-"Informação e Comunicação"
dados_eixo[dados_eixo$eixo == "eixo_infraestrutura_em", "eixo"] <-"Infraestrutura"
dados_eixo[dados_eixo$eixo == "eixo_prodaliment_em", "eixo"] <-"Produção Alimentícia"
dados_eixo[dados_eixo$eixo == "eixo_prodcult_em", "eixo"] <-"Produção Cultural e Design"
dados_eixo[dados_eixo$eixo == "eixo_prodindust_em", "eixo"] <-"Produção Industrial"
dados_eixo[dados_eixo$eixo == "eixo_recnaturais_em", "eixo"] <-"Recursos Naturais"
dados_eixo[dados_eixo$eixo == "eixo_seguranca_em", "eixo"] <-"Segurança"
dados_eixo[dados_eixo$eixo == "eixo_hospelazer_em", "eixo"] <-"Turismo Hospedagem e Lazer"
dados_eixo[dados_eixo$eixo == "eixo_militar_em", "eixo"] <-"Militar"
dados_eixo[dados_eixo$eixo == "eixo_ambesaude_sup", "eixo"] <-"Ambiente e Saúde"
dados_eixo[dados_eixo$eixo == "eixo_conteprocessos_sup", "eixo"] <-"Controle e Processos Industriais"
dados_eixo[dados_eixo$eixo == "eixo_negocios_sup", "eixo"] <-"Gestão e Negócios"
dados_eixo[dados_eixo$eixo == "eixo_infoecomunic_sup", "eixo"] <-"Informação e Comunicação"
dados_eixo[dados_eixo$eixo == "eixo_infraestrutura_sup", "eixo"] <-"Infraestrutura"
dados_eixo[dados_eixo$eixo == "eixo_prodcult_sup", "eixo"] <-"Produção Cultural e Design"
dados_eixo[dados_eixo$eixo == "eixo_prodindust_sup", "eixo"] <-"Produção Industrial"
dados_eixo[dados_eixo$eixo == "eixo_seguranca_sup", "eixo"] <-"Segurança"
dados_eixo[dados_eixo$eixo == "eixo_hospelazer_sup", "eixo"] <-"Turismo Hospedagem e Lazer"
dados_eixo[dados_eixo$eixo == "eixo_prodaliment_sup", "eixo"] <-"Produção Alimentícia"
dados_eixo[dados_eixo$eixo == "eixo_militar_sup", "eixo"] <-"Militar"

# Adicionando Estrutura de CBO ----
# nomes cbos
estrutura_cbo <- readRDS('produto/rds/estrutura-cbo.rds')
# join
dados_cbo_eixo <- left_join(dados_eixo, estrutura_cbo, by = "cboocupacao2002")
# variável de codigo - nome
dados_cbo_eixo$ocupacao <- paste0(dados_cbo_eixo$cboocupacao2002,"\n",dados_cbo_eixo$nome_cbo_ocupacao)

# Base Final ----
dados_eixo <- dados_cbo_eixo %>% 
  dplyr::select(cboocupacao2002,nome_cbo_ocupacao,eixo,vinculos)

dados_cbo_eixo <- dados_cbo_eixo %>% select(nome_cbo_ocupacao,cboocupacao2002,ocupacao,eixo,nivel,vinculos)

saveRDS(dados_eixo,"RDS/dados_cbo_eixo.RDS")

readr::write_excel_csv2(dados_cbo_eixo, "produto/csv/base-cbo-eixo-tec.csv")


paleta <- c("#242424","#064471","#2b597a","#2960a7",
            "#2b7398","#2e818d","#3e9974","#46a462",
            "#84b263","#99b763","#bebf64","#f2cb64",
            "#a6a6a6")

#install.packages("treemapify")
library(treemapify)
library(ggplot2)

options(scipen=999) 

dados_cbo_eixo %>%
  dplyr::filter(nivel == "Nível Médio") %>% 
  dplyr::group_by(eixo) %>% 
  dplyr::mutate(prop_vinculos = vinculos/sum(vinculos)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(ocup = ifelse(prop_vinculos <= .005,"Outros",nome_cbo_ocupacao)) %>% 
  dplyr::group_by(eixo,ocup) %>% 
  dplyr::summarise(vinculos = sum(vinculos)) %>%
  ggplot(aes(area = vinculos, fill = eixo,
               label = ocup, subgroup = eixo)) +
  geom_treemap() + 
  geom_treemap_text(colour = "white",
                    #place = "centre",
                    size = 8, 
                    grow = T) +
  geom_treemap_subgroup_border(colour = "white", size = 1.5) +
  scale_fill_manual(values = paleta) +
  theme(legend.position = "bottom") +
  labs(fill = "")

dados_cbo_eixo %>%
  dplyr::filter(nivel == "Nível Médio") %>% 
  dplyr::arrange(eixo,vinculos) %>% 
  dplyr::group_by(eixo) %>% 
  dplyr::mutate(soma_acum = cumsum(vinculos),
                prop_acum = cumsum(vinculos)/sum(vinculos)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(prop_vinculos = vinculos/sum(vinculos)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(ocup = ifelse(prop_acum <= .05,"Outros",nome_cbo_ocupacao)) %>% 
  dplyr::group_by(eixo,ocup) %>% 
  dplyr::summarise(vinculos = sum(vinculos)) %>%
  ggplot(aes(area = vinculos, fill = eixo,
             label = ocup, subgroup = eixo)) +
  geom_treemap() + 
  geom_treemap_text(colour = "white",
                    #place = "centre",
                    size = 8, 
                    grow = T) +
  geom_treemap_subgroup_border(colour = "white", size = 1.5) +
  scale_fill_manual(values = paleta) +
  theme(legend.position = "bottom") +
  labs(fill = "")


dados_cbo_eixo %>%
  dplyr::arrange(eixo,vinculos) %>% 
  dplyr::group_by(eixo) %>% 
  dplyr::mutate(soma_acum = cumsum(vinculos),
                prop_acum = cumsum(vinculos)/sum(vinculos),
                prop_eixo = vinculos/sum(vinculos)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(prop_vinculos = vinculos/sum(vinculos)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(ocup = ifelse(prop_acum <= .08,"Outros",nome_cbo_ocupacao)) %>% 
  dplyr::group_by(eixo,ocup) %>% 
  dplyr::summarise(vinculos = sum(vinculos)) %>%
  dplyr::mutate(cat = ifelse(ocup == "Outros","Outros","CBO")) %>% 
  ggplot(aes(area = vinculos, fill = eixo,
             label = ocup, subgroup = eixo, subgroup2 = cat, subgroup3 = ocup)) +
  geom_treemap() + 
  geom_treemap_text(colour = "white",
                    #place = "centre",
                    size = 8, 
                    grow = F,
                    reflow = T) +
  geom_treemap_subgroup_border(colour = "white", size = 2) +
  geom_treemap_subgroup2_border(colour = "white", size = 2) +
  geom_treemap_subgroup3_border(colour = "white", size = 2) +
  scale_fill_manual(values = paleta) +
  theme(legend.position = "bottom") +
  labs(fill = "")



  dados_cbo_eixo %>%
  dplyr::arrange(eixo, vinculos) %>% 
  dplyr::group_by(eixo) %>% 
  dplyr::mutate(soma_acum = cumsum(vinculos),
                prop_acum = cumsum(vinculos) / sum(vinculos),
                prop_eixo = vinculos / sum(vinculos)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(prop_vinculos = vinculos / sum(vinculos),
                rank_prop_eixo = rank(prop_eixo)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(ocup = ifelse(
    prop_acum <= .01 | 
      (rank_prop_eixo > 1 & prop_eixo <= .01 * max(prop_eixo[rank_prop_eixo < max(rank_prop_eixo)])), 
    "Outros", 
    nome_cbo_ocupacao
  )) %>% 
  dplyr::group_by(eixo, ocup) %>% 
  dplyr::summarise(vinculos = sum(vinculos)) %>%
  ggplot(aes(area = vinculos, fill = eixo,
             label = ocup, subgroup = eixo)) +
  geom_treemap() + 
  geom_treemap_text(colour = "white",
                    #place = "centre",
                    size = 8, 
                    grow = T) +
  geom_treemap_subgroup_border(colour = "white", size = 1.5) +
  scale_fill_manual(values = paleta) +
  theme(legend.position = "bottom") +
  labs(fill = "")

dados_cbo_eixo %>%
  dplyr::arrange(eixo, vinculos) %>% 
  dplyr::group_by(eixo) %>% 
  dplyr::mutate(soma_acum = cumsum(vinculos),
                prop_acum = cumsum(vinculos) / sum(vinculos),
                prop_eixo = vinculos / sum(vinculos),
                rank_vinculos = rank(vinculos)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(prop_vinculos = vinculos / sum(vinculos),
                rank_prop_eixo = rank(prop_eixo)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(ocup = ifelse(
    prop_acum <= .01 | 
      (rank_prop_eixo > 1 & prop_eixo <= .01 * max(prop_eixo[!nome_cbo_ocupacao %in% "Outros"])), 
    "Outros", 
    nome_cbo_ocupacao
  )) %>% 
  dplyr::group_by(eixo, ocup) %>% 
  dplyr::summarise(vinculos = sum(vinculos)) %>%
  ggplot(aes(area = vinculos, fill = eixo,
             label = ocup, subgroup = eixo)) +
  geom_treemap() + 
  geom_treemap_text(colour = "white",
                    #place = "centre",
                    size = 8, 
                    grow = T,
                    reflow = T) +
  geom_treemap_subgroup_border(colour = "white", size = 1.5) +
  scale_fill_manual(values = paleta) +
  theme(legend.position = "bottom") +
  labs(fill = "")

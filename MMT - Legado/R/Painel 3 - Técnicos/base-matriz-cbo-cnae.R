#### Scrip gera dados em matriz de CBOs por CNAE
# Carregando pacotes ----
# Limpa Diretório
rm(list=ls(all=T))
# Dados sem notação científica
options(scipen=100)

# Carregando pacotes ----
library(data.table)
library(tidyverse)

rstudioapi::writeRStudioPreference("data_viewer_max_columns", 300L)
## Carregando dados ----

## Rais
rais <- data.table::fread('dados/rais-panorama-2021.csv',encoding = "Latin-1")

## Corrijindo Cnae 2.3 da base ----
rais[rais$cnae20subclasse ==1091101, "cnae20subclasse"] <-1091100
rais[rais$cnae20subclasse ==1091102, "cnae20subclasse"] <-4721101
rais[rais$cnae20subclasse ==1610203, "cnae20subclasse"] <-1610201
rais[rais$cnae20subclasse ==1822901, "cnae20subclasse"] <-1822900
rais[rais$cnae20subclasse ==1822902, "cnae20subclasse"] <-1822900
rais[rais$cnae20subclasse ==1822999, "cnae20subclasse"] <-1822900
rais[rais$cnae20subclasse ==2013401, "cnae20subclasse"] <-2013400
rais[rais$cnae20subclasse ==2539001, "cnae20subclasse"] <-2539000
rais[rais$cnae20subclasse ==3091101, "cnae20subclasse"] <-3091100
rais[rais$cnae20subclasse ==3250709, "cnae20subclasse"] <-3250707
rais[rais$cnae20subclasse ==3511501, "cnae20subclasse"] <-3511500
rais[rais$cnae20subclasse ==3511502, "cnae20subclasse"] <-3511500
rais[rais$cnae20subclasse ==4520008, "cnae20subclasse"] <-4520001
rais[rais$cnae20subclasse ==4541206, "cnae20subclasse"] <-4541205
rais[rais$cnae20subclasse ==4713004, "cnae20subclasse"] <-4713001
rais[rais$cnae20subclasse ==4713005, "cnae20subclasse"] <-4713001
rais[rais$cnae20subclasse ==4729602, "cnae20subclasse"] <-4729699
rais[rais$cnae20subclasse ==4744006, "cnae20subclasse"] <-4744005
rais[rais$cnae20subclasse ==4751201, "cnae20subclasse"] <-4751200
rais[rais$cnae20subclasse ==4751202, "cnae20subclasse"] <-4751200
rais[rais$cnae20subclasse ==5611204, "cnae20subclasse"] <-5611202
rais[rais$cnae20subclasse ==5611205, "cnae20subclasse"] <-5611202
rais[rais$cnae20subclasse ==5812301, "cnae20subclasse"] <-5812300
rais[rais$cnae20subclasse ==5812302, "cnae20subclasse"] <-5812300
rais[rais$cnae20subclasse ==5822101, "cnae20subclasse"] <-5822100
rais[rais$cnae20subclasse ==6201501, "cnae20subclasse"] <-6201500
rais[rais$cnae20subclasse ==6201502, "cnae20subclasse"] <-6201500
rais[rais$cnae20subclasse ==6810203, "cnae20subclasse"] <-6810202
rais[rais$cnae20subclasse ==7410299, "cnae20subclasse"] <-7410202
rais[rais$cnae20subclasse ==8020001, "cnae20subclasse"] <-8020000
rais[rais$cnae20subclasse ==8020002, "cnae20subclasse"] <-8020000
rais[rais$cnae20subclasse ==8690903, "cnae20subclasse"] <-8690901
rais[rais$cnae20subclasse ==8690904, "cnae20subclasse"] <-8690999
rais[rais$cnae20subclasse ==9412001, "cnae20subclasse"] <-9412000
rais[rais$cnae20subclasse ==9412099, "cnae20subclasse"] <-9412000
rais[rais$cnae20subclasse ==9609206, "cnae20subclasse"] <-9609299
rais[rais$cnae20subclasse ==9609207, "cnae20subclasse"] <-9609203
rais[rais$cnae20subclasse ==9609208, "cnae20subclasse"] <-9609203
rais[rais$cnae20subclasse ==4713004, "cnae20subclasse"] <-4713001

## Rais 2021 Técnicos
rais_tec <- rais %>%
  dplyr::filter(referencia == 2021, 
                cbo_tec == 1,
                !is.na(cboocupacao2002), 
                !is.na(cnae20subclasse))

## Bases com os nomes de CBOs ----
estrutura_cbo <- readRDS('RDS/estrutura-cbo.rds') 

estrutura_cbo$nome_cbo_grandegrupo <- paste0(estrutura_cbo$cbo_grande_grupo," - ",estrutura_cbo$nome_cbo_grandegrupo)

## Bases com os nomes de CNAEs ----
estrutura_cnae <- readRDS('RDS/estrutura-cnae.rds')
## Bases com a conversão CNAEs Setor IBGE ----
cnae_ibge <- readxl::read_excel("dados/conversao-cnae-setor-ibge.xlsx")

### Selecionando variáveis de conversão
cnae_ibge <- cnae_ibge %>% 
  dplyr::select(cnae20subclasse = CNAE, setor_ibge = `ibge gr setor`)
#### União
estrutura_cnae <- estrutura_cnae %>% 
  dplyr::left_join(cnae_ibge)

## Base com número de vínculos por CBO e eixo ----
### Definindo função ----
calcula_eixo = function(nome){
  temp = copy(rais)
  
  temp = temp[get(nome) == 1,]
  
  conta = temp[referencia == 2021, .(vinculos = .N), by = cboocupacao2002]
  
  conta[, eixo := nome]
}

### Definindo Espaço do loop (dummies de eixo) ----
nomes_superior = names(rais)[46:57] # Ensino Superior
nomes_medio = names(rais)[33:45] #Ensino Médio

### Loop ----
dados_eixo = rbind(
  rbindlist(lapply(nomes_medio, calcula_eixo)),
  rbindlist(lapply(nomes_superior, calcula_eixo))
)

### Diferenciando Nível Médio de Nível Superior ----
dados_eixo$nivel <- case_when(stringr::str_detect(dados_eixo$eixo, "_em")~"Nível Médio",
                              TRUE~"Nível Superior")

### Alterando nomes dos Eixos ----
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

# Unindo as bases ----
base_tec <- rais_tec[,c("referencia","cboocupacao2002","cnae20subclasse")] %>% 
  dplyr::mutate(cnae20subclasse = as.character(cnae20subclasse)) %>% 
  dplyr::left_join(estrutura_cbo) %>% 
  dplyr::left_join(estrutura_cnae) %>%
  dplyr::left_join(dados_eixo)
  
base_cbo_cnae <- base_tec %>%
  dplyr::filter(!is.na(setor_ibge)) %>% 
  dplyr::group_by(referencia,cboocupacao2002,cnae20subclasse,
                  nome_cbo_ocupacao,nome_cbo_grandegrupo,
                  nome_cnae_subclasse,setor_ibge) %>% 
  dplyr::summarise(vinculos = n(),
                   eixo = "Geral")

base_cbo_eixo <- base_tec %>%
  dplyr::group_by(referencia,cboocupacao2002,
                  nome_cbo_ocupacao,eixo) %>% 
  dplyr::summarise(vinculos = n(),
                   cnae20subclasse = "Não se aplica",
                   nome_cbo_grandegrupo = "Não se aplica",
                   nome_cnae_subclasse = "Não se aplica",
                   setor_ibge = "Não se aplica")

base_final <- rbind(base_cbo_cnae,base_cbo_eixo)

                     
paleta <- c("#064471","#2b597a","#2960a7","#2b7398","#2e818d","#3e9974","#46a462",
            "#84b263","#99b763","#bebf64","#f2cb64","#a6a6a6","#242424")
                     
# Cnaes par auma dada CBO
base_final %>% 
  dplyr::filter(cboocupacao2002 == 513505) %>% #Assistente Administrativo - 411010
  dplyr::filter(eixo == "Geral") %>% 
  ggplot(aes(area = vinculos,
             fill = setor_ibge,
             subgroup = setor_ibge,
             label = nome_cnae_subclasse)) +
  treemapify::geom_treemap() + 
  treemapify::geom_treemap_text(colour = "white", place = "bottomleft",size = 10, reflow = T) +
  treemapify::geom_treemap_subgroup_border(colour = "white", size = 2) +
  scale_fill_manual(values = paleta)+
  theme(legend.position = "bottom") +
  labs(fill = "Grande Setor IBGE", title = "Auxiliar nos serviços de alimentação - 513505")

# CBOs para uma dada Cnae
base_final %>% 
  dplyr::filter(cnae20subclasse == 8411600) %>% #Assistente Administrativo - 411010
  dplyr::filter(eixo == "Geral") %>% 
  ggplot(aes(area = vinculos,
             fill = nome_cbo_grandegrupo,
             subgroup = nome_cbo_grandegrupo,
             label = nome_cbo_ocupacao)) +
  treemapify::geom_treemap() + 
  treemapify::geom_treemap_text(colour = "white", place = "bottomleft",size = 10, reflow = T) +
  treemapify::geom_treemap_subgroup_border(colour = "white", size = 2) +
  scale_fill_manual(values = paleta)+
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(ncol = 1))+
  labs(fill = "Grande Grupo CBO", title = "Administração pública em geral - 8411600")

# Cbos por eixo
base_final %>% 
  dplyr::filter(eixo != "Geral") %>%
  ggplot2::ggplot(aes(area = vinculos, 
                      fill = eixo, 
                      label = nome_cbo_ocupacao, 
                      subgroup = eixo)) +
  treemapify::geom_treemap() + 
  treemapify::geom_treemap_text(colour = "white", place = "bottomleft",
                                size = 10, reflow = T) +
  treemapify::geom_treemap_subgroup_border(colour = "white", size = 2) +
  scale_fill_manual(values = paleta)+
  theme(legend.position = "bottom") +
  labs(fill = "", title = "CBOs por Eixo")

readr::write_excel_csv2(base_final, 'produto/csv/painel3/base-eixo-cbo-cnae.csv')

# Preparando base geral

# # Preparando base CBO x CNAE ----
# base <- left_join(rais, estrutura_cbo, by = "cboocupacao2002") %>% 
#   left_join(estrutura_cnae, by = "cnae20subclasse") %>%
#   mutate(cbo = paste0(cboocupacao2002, " - ", nome_cbo_ocupacao),
#          cnae = paste0(cnae20subclasse," - ", nome_cnae_subclasse)) %>% 
#   select(cbo, cnae)
# 
# # Base CBO x CNAE ----
# base_cbo_cnae <- as.data.frame.matrix(table(base$cbo, base$cnae))
# 
# ## Nomeando primeira coluna que estava sem nome pelo processo de criação da base)
# base_cbo_cnae <- rownames_to_column(base_cbo_cnae, var = "cboocupacao2002")
# 
# # Base CNAE x CBO ----
# base_cnae_cbo <- as.data.frame.matrix(table(base$cnae, base$cbo))
# 
# ## Nomeando primeira coluna que estava sem nome pelo processo de criação da base)
# base_cnae_cbo <- rownames_to_column(base_cnae_cbo, var = "cnae20subclasse")
# 
# readr::write_excel_csv2(base_cbo_cnae, 'produto/csv/base-cbo_cnae.csv')
# readr::write_excel_csv2(base_cnae_cbo, 'produto/csv/base-cnae-cbo.csv')

#'*Script que cria os dados para os gráficos de Árvore de Ocupações para Empresas no MMT*

# Carregar pacotes ----

library(tidyverse)# Manipulação dos dados
library(imputeTS) # SUbstituir NA's
library(readxl)   # Leitura de Arquivos em .xlsx

`%notin%` <- Negate(`%in%`)           # Função de filtro
options(readr.show_col_types = FALSE) # Omitir formato das colunas no console

# Importação dos dados ----

estrutura_cbo <- readRDS("../1. Extração dos dados/Dicionários/Dicionário CBO.RDS")

cnae_ibge <- readRDS("../1. Extração dos dados/Dicionários/CNAE para Setor IBGE.RDS") |> 
  select(cnae20subclasse = CNAE, 
         setor_ibge = `ibge gr setor`)

estrutura_cnae <- readRDS("../1. Extração dos dados/Dicionários/Dicionário CNAE.RDS") |>
  left_join(cnae_ibge) |> 
  unique()

## Filtro de técnicos ----

rais_1 <- rais |> 
  filter(referencia == max(referencia), 
         cbo_tec == 1,
         !is.na(cboocupacao2002), 
         !is.na(cnae20subclasse))

## Corrijindo CNAE 2.3 da base ----

rais_1[rais_1$cnae20subclasse ==1091101, "cnae20subclasse"] <-1091100
rais_1[rais_1$cnae20subclasse ==1091102, "cnae20subclasse"] <-4721101
rais_1[rais_1$cnae20subclasse ==1610203, "cnae20subclasse"] <-1610201
rais_1[rais_1$cnae20subclasse ==1822901, "cnae20subclasse"] <-1822900
rais_1[rais_1$cnae20subclasse ==1822902, "cnae20subclasse"] <-1822900
rais_1[rais_1$cnae20subclasse ==1822999, "cnae20subclasse"] <-1822900
rais_1[rais_1$cnae20subclasse ==2013401, "cnae20subclasse"] <-2013400
rais_1[rais_1$cnae20subclasse ==2539001, "cnae20subclasse"] <-2539000
rais_1[rais_1$cnae20subclasse ==3091101, "cnae20subclasse"] <-3091100
rais_1[rais_1$cnae20subclasse ==3250709, "cnae20subclasse"] <-3250707
rais_1[rais_1$cnae20subclasse ==3511501, "cnae20subclasse"] <-3511500
rais_1[rais_1$cnae20subclasse ==3511502, "cnae20subclasse"] <-3511500
rais_1[rais_1$cnae20subclasse ==4520008, "cnae20subclasse"] <-4520001
rais_1[rais_1$cnae20subclasse ==4541206, "cnae20subclasse"] <-4541205
rais_1[rais_1$cnae20subclasse ==4713004, "cnae20subclasse"] <-4713001
rais_1[rais_1$cnae20subclasse ==4713005, "cnae20subclasse"] <-4713001
rais_1[rais_1$cnae20subclasse ==4729602, "cnae20subclasse"] <-4729699
rais_1[rais_1$cnae20subclasse ==4744006, "cnae20subclasse"] <-4744005
rais_1[rais_1$cnae20subclasse ==4751201, "cnae20subclasse"] <-4751200
rais_1[rais_1$cnae20subclasse ==4751202, "cnae20subclasse"] <-4751200
rais_1[rais_1$cnae20subclasse ==5611204, "cnae20subclasse"] <-5611202
rais_1[rais_1$cnae20subclasse ==5611205, "cnae20subclasse"] <-5611202
rais_1[rais_1$cnae20subclasse ==5812301, "cnae20subclasse"] <-5812300
rais_1[rais_1$cnae20subclasse ==5812302, "cnae20subclasse"] <-5812300
rais_1[rais_1$cnae20subclasse ==5822101, "cnae20subclasse"] <-5822100
rais_1[rais_1$cnae20subclasse ==6201501, "cnae20subclasse"] <-6201500
rais_1[rais_1$cnae20subclasse ==6201502, "cnae20subclasse"] <-6201500
rais_1[rais_1$cnae20subclasse ==6810203, "cnae20subclasse"] <-6810202
rais_1[rais_1$cnae20subclasse ==7410299, "cnae20subclasse"] <-7410202
rais_1[rais_1$cnae20subclasse ==8020001, "cnae20subclasse"] <-8020000
rais_1[rais_1$cnae20subclasse ==8020002, "cnae20subclasse"] <-8020000
rais_1[rais_1$cnae20subclasse ==8690903, "cnae20subclasse"] <-8690901
rais_1[rais_1$cnae20subclasse ==8690904, "cnae20subclasse"] <-8690999
rais_1[rais_1$cnae20subclasse ==9412001, "cnae20subclasse"] <-9412000
rais_1[rais_1$cnae20subclasse ==9412099, "cnae20subclasse"] <-9412000
rais_1[rais_1$cnae20subclasse ==9609206, "cnae20subclasse"] <-9609299
rais_1[rais_1$cnae20subclasse ==9609207, "cnae20subclasse"] <-9609203
rais_1[rais_1$cnae20subclasse ==9609208, "cnae20subclasse"] <-9609203
rais_1[rais_1$cnae20subclasse ==4713004, "cnae20subclasse"] <-4713001

## Preparação da base ----

base_tec <- rais_1 |>
  select(cboocupacao2002, cnae20subclasse) |>
  mutate(cnae20subclasse = as.character(cnae20subclasse)) |>
  left_join(estrutura_cbo, by = "cboocupacao2002") |>
  left_join(estrutura_cnae, by = "cnae20subclasse") |> 
  filter(!is.na(setor_ibge)) |> 
  group_by(cboocupacao2002,cnae_classe,cnae_divisao,cnae_grupo,
           nome_cbo_ocupacao,
           nome_cnae_classe,setor_ibge) |> 
  summarise(vinculos = n())

# Gráfico Exemplo ----

cbo_filtrado <- sample(unique(base_tec$nome_cbo_ocupacao), 1)

base_tec |> 
  filter(nome_cbo_ocupacao == cbo_filtrado) |> 
  mutate(label= paste0(nome_cnae_classe,": ",format(vinculos,big.mark = ".",decimal.mark = ","))) |> 
  ggplot(aes(area = vinculos,
             fill = setor_ibge,
             subgroup = setor_ibge,
             subgroup2 = cnae_divisao,
             subgroup3 = cnae_grupo,
             label = label)) +
  treemapify::geom_treemap() + theme_void()+
  treemapify::geom_treemap_text(colour = "white" ,size = 13, reflow = T) +
  treemapify::geom_treemap_subgroup_border(colour = "white", size = 3) +
  treemapify::geom_treemap_subgroup2_border(colour = "white", size = 1) +
  treemapify::geom_treemap_subgroup3_border(colour = "white", size = 1) +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = c("#0a78c7","#2b597a","#fb8e80","#f87d28","#bd3928","#d92335"))+
  labs(fill = "Grande Setor IBGE",title = paste("Gráfico para CBO:", cbo_filtrado))

# Exportar CSV ----

write_excel_csv2(base_tec, "Painel 3 - Ocupações Técnicas/Resultados/3.1 - Ocupações para empresas.csv")

remove(base_tec, rais_1)

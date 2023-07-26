#'*Script que cria os dados para os gráficos de Árvore de Ocupações por Eixo no MMT*

# Carregar pacotes ----

library(tidyverse)# Manipulação dos dados
library(imputeTS) # SUbstituir NA's
library(readxl)   # Leitura de Arquivos em .xlsx
library(treemapify)
library(stringr)

`%notin%` <- Negate(`%in%`)           # Função de filtro
options(readr.show_col_types = FALSE) # Omitir formato das colunas no console

calcula_eixo <- function(nome){
  
  temp <- rais_3 |> 
    filter(.data[[nome]] == 1)
  
  conta <- temp |> 
    group_by(cbo_familia,
             nome_cbo_familia,
             nome_cbo_subgrupo,
             nome_cbo_subgrupo_principal) |> 
    summarise(vinculos = n()) |> 
    mutate(eixo = nome)
  
  return(conta)
}

# Importação dos dados ----

estrutura_cbo <- readRDS("../1. Extração dos dados/Dicionários/Dicionário CBO.RDS")

## Filtro de técnicos ----

rais_3 <- rais |>
  filter(referencia == max(referencia), 
                cbo_tec == 1,
                !is.na(cboocupacao2002)) |> 
  left_join(estrutura_cbo)

## Definindo espaço do loop (dummies de eixo) ----

nomes_medio <- names(rais_3)[26:38]    # Ensino Médio
nomes_superior <- names(rais_3)[39:51] # Ensino Superior

## Loop ----

dados_eixo <- bind_rows(
  lapply(nomes_medio, calcula_eixo),
  lapply(nomes_superior, calcula_eixo)) |> 
  mutate(nivel = case_when(str_detect(eixo, "_em") ~ "Nível Médio",
                           TRUE ~ "Nível Superior"),
         eixo = case_when(eixo %in% c("eixo_amb_saude_em", "eixo_amb_saude_sup")           ~ "Ambiente e Saúde",
                          eixo %in% c("eixo_conteprocessos_em", "eixo_conteprocessos_sup") ~ "Controle e Processos Industriais",
                          eixo %in% c("eixo_desedusoc_em")                                 ~ "Desenvolvimento Educacional e Social",
                          eixo %in% c("eixo_negocios_em", "eixo_negocios_sup")             ~ "Gestão e Negócios",
                          eixo %in% c("eixo_infoecomunic_em", "eixo_infoecomunic_sup")     ~ "Informação e Comunicação",
                          eixo %in% c("eixo_infraestrutura_em", "eixo_infraestrutura_sup") ~ "Infraestrutura",
                          eixo %in% c("eixo_prodaliment_em", "eixo_prodaliment_sup")       ~ "Produção Alimentícia",
                          eixo %in% c("eixo_prodcult_em", "eixo_prodcult_sup")             ~ "Produção Cultural e Design",
                          eixo %in% c("eixo_prodindust_em", "eixo_prodindust_sup")         ~ "Produção Industrial",
                          eixo %in% c("eixo_recnaturais_em", "eixo_recnaturais_sup")       ~ "Recursos Naturais",
                          eixo %in% c("eixo_seguranca_em", "eixo_seguranca_sup")           ~ "Segurança",
                          eixo %in% c("eixo_hospelazer_em", "eixo_hospelazer_sup")         ~ "Turismo Hospedagem e Lazer",
                          eixo %in% c("eixo_militar_em", "eixo_militar_sup")               ~ "Militar",
                          TRUE ~ eixo ))

# Gráfico Exemplo ----

dados_eixo |>
  mutate(label= paste0(nome_cbo_familia,": ",format(vinculos,big.mark = ".",decimal.mark =","))) |>
  ggplot(aes(area = vinculos, 
                      fill = eixo, 
                      label = label, 
                      subgroup = eixo,
                      subgroup2 = nome_cbo_subgrupo_principal,
                      subgroup3 = nome_cbo_subgrupo)) +
  geom_treemap() + 
  geom_treemap_text(colour = "white", place = "bottomleft",
                                size = 10, reflow = T) +
  geom_treemap_subgroup_border(colour = "white", size = 2.1) +
  geom_treemap_subgroup2_border(colour = "white", size = 1) +
  geom_treemap_subgroup3_border(colour = "white", size = .5) +
  scale_fill_manual(values = c("#0a78c7","#2b597a","#fb8e80","#00980c",
                               "#bd3928","#d92335","#f87d28","#064471",
                               "#72879d","#e46874","#54a4dc","#e2ccc4","#808080"))+
  theme(legend.position = "bottom") +
  labs(fill = "", title = "FAMÍLIAS DE OCUPAÇÕES TÉCNICAS DO DF POR EIXO TECNOLÓGICO")

# Exportar CSV ----

write_excel_csv2(dados_eixo, "Painel 3 - Ocupações Técnicas/Resultados/3.3 - Ocupações e eixos.csv")

remove(rais_3)
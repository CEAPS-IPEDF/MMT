#'*Script que calcula a remuneração média por hora trabalhada*

# Carregar pacotes ----

library(tidyverse)# Manipulação dos dados
library(imputeTS) # Substituir NA's
library(readxl)   # Leitura de Arquivos em .xlsx
library(stringr)  # Manipulação de dados em formato de caracter
library(data.table)

`%notin%` <- Negate(`%in%`)           # Função de filtro
options(readr.show_col_types = FALSE) # Omitir formato das colunas no console

# Importação dos dados ----
rais <- readRDS("../1. Extração dos dados/RAIS/Dados/RAIS.RDS")
estrutura_cbo <- readRDS("Dados/Dicionário CBO.RDS")
estrutura_cnae <- readRDS("Dados/Dicionário CNAE.RDS")
cnae_ibge <- readxl::read_excel("Dados/conversao-cnae-setor-ibge.xlsx")

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
#=========================================================

base <- rais |> 
   select(referencia,cboocupacao2002,cnae20subclasse) |>  
   left_join(estrutura_cbo,by = "cboocupacao2002") |> 
   left_join(estrutura_cnae,by = "cnae20subclasse")

# base |>
#    filter(nome_cnae_divisao =="Atividades de atenção à saúde humana") |> 
#    group_by(referencia) |> 
#    summarise(vinc = n())
# 

base_saude <- base |>
   filter(cnae_divisao == "86")

base_saude |>
   filter(nome_cbo_grandegrupo == "PROFISSIONAIS DAS CIÊNCIAS E DAS ARTES",
          grepl("édic",nome_cbo_ocupacao)) |> 
   group_by(referencia,nome_cbo_ocupacao) |> 
   summarise(vinc= n()) |>
   filter(vinc >50) |> 
   ggplot(aes(x = referencia,y = vinc,col = nome_cbo_ocupacao))+
   geom_line()+geom_point()+theme_classic()
   
rais_medicos <- rais |>
   left_join(estrutura_cbo,by = "cboocupacao2002") |> 
   left_join(estrutura_cnae,by = "cnae20subclasse") |> 
   filter(nome_cbo_ocupacao %in% c("Médico clínico","Biomédico"))

rais |>
   left_join(estrutura_cbo,by = "cboocupacao2002") |> 
   left_join(estrutura_cnae,by = "cnae20subclasse") |> 
   group_by(referencia,nome_cnae_divisao) |> 
   summarise(vinc= n()) |>
   filter(vinc >1000) |> 
   ggplot(aes(x = referencia,y = vinc,col = nome_cnae_divisao))+
   geom_line()+geom_point()+theme_classic()

rais <- rais |>
   left_join(estrutura_cbo,by = "cboocupacao2002") |> 
   left_join(estrutura_cnae,by = "cnae20subclasse")
   
rais |>
   filter(cnae_divisao == "86") |> 
   group_by(referencia,tempoemprego) |> 
   summarise(vinc= n()) |>
   ggplot(aes(x = referencia,y = vinc,col = factor(tempoemprego)))+
   geom_line()+geom_point()+ 
   #(label = scales::number_format(vinc))+ 
   theme_bw()+ggtitle("Atividades de atenção à saúde humana")


 rais_medicos |> 
   group_by(referencia) |> 
   summarise(vinc= n()) |>
   ggplot(aes(x = referencia,y = vinc))+
   geom_line()+geom_point()+theme_classic()

rais_medicos |>  
   group_by(referencia,tipovinculo) |> 
   summarise(vinc= n()) |>
   ggplot(aes(x = referencia,y = vinc,col = factor(tipovinculo)))+
   geom_line()+geom_point()+theme_classic()


#=========================================================
## Rais 2021 Técnicos
rais_tec <- rais  |>
   mutate(cnae20subclasse = as.character(cnae20subclasse)) |> 
   filter(referencia == 2021, 
                 cbo_tec == 1,
                 !is.na(cboocupacao2002), 
                 !is.na(cnae20subclasse))

cnae_ibge <- cnae_ibge |> 
   select(cnae20subclasse = CNAE, setor_ibge = `ibge gr setor`) |> unique()

#### União
estrutura_cnae <- estrutura_cnae |> 
   left_join(cnae_ibge)

## Base com número de vínculos por CBO e eixo ----
### Definindo função ----
calcula_eixo <- function(nome) {
   temp <- rais |> 
      filter(get(nome) == 1)
   
   conta <- temp |> 
      filter(referencia == 2021)|> 
      group_by(cboocupacao2002) |> 
      summarise(vinculos = n()) |> 
      mutate(eixo = nome)
   
   return(conta)
}

### Definindo Espaço do loop (dummies de eixo) ----
nomes_superior = names(rais)[39:51] # Ensino Superior
nomes_medio = names(rais)[26:38] #Ensino Médio

### Loop ----
dados_eixo = rbind(
   rbindlist(lapply(nomes_medio, calcula_eixo)),
   rbindlist(lapply(nomes_superior, calcula_eixo))
)

### Diferenciando Nível Médio de Nível Superior ----
dados_eixo$nivel <- case_when(stringr::str_detect(dados_eixo$eixo, "_em")~"Nível Médio",
                              TRUE~"Nível Superior")

### Alterando nomes dos Eixos ----
dados_eixo <- dados_eixo %>%
   mutate(eixo = case_when(
      eixo == "eixo_amb_saude_em" ~ "Ambiente e Saúde",
      eixo == "eixo_conteprocessos_em" ~ "Controle e Processos Industriais",
      eixo == "eixo_desedusoc_em" ~ "Desenvolvimento Educacional e Social",
      eixo == "eixo_negocios_em" ~ "Gestão e Negócios",
      eixo == "eixo_infoecomunic_em" ~ "Informação e Comunicação",
      eixo == "eixo_infraestrutura_em" ~ "Infraestrutura",
      eixo == "eixo_prodaliment_em" ~ "Produção Alimentícia",
      eixo == "eixo_prodcult_em" ~ "Produção Cultural e Design",
      eixo == "eixo_prodindust_em" ~ "Produção Industrial",
      eixo == "eixo_recnaturais_em" ~ "Recursos Naturais",
      eixo == "eixo_seguranca_em" ~ "Segurança",
      eixo == "eixo_hospelazer_em" ~ "Turismo Hospedagem e Lazer",
      eixo == "eixo_militar_em" ~ "Militar",
      eixo == "eixo_ambesaude_sup" ~ "Ambiente e Saúde",
      eixo == "eixo_conteprocessos_sup" ~ "Controle e Processos Industriais",
      eixo == "eixo_negocios_sup" ~ "Gestão e Negócios",
      eixo == "eixo_infoecomunic_sup" ~ "Informação e Comunicação",
      eixo == "eixo_infraestrutura_sup" ~ "Infraestrutura",
      eixo == "eixo_prodcult_sup" ~ "Produção Cultural e Design",
      eixo == "eixo_prodindust_sup" ~ "Produção Industrial",
      eixo == "eixo_seguranca_sup" ~ "Segurança",
      eixo == "eixo_hospelazer_sup" ~ "Turismo Hospedagem e Lazer",
      eixo == "eixo_prodaliment_sup" ~ "Produção Alimentícia",
      eixo == "eixo_militar_sup" ~ "Militar",
      TRUE ~ eixo
   ))

# Unindo as bases ----
base_tec <- rais_tec |> 
   select(referencia,cboocupacao2002,cnae20subclasse) |>  
   left_join(estrutura_cbo,by = "cboocupacao2002") |> 
   left_join(estrutura_cnae,by = "cnae20subclasse")

library(ggplot2)
paleta <- c("#064471","#2b597a","#2960a7","#2b7398","#2e818d","#3e9974","#46a462",
            "#84b263","#99b763","#bebf64","#f2cb64","#a6a6a6","#242424")

base_tec |> 
   dplyr::filter(!is.na(setor_ibge)) |> 
   dplyr::group_by(referencia,cboocupacao2002,cnae_clas,
                   nome_cbo_ocupacao,nome_cbo_grandegrupo,
                   nome_cnae_classe,setor_ibge) |> 
   dplyr::summarise(vinculos = n()) |>
   dplyr::mutate(nome = paste0(nome_cnae_classe,"\n(",vinculos,")")) |> 
   dplyr::filter(vinculos >20) |> 
   dplyr::filter(cboocupacao2002 == 411010) %>% #Assistente Administrativo - 411010
   ggplot(aes(area = vinculos,
              fill = setor_ibge,
              subgroup = setor_ibge,
              label = nome)) +
   treemapify::geom_treemap() + 
   treemapify::geom_treemap_text(colour = "white", place = "bottomleft",size = 10, reflow = T) +
   treemapify::geom_treemap_subgroup_border(colour = "white", size = 2) +
   scale_fill_manual(values = paleta)+
   theme(legend.position = "bottom") +
   labs(fill = "Grande Setor IBGE", title = "Assistente Administrativo - 411010")


base_tec %>% 
   dplyr::filter(cboocupacao2002 == 411010) %>% #Assistente Administrativo - 411010
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


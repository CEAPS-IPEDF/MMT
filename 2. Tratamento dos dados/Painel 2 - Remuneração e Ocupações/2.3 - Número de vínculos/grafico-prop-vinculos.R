#### Scrip gera gráficos-exemplo de remuneração média

# Limpa Diretório
rm(list=ls(all=T))
# Dados sem notação científica
options(scipen=100)

# Carrega pacotes ----
library(data.table)
library(tidyverse)
library(readxl)

# Ler os dados ----
## RAIS ----
rais = data.table::fread('dados/rais-panorama-2021.csv',encoding = "Latin-1")

# Selecionar variáveis ----
base <- rais %>%
  select(ano = referencia,
         tipo_emprego, #salário hora e tipo de ocupação
         tec = cbo_tec) #escolaridade

# Gráficos ----
## Geral ----  
base %>%
  # variável de técnico
  mutate(tec = ifelse(tec == 1, "Técnico", "Não Técnico")) %>% 
  group_by(ano,tipo_emprego) %>% 
  summarise(vinculos = n()) %>%
  # percentuais
  mutate(prop = vinculos/sum(vinculos),
         # ajustando variável de ano
         ano = as.character(ano)) %>%
  # gráfico
  ggplot(aes(x = ano, y = prop, fill = tipo_emprego)) +
  # colunas
  geom_col()+
  # rótulos
  labs(title = "Evolução da proporção de vínculos no DF", x = "", 
       y = "Proporção de vínculos (%)") +
  # cores
  scale_fill_manual(values = c("#3e9974","#2e818d", "#f2cb64")) +
  # eixo y
  scale_y_continuous(label = scales::percent) +
  # remover eixos
  theme_minimal(base_size = 15) +
  # posição da legenda
  theme(panel.grid.major.x = element_blank(),legend.position = "bottom")

## Técnico e Não Técnico ----
ajuste <- base %>%
  # variável de técnico
  mutate(tec = ifelse(tec == 1, "Técnico", "Não Técnico"),
         tipo = paste0(tipo_emprego,"/",tec),
         ano = as.character(ano)) %>% 
  group_by(ano,tipo) %>% 
  summarise(vinculos = n()) %>% 
  # percentuais
  mutate(prop = vinculos/sum(vinculos))  

ajuste %>%
  # gráfico
  ggplot(aes(x = ano, y = prop, fill = tipo)) +
  # colunas
  geom_col(stat = "identity")+
  # rótulos
  labs(title = "Evolução da proporção de vínculos no DF", x = "", 
       y = "Proporção de vínculos (%)") +
  # rótulos
  geom_text(aes(x = ano, y = prop, label = ifelse(prop > 0.09,scales::percent(round(prop,2)),NA_character_), fontface = "bold"), 
            show.legend = F,
            position = position_stack(vjust = 0.5),
            col = "white") +
  # cores
  scale_fill_manual(values = c("#46a462","#3e9974" ,"#2e818d","#2b7398" ,"#f2cb64","#a6a6a6","#2960a7","#2b597a" )) +
  # eixo y
  scale_y_continuous(label = scales::percent) +
  # remover eixos
  theme_minimal(base_size = 15) +
  # posição da legenda
  theme(panel.grid.major.x = element_blank(),legend.position = "bottom")


#### Scrip gera gráficos-exemplo de ranking de ganhos e perdas salariais e de vínculos

# Limpa Diretório
rm(list=ls(all=T))
# Dados sem notação científica
options(scipen=100)

# Carregando pacotes ----
library(data.table)
library(tidyverse)
library(esquisse)

remun_media <- read.csv2('produto/csv/base-remun-media-escolaridade.csv')
#esquisser(remun_media)

# Remuneração Média ----
## Técnicos ----
### Celetistas ----
remun_media %>%
  # alterandovariável de ano
  mutate(ano = as.character(ano)) %>%
  # filtrando variáveis de interesse
  filter(filtro == "Técnico",
         tipo_emprego == "Celetista") %>%
  # gráfico
  ggplot(aes(x = ano, y = salario_hora, fill = tipo)) +
  # barras
  geom_bar(stat = "identity", position = position_dodge2()) +
  # título
  labs(title = "Remuneração média por hora trabalhada no Distrito Federal (Celetistas, Técnicos)", x = "", 
       y = "Remuneração média/hora (R$)", fill = "") +
  # cores
  scale_fill_manual(values = c("#46a462","#2e818d","#2960a7")) +
  # remover eixos
  theme_minimal(base_size = 15) +
  # posição da legenda
  theme(panel.grid.major.x = element_blank(),legend.position = "bottom")

### Estatutários ----
remun_media %>%
  # alterandovariável de ano
  mutate(ano = as.character(ano)) %>%
  # filtrando variáveis de interesse
  filter(filtro == "Técnico",
         tipo_emprego == "Estatutário") %>%
  # gráfico
  ggplot(aes(x = ano, y = salario_hora, fill = tipo)) +
  # barras
  geom_bar(stat = "identity", position = position_dodge2()) +
  # título
  labs(title = "Remuneração média por hora trabalhada no Distrito Federal (Estatutários, Técnicos)", x = "", 
       y = "Remuneração média/hora (R$)", fill = "") +
  # cores
  scale_fill_manual(values = c("#46a462","#2e818d","#2960a7")) +
  # remover eixos
  theme_minimal(base_size = 15) +
  # posição da legenda
  theme(panel.grid.major.x = element_blank(),legend.position = "bottom")

### Outros ----
remun_media %>%
  # alterandovariável de ano
  mutate(ano = as.character(ano)) %>%
  # filtrando variáveis de interesse
  filter(filtro == "Técnico",
         tipo_emprego == "Outros") %>%
  # gráfico
  ggplot(aes(x = ano, y = salario_hora, fill = tipo)) +
  # barras
  geom_bar(stat = "identity", position = position_dodge2()) +
  # título
  labs(title = "Remuneração média por hora trabalhada no Distrito Federal (Outros, Técnicos)", x = "", 
       y = "Remuneração média/hora (R$)", fill = "") +
  # cores
  scale_fill_manual(values = c("#46a462","#2e818d","#2960a7")) +
  # remover eixos
  theme_minimal(base_size = 15) +
  # posição da legenda
  theme(panel.grid.major.x = element_blank(),legend.position = "bottom")

### Geral ----
remun_media %>%
  # alterandovariável de ano
  mutate(ano = as.character(ano)) %>%
  # filtrando variáveis de interesse
  filter(filtro == "Técnico",
         tipo_emprego == "Geral") %>%
  # gráfico
  ggplot(aes(x = ano, y = salario_hora, fill = tipo)) +
  # barras
  geom_bar(stat = "identity", position = position_dodge2()) +
  # título
  labs(title = "Remuneração média por hora trabalhada no Distrito Federal (Geral, Técnicos)", x = "", 
       y = "Remuneração média/hora (R$)", fill = "") +
  # cores
  scale_fill_manual(values = c("#46a462","#2e818d","#2960a7")) +
  # remover eixos
  theme_minimal(base_size = 15) +
  # posição da legenda
  theme(panel.grid.major.x = element_blank(),legend.position = "bottom")

## Não Técnicos ----
### Celetistas ----
remun_media %>%
  # alterandovariável de ano
  mutate(ano = as.character(ano)) %>%
  # filtrando variáveis de interesse
  filter(filtro == "Não Técnico",
         tipo_emprego == "Celetista") %>%
  # gráfico
  ggplot(aes(x = ano, y = salario_hora, fill = tipo)) +
  # barras
  geom_bar(stat = "identity", position = position_dodge2()) +
  # título
  labs(title = "Remuneração média por hora trabalhada no Distrito Federal (Celetistas, Não Técnicos)", x = "", 
       y = "Remuneração média/hora (R$)", fill = "") +
  # cores
  scale_fill_manual(values = c("#46a462","#2e818d","#2960a7","#f2cb64","#a6a6a6")) +
  # remover eixos
  theme_minimal(base_size = 15) +
  # posição da legenda
  theme(panel.grid.major.x = element_blank(),legend.position = "bottom")

### Estatutários ----
remun_media %>%
  # alterandovariável de ano
  mutate(ano = as.character(ano)) %>%
  # filtrando variáveis de interesse
  filter(filtro == "Não Técnico",
         tipo_emprego == "Estatutário") %>%
  # gráfico
  ggplot(aes(x = ano, y = salario_hora, fill = tipo)) +
  # barras
  geom_bar(stat = "identity", position = position_dodge2()) +
  # título
  labs(title = "Remuneração média por hora trabalhada no Distrito Federal (Estatutários, Não Técnicos)", x = "", 
       y = "Remuneração média/hora (R$)", fill ="") +
  # cores
  scale_fill_manual(values = c("#46a462","#2e818d","#2960a7","#f2cb64","#a6a6a6")) +
  # remover eixos
  theme_minimal(base_size = 15) +
  # posição da legenda
  theme(panel.grid.major.x = element_blank(),legend.position = "bottom")


### Outros ----
remun_media %>%
  # alterandovariável de ano
  mutate(ano = as.character(ano)) %>%
  # filtrando variáveis de interesse
  filter(filtro == "Não Técnico",
         tipo_emprego == "Outros") %>%
  # gráfico
  ggplot(aes(x = ano, y = salario_hora, fill = tipo)) +
  # barras
  geom_bar(stat = "identity", position = position_dodge2()) +
  # título
  labs(title = "Remuneração média por hora trabalhada no Distrito Federal (Outros, Não Técnicos)", x = "", 
       y = "Remuneração média/hora (R$)", fill = "") +
  # cores
  scale_fill_manual(values = c("#46a462","#2e818d","#2960a7","#f2cb64","#a6a6a6")) +
  # remover eixos
  theme_minimal(base_size = 15) +
  # posição da legenda
  theme(panel.grid.major.x = element_blank(),legend.position = "bottom")

### Geral ----
remun_media %>%
  # alterandovariável de ano
  mutate(ano = as.character(ano)) %>%
  # filtrando variáveis de interesse
  filter(filtro == "Não Técnico",
         tipo_emprego == "Geral") %>%
  # gráfico
  ggplot(aes(x = ano, y = salario_hora, fill = tipo)) +
  # barras
  geom_bar(stat = "identity", position = position_dodge2()) +
  # título
  labs(title = "Remuneração média por hora trabalhada no Distrito Federal (Geral, Não Técnicos)", x = "", 
       y = "Remuneração média/hora (R$)", fill = "") +
  # cores
  scale_fill_manual(values = c("#46a462","#2e818d","#2960a7","#f2cb64","#a6a6a6")) +
  # remover eixos
  theme_minimal(base_size = 15) +
  # posição da legenda
  theme(panel.grid.major.x = element_blank(),legend.position = "bottom")

## Todos ----
### Celetistas ----
remun_media %>%
  # alterandovariável de ano
  mutate(ano = as.character(ano)) %>%
  # filtrando variáveis de interesse
  filter(tipo_emprego == "Celetista") %>%
  # gráfico
  ggplot(aes(x = ano, y = salario_hora, fill = tipo)) +
  # barras
  geom_bar(stat = "identity", position = position_dodge2()) +
  # título
  labs(title = "Remuneração média por hora trabalhada no Distrito Federal (Celetistas, Todos)", x = "", 
       y = "Remuneração média/hora (R$)", fill = "") +
  # cores
  scale_fill_manual(values = c("#46a462","#3e9974","#2e818d","#2b7398", "#2960a7","#2b597a","#f2cb64","#a6a6a6")) +
  # remover eixos
  theme_minimal(base_size = 15) +
  # posição da legenda
  theme(panel.grid.major.x = element_blank(),legend.position = "bottom")

### Estatutários ----
remun_media %>%
  # alterandovariável de ano
  mutate(ano = as.character(ano)) %>%
  # filtrando variáveis de interesse
  filter(tipo_emprego == "Estatutário") %>%
  # gráfico
  ggplot(aes(x = ano, y = salario_hora, fill = tipo)) +
  # barras
  geom_bar(stat = "identity", position = position_dodge2()) +
  # título
  labs(title = "Remuneração média por hora trabalhada no Distrito Federal (Estatutários, Todos)", x = "", 
       y = "Remuneração média/hora (R$)", fill = "") +
  # cores
  scale_fill_manual(values = c("#46a462","#3e9974" ,"#2e818d","#2b7398" ,"#2960a7","#2b597a" ,"#f2cb64","#a6a6a6")) +
  # remover eixos
  theme_minimal(base_size = 15) +
  # posição da legenda
  theme(panel.grid.major.x = element_blank(),legend.position = "bottom")


### Outros ----
remun_media %>%
  # alterandovariável de ano
  mutate(ano = as.character(ano)) %>%
  # filtrando variáveis de interesse
  filter(tipo_emprego == "Outros") %>%
  # gráfico
  ggplot(aes(x = ano, y = salario_hora, fill = tipo)) +
  # barras
  geom_bar(stat = "identity", position = position_dodge2()) +
  # título
  labs(title = "Remuneração média por hora trabalhada no Distrito Federal (Outros, Todos)", x = "", 
       y = "Remuneração média/hora (R$)", fill = "") +
  # cores
  scale_fill_manual(values = c("#46a462","#3e9974" ,"#2e818d","#2b7398" ,"#2960a7","#2b597a", "#f2cb64","#a6a6a6")) +
  # remover eixos
  theme_minimal(base_size = 15) +
  # posição da legenda
  theme(panel.grid.major.x = element_blank(),legend.position = "bottom")

### Geral ----
remun_media %>%
  # alterandovariável de ano
  mutate(ano = as.character(ano)) %>%
  # filtrando variáveis de interesse
  filter(tipo_emprego == "Geral") %>%
  # gráfico
  ggplot(aes(x = ano, y = salario_hora, fill = tipo)) +
  # barras
  geom_bar(stat = "identity", position = position_dodge2()) +
  # título
  labs(title = "Remuneração média por hora trabalhada no Distrito Federal (Geral, Todos)", x = "", 
       y = "Remuneração média/hora (R$)", fill = "") +
  # cores
  scale_fill_manual(values = c("#46a462","#3e9974" ,"#2e818d","#2b7398" ,"#2960a7","#2b597a", "#f2cb64","#a6a6a6")) +
  # remover eixos
  theme_minimal(base_size = 15) +
  # posição da legenda
  theme(panel.grid.major.x = element_blank(),legend.position = "bottom")


#### Scrip gera gráficos-exemplo de ranking de ganhos e perdas salariais e de vínculos

# Limpa Diretório
rm(list=ls(all=T))
# Dados sem notação científica
options(scipen=100)

# Carregando pacotes ----
library(data.table)
library(tidyverse)
library(esquisse)

# Carregabdo Base  ----
cbo_variacao <- read.csv2('produto/csv/base-cbos-variacao-sal-vinc.csv')

#esquisser(cbo_variacao)

# Ranking Ocupacional ----
## Técnico ----
### Ranking Ganho Ocupacional Técnica ----
cbo_variacao %>%
  # Filtros: Remover NA's, remover CBOS de Vínculos Baixos
  filter(!is.na(variacao_vinculos2012_2021),
         tipo_cbo == "tecnica",
         mediana_vinculos2012_2021 >= 200) %>% 
  # variáveis de interesse
  select(nome_cbo_ocupacao, variacao_vinculos2012_2021) %>%
  # transformação
  rename(cbo = nome_cbo_ocupacao, variacao = variacao_vinculos2012_2021) %>% 
  # rankeamento
  arrange(desc(variacao)) %>%
  # recorte
  slice(1:10) %>%
  # gráfico
  ggplot(aes(x = reorder(cbo,variacao), y = variacao)) +
  # barras
  geom_col(fill = "#46a462", width = 0.8) +
  # rótulos
  geom_text(aes(x = reorder(cbo,variacao), y = .05, label = scales::percent(round(variacao,4)), fontface = "bold"), 
            show.legend = F, 
            col = "white") +
  # título
  labs(title = "Ranking Ganho Ocupacinal 2012-2021 (CBOs técnicas)", x = "", y = "") +
  # eixo y
  scale_y_continuous(label = scales::percent) +
  # inverter eixos e remover eixos
  coord_flip() + theme_minimal(base_size = 15) +
  # remover linhas de grade
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

### Ranking Perda Ocupacional Técnica ----
cbo_variacao %>%
  # Filtros: Remover NA's, remover CBOS de Vínculos Baixos
  filter(!is.na(variacao_vinculos2012_2021),
         tipo_cbo == "tecnica",
         mediana_vinculos2012_2021 >= 200) %>% 
  # variáveis de interesse
  select(nome_cbo_ocupacao, variacao_vinculos2012_2021) %>%
  # transformação
  rename(cbo = nome_cbo_ocupacao, variacao = variacao_vinculos2012_2021) %>% 
  # rankeamento
  arrange(variacao) %>%
  # recorte
  slice(1:10) %>%
  # gráfico
  ggplot(aes(x = reorder(cbo,-variacao), y = variacao)) +
  # barras
  geom_col(fill = "#EB6560", width = 0.8) +
  # rótulos
  geom_text(aes(x = reorder(cbo,variacao), y = variacao, label = scales::percent(variacao), fontface = "bold"), 
            show.legend = F, 
            col = "white", hjust = "left") +
  # título
  labs(title = "Ranking Perda Ocupacinal 2012-2021 (CBOs técnicas)", x = "", y = "") +
  # eixo y
  scale_y_continuous(label = scales::percent) +
  # inverter eixos e remover eixos
  coord_flip() + theme_minimal(base_size = 15) +
  # remover linhas de grade
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.title = element_text(hjust = 1))

## Geral ----
### Ranking Ganho Ocupacional Técnica ----
cbo_variacao %>%
  # Filtros: Remover NA's, remover CBOS de Vínculos Baixos
  filter(!is.na(variacao_vinculos2012_2021),
         tipo_cbo == "geral",
         mediana_vinculos2012_2021 >= 200) %>% ## Problemas
  # variáveis de interesse
  select(nome_cbo_ocupacao, variacao_vinculos2012_2021) %>%
  # transformação
  rename(cbo = nome_cbo_ocupacao, variacao = variacao_vinculos2012_2021) %>% 
  # rankeamento
  arrange(desc(variacao)) %>%
  # recorte
  slice(1:10) %>%
  # gráfico
  ggplot(aes(x = reorder(cbo,variacao), y = variacao)) +
  # barras
  geom_col(fill = "#46a462", width = 0.8) +
  # rótulos
  geom_text(aes(x = reorder(cbo,variacao), y = .17, label = scales::percent(variacao), fontface = "bold"), 
            show.legend = F, 
            col = "white") +
  # título
  labs(title = "Ranking Ganho Ocupacinal 2012-2021 (Geral)", x = "", y = "") +
  # eixo y
  scale_y_continuous(label = scales::percent) +
  # inverter eixos e remover eixos
  coord_flip() + theme_minimal(base_size = 15) +
  # remover linhas de grade
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.title = element_text(hjust = 1))

### Ranking Perda Ocupacional Técnica ----
cbo_variacao %>%
  # Filtros: Remover NA's, remover CBOS de Vínculos Baixos
  filter(!is.na(variacao_vinculos2012_2021),
         tipo_cbo == "geral",
         vinculos_2012 >= 10) %>% ## Problemas
  # variáveis de interesse
  select(nome_cbo_ocupacao, variacao_vinculos2012_2021) %>%
  # transformação
  rename(cbo = nome_cbo_ocupacao, variacao = variacao_vinculos2012_2021) %>% 
  # rankeamento
  arrange(variacao) %>%
  # recorte
  slice(1:10) %>%
  # gráfico
  ggplot(aes(x = reorder(cbo,-variacao), y = variacao)) +
  # barras
  geom_col(fill = "#EB6560", width = 0.8) +
  # rótulos
  geom_text(aes(x = reorder(cbo,variacao), y = variacao, label = scales::percent(round(variacao,4)), fontface = "bold"), 
            show.legend = F, 
            col = "white", hjust = "left") +
  # título
  labs(title = "Ranking Perda Ocupacinal 2012-2021 (Geral)", x = "", y = "") +
  # eixo y
  scale_y_continuous(label = scales::percent) +
  # inverter eixos e remover eixos
  coord_flip() + theme_minimal(base_size = 15) +
  # remover linhas de grade
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Ranking Salarial ----
## Técnico ----
### Ranking Ganho Salarial Técnica ----
cbo_variacao %>%
  # Filtros: Remover NA's, remover CBOS de Vínculos Baixos
  filter(!is.na(variacao_rendimento2012_2021),
         tipo_cbo == "tecnica",
         mediana_vinculos2012_2021 >= 200) %>% 
  # variáveis de interesse
  select(nome_cbo_ocupacao, variacao_rendimento2012_2021) %>%
  # transformação
  rename(cbo = nome_cbo_ocupacao, variacao = variacao_rendimento2012_2021) %>% 
  # rankeamento
  arrange(desc(variacao)) %>%
  # recorte
  slice(1:10) %>%
  # gráfico
  ggplot(aes(x = reorder(cbo,variacao), y = variacao)) +
  # barras
  geom_col(fill = "#46a462", width = 0.8) +
  # rótulos
  geom_text(aes(x = reorder(cbo,variacao), y = .05, label = scales::percent(round(variacao,4)), fontface = "bold"), 
            show.legend = F, 
            col = "white") +
  # título
  labs(title = "Ranking Ganho Salarial 2012-2021 (CBOs técnicas)", x = "", y = "") +
  # eixo y
  scale_y_continuous(label = scales::percent) +
  # inverter eixos e remover eixos
  coord_flip() + theme_minimal(base_size = 10) +
  # remover linhas de grade
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

### Ranking Perda Salarial Técnica ----
cbo_variacao %>%
  # Filtros: Remover NA's, remover CBOS de Vínculos Baixos
  filter(!is.na(variacao_rendimento2012_2021),
         tipo_cbo == "tecnica",
         mediana_vinculos2012_2021 >= 200) %>% 
  # variáveis de interesse
  select(nome_cbo_ocupacao, variacao_rendimento2012_2021) %>%
  # transformação
  rename(cbo = nome_cbo_ocupacao, variacao = variacao_rendimento2012_2021) %>% 
  # rankeamento
  arrange(variacao) %>%
  # recorte
  slice(1:10) %>%
  # gráfico
  ggplot(aes(x = reorder(cbo,-variacao), y = variacao)) +
  # barras
  geom_col(fill = "#EB6560", width = 0.8) +
  # rótulos
  geom_text(aes(x = reorder(cbo,variacao), y = variacao, label = scales::percent(round(variacao,4)), fontface = "bold"), 
            show.legend = F, 
            col = "white", hjust = "left") +
  # título
  labs(title = "Ranking Perda Salarial 2012-2021 (CBOs técnicas)", x = "", y = "") +
  # eixo y
  scale_y_continuous(label = scales::percent) +
  # inverter eixos e remover eixos
  coord_flip() + theme_minimal(base_size = 15) +
  # remover linhas de grade
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.title = element_text(hjust = 1))

## Geral ----
### Ranking Ganho Salarial Técnica ----
cbo_variacao %>%
  # Filtros: Remover NA's, remover CBOS de Vínculos Baixos
  filter(!is.na(variacao_rendimento2012_2021),
         tipo_cbo == "geral",
         vinculos_2012>= 10) %>% ## Problemas
  # variáveis de interesse
  select(nome_cbo_ocupacao, variacao_rendimento2012_2021) %>%
  # transformação
  rename(cbo = nome_cbo_ocupacao, variacao = variacao_rendimento2012_2021) %>% 
  # rankeamento
  arrange(desc(variacao)) %>%
  # recorte
  slice(1:10) %>%
  # gráfico
  ggplot(aes(x = reorder(cbo,variacao), y = variacao)) +
  # barras
  geom_col(fill = "#46a462", width = 0.8) +
  # rótulos
  geom_text(aes(x = reorder(cbo,variacao), y = .15, label = scales::percent(round(variacao,4)), fontface = "bold"), 
            show.legend = F, 
            col = "white") +
  # título
  labs(title = "Ranking Ganho Salarial 2012-2021 (Geral)", x = "", y = "") +
  # eixo y
  scale_y_continuous(label = scales::percent) +
  # inverter eixos e remover eixos
  coord_flip() + theme_minimal(base_size = 15) +
  # remover linhas de grade
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.title = element_text(hjust = 1))

### Ranking Perda Salarial Técnica ----
cbo_variacao %>%
  # Filtros: Remover NA's, remover CBOS de Vínculos Baixos
  filter(!is.na(variacao_rendimento2012_2021),
         tipo_cbo == "geral",
         mediana_vinculos2012_2021 >= 200) %>% ## Problemas
  # variáveis de interesse
  select(nome_cbo_ocupacao, variacao_rendimento2012_2021) %>%
  # transformação
  rename(cbo = nome_cbo_ocupacao, variacao = variacao_rendimento2012_2021) %>% 
  # rankeamento
  arrange(variacao) %>%
  # recorte
  slice(1:10) %>%
  # gráfico
  ggplot(aes(x = reorder(cbo,-variacao), y = variacao)) +
  # barras
  geom_col(fill = "#EB6560", width = 0.8) +
  # rótulos
  geom_text(aes(x = reorder(cbo,variacao), y = variacao, label = scales::percent(round(variacao,4)), fontface = "bold"), 
            show.legend = F, 
            col = "white", hjust = "left") +
  # título
  labs(title = "Ranking Perda Salarial 2012-2021 (Geral)", x = "", y = "") +
  # eixo y
  scale_y_continuous(label = scales::percent) +
  # inverter eixos e remover eixos
  coord_flip() + theme_minimal(base_size = 15) +
  # remover linhas de grade
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Gerar base Técnologos

#Pacotes ----
library(data.table)
library(tidyverse)
library(readxl)
#Diretorio ----
diretorio <- c("P:/RECENTES/DIEPS/GEFAPS/GEFAPS/2022/Panorama da Capacitação/rais-panorama/dados/tecnologo/")



#Lê as bases e empilha ----
base <- purrr::map_dfr(paste0(diretorio, dir(diretorio)), # Para cada uma das bases do diretório
               purrr::possibly(~read_csv(.x), otherwise = NULL)) 


#Limpeza ----
## Corrige carcateres dos cursos
Encoding(base$NO_CURSO) <- 'latin1'
base$curso <- base$NO_CURSO
## Variável Fator de Raça/Cor
base$racacor <- factor(base$TP_COR_RACA,
                      levels = c(0:5,9),
                      labels = c("Não declarado",
                                 "Branca",
                                 "Preta",
                                 "Parda",
                                 "Amarela",
                                 "Indígena",
                                 "Não resposta"))
## Variável Fator de Sexo
base$sexo <- factor(base$TP_SEXO,
                      levels = c(0:2),
                      labels = c("Não declarado",
                                 "Feminino",
                                 "Masculino"))
## Variável Fator de Categoria Administrativa
base$cat_adm <- factor(base$TP_CATEGORIA_ADMINISTRATIVA,
                       levels = c(1:7),
                       labels = c("Pública Federal",
                                  "Pública Estadual",
                                  "Pública Municipal",
                                  "Privada com fins lucrativos",
                                  "Privada sem fins lucrativos",
                                  "Privada - Particular em sentido estrito",
                                  "Especial"))
## Variável Fator de Público/Privado
base$categoria_geral <- case_when(base$TP_CATEGORIA_ADMINISTRATIVA %in% c(1:3)~"Pública",
                            TRUE~"Privada")
## Variável Fator de esfera Pública
base$categoria <- case_when(base$TP_CATEGORIA_ADMINISTRATIVA == 1~"Pública Federal",
                             base$TP_CATEGORIA_ADMINISTRATIVA == 2~"Pública Estadual",
                             base$TP_CATEGORIA_ADMINISTRATIVA == 3~"Pública Municipal",
                             TRUE~"Privada")



base %>% 
  filter(CO_UF == 53)%>%
  group_by(ano_censo) %>%
  summarise(matriculas_df = sum(IN_MATRICULA == 1 ))


#Gráficos ----
## Gráfico 1
graf1 <- base %>% 
  filter(CO_UF == 53) %>%
  group_by(ano_censo,categoria) %>%
  summarise(matriculas = sum(IN_MATRICULA == 1),
            tipo = "Distrito Federal") %>%
  mutate(perc = matriculas/sum(matriculas)) %>%
  rbind(base %>% 
          group_by(ano_censo,categoria) %>%
          summarise(matriculas = sum(IN_MATRICULA == 1),
                    tipo = "Brasil") %>%
          mutate(perc = matriculas/sum(matriculas)))

write_excel_csv2(graf1,paste0(diretorio,"produto/matriculas_br_df.csv"))

graf1 <- base %>% 
  filter(CO_UF == 53) %>%
  group_by(ano_censo,categoria) %>%
  summarise(matriculas = sum(IN_MATRICULA == 1),
            tipo = "Distrito Federal") %>%
  mutate(perc = matriculas/sum(matriculas)) %>%
  rbind(base %>% 
          group_by(ano_censo,categoria) %>%
          summarise(matriculas = sum(IN_MATRICULA == 1),
                    tipo = "Brasil") %>%
          mutate(perc = matriculas/sum(matriculas))) %>%
  ggplot(aes(x = ano_censo, y = perc, fill = categoria)) +
  geom_col(width = .9) + theme_minimal(base_size = 15) +
  geom_text(aes(x = ano_censo, y = perc,
                label = scales::percent(ifelse(perc > 0.039,
                                               round(perc, 2),
                                               NA))), 
            position = position_stack(vjust = 0.6),
            size = 3.5,
            color = "white")+
  labs(x = "",y= "", fill = "") +
  scale_x_continuous(breaks = seq(2010,2019, by = 1),
                     expand = c(0,0.2)) +
  scale_y_continuous(labels = scales::percent,
                     expand = c(0,0.01)) +
  jcolors::scale_fill_jcolors(palette = "pal10") +
  facet_grid(cols = vars(tipo)) + theme(legend.position = "bottom",
                                        panel.spacing = unit(0, "lines"),
                                        panel.grid.major = element_blank(), 
                                        panel.grid.minor = element_blank())

graf1
## Gráfico 2

graf2 <- base %>% 
  filter(CO_UF == 53) %>%
  group_by(ano_censo) %>%
  summarise(matriculas = sum(IN_MATRICULA == 1),
            tipo = "Distrito Federal") %>%
  rbind(base %>% 
          group_by(ano_censo) %>%
          summarise(matriculas = sum(IN_MATRICULA == 1),
                    tipo = "Brasil"))

write_excel_csv2(graf2,paste0(diretorio,"produto/serie_matriculas.csv"))





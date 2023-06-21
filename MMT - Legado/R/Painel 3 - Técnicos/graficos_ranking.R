#### Scrip gera gráficos-exemplo de ranking de ganhos e perdas salariais e de vínculos
 
# Limpa Diretório
   rm(list=ls(all=T))
# Dados sem notação científica
   options(scipen=100)
 
# Carregando pacotes ----
 library(data.table)
 library(tidyverse)
 
# Carregabdo Base  ----
cbo_variacao <- read.csv2('produto/csv/base-cbos-familia-tipo-emprego-variacao-sal-vinc.csv')
   
cbo_variacao <- cbo_variacao %>% filter(mediana_vinculos2012_2021 >= 200,
                                        vinculos_2021 >= 200)
   
# Salários   
base_sal <- cbo_variacao %>% 
  select(nome_cbo_familia,tipo_cbo,tipo_emprego,
                                variacao = variacao_rendimento2012_2021) %>%
  mutate(situacao = ifelse(variacao > 0, "Ganho","Perda"),
         tipo_cbo = ifelse(tipo_cbo == "geral", "CBO Geral", "CBO Técnica"),
         tipo = "Salarial")
# Vínculos
base_vinc <- cbo_variacao %>% 
  select(nome_cbo_familia,tipo_cbo,tipo_emprego,
                                variacao = variacao_vinculos2012_2021) %>%
  mutate(situacao = ifelse(variacao > 0, "Ganho","Perda"),
         tipo_cbo = ifelse(tipo_cbo == "geral", "CBO Geral", "CBO Técnica"),
         tipo = "Ocupacional")

# Bind
base <- rbind(base_sal, base_vinc) %>% 
  filter(!is.na(variacao))

# Variável título de filtro
base$titulo <- paste0("Ranking ",base$situacao," ",base$tipo," (",base$tipo_emprego,", ",base$tipo_cbo,")")

# Função ----
gera_graficos <- function(dados){

for (i in unique(dados$titulo)) {
  
  data <- dados %>%
    # Filtros: Remover NA's, remover CBOS de Vínculos Baixos
    filter(titulo == i) %>% 
    # rankeamento
    arrange(if (str_detect(i,"Ganho") == "TRUE") desc(variacao) else variacao) %>%
    # recorte
    slice(1:10) %>% 
    mutate(indice = c(10:1))
  
    # gráfico
    graf <- ggplot(data = data ,aes(x = reorder(nome_cbo_familia,indice), y = variacao)) +
    # barras
    geom_bar(stat = "identity",fill = "#46a462", width = 0.8) +
    # rótulos
    geom_label(aes(x = nome_cbo_familia, y = variacao, label = scales::percent(round(variacao,4)), fontface = "bold"), 
              show.legend = F, 
              col = "black",
              col = "white") +
    # título
    labs(title = i, x = "", y = "") +
    # eixo y
    scale_y_continuous(label = scales::percent) +
    # inverter eixos e remover eixos
    coord_flip() + theme_minimal(base_size = 10) +
    # remover linhas de grade
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
  print(graf)
}
}

gera_graficos(base)


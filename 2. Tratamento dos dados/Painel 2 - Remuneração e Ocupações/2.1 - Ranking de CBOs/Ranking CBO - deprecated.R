#'*Script que calcula a remuneração média por hora trabalhada*

# Carregar pacotes ----

library(tidyverse)# Manipulação dos dados
library(imputeTS) # SUbstituir NA's
library(readxl)   # Leitura de Arquivos em .xlsx

`%notin%` <- Negate(`%in%`)           # Função de filtro
options(readr.show_col_types = FALSE) # Omitir formato das colunas no console

# Importação dos dados ----

rais <- readRDS("../1. Extração dos dados/RAIS/Dados/RAIS.RDS")

estrutura_cbo <- readRDS("../1. Extração dos dados/Dicionários/Dicionário CBO.RDS") |>
  select(cboocupacao2002, nome_cbo_ocupacao, 
         cbo_subgrupo_principal, nome_cbo_subgrupo_principal)

# Tratamento dos dados ----

## Retirar 2011 da base ----

rais <- rais |>
  filter(referencia != 2011,
         !is.na(cboocupacao2002))

# rais |>
#   filter(tipovinculo == 1,
#          cboocupacao2002 == "342520") |>
#   group_by(referencia) |>
#   summarise(n = n())

## Dados do ano mais recente ----

dados_recente <- rais |>
  filter(tipovinculo == 1,                 # Celetista
         referencia == max(referencia)) |> # Filtar pelo ano mais recente
  group_by(cboocupacao2002) %>%
  summarise(referencia = max(referencia),
            vinculos_recente = n(),
            mediana_rendimento_recente = median(salario_dez_defl, na.rm = TRUE), # Salário médio
            mediana_salario_hora_recente = median(salario_hora, na.rm = TRUE))   # Hora de trabalho média

## Dados do ano base ----

dados_base <- rais %>%
  filter(tipovinculo == 1,
         referencia == max(referencia) - 1) %>% # Período base - t-1 (2020)
  group_by(cboocupacao2002) %>%
  summarise(vinculos_base = n(),
            mediana_rendimento_base = median(salario_dez_defl, na.rm = TRUE),  # Salário médio
            mediana_salario_hora_base = median(salario_hora, na.rm = TRUE)) |> # Hora de trabalho média
  mutate_if(is.integer, as.numeric)

## Dados históricos ----

#salario_vinculo_cbo <- rais |>
#  filter(tipovinculo == 1) |> # Celetista 
#  group_by(cboocupacao2002, referencia) %>%
#  summarise(vinculos = n(),
#            mediana_rendimento = median(salario_dez_defl, na.rm = TRUE), # Salário médio
#            mediana_salario_hora = median(salario_hora, na.rm = TRUE))   # Hora de trabalho médio

## Mediana do histórico ----

#dados_historico <- salario_vinculo_cbo |>
#  group_by(cboocupacao2002) |>
#  summarise(mediana_vinculos_historico = median(vinculos),              # Mediana de vínculos histórico
#            mediana_rendimento_historico = median(mediana_rendimento))  # Mediana de rendimentos histórico

## Merge das bases ----

dados <- dados_base |>
  left_join(dados_recente, by = "cboocupacao2002") |>
  #left_join(dados_historico, by = "cboocupacao2002") |>
  #filter(mediana_vinculos_historico > 12) |>
  mutate(variacao_rendimento = (mediana_rendimento_recente / mediana_rendimento_base) - 1,
         variacao_vinculos = (vinculos_recente / vinculos_base) - 1)

dados <- dados %>%
  left_join(estrutura_cbo, by = "cboocupacao2002") %>%
  #na_replace(.,"null") %>%
  #replace(is.na(.), "null") %>%
  #mutate_at(vars(cboocupacao2002, cbo_subgrupo_principal), ~ paste0('"', ., '"')) %>%
  select(cboocupacao2002, 
         nome_cbo_ocupacao,
         mediana_vinculos = mediana_vinculos_historico,
         mediana_rendimentos = mediana_rendimento_historico,
         vinculos_ultimo_ano = vinculos_recente,
         mediana_rendimento_ultimo_ano = mediana_rendimento_recente,
         mediana_salario_hora_ultimo_ano = mediana_salario_hora_recente,
         vinculos_ano_base = vinculos_base,                     # Informações de 2012, discutir a inclusão na reunião
         mediana_rendimento_ano_base = mediana_rendimento_base,     # Informações de 2012, discutir a inclusão na reunião
         mediana_salario_hora_ano_base = mediana_salario_hora_base, # Informações de 2012, discutir a inclusão na reunião
         variacao_rendimentos = variacao_rendimento,
         variacao_vinculos = variacao_vinculos,
         cbo = nome_cbo_subgrupo_principal,
         codigo_cbo = cbo_subgrupo_principal)


#write_excel_csv2(dados, "W:/GEFAPS/2023/MMT/3. Gráficos/Dados/Ranking.csv")

# Gráficos de Exemplo
graf <- dados

quebra <- function(coluna, numPalavras) {
  colunaNova <- character(length(coluna))  # Vetor vazio para armazenar os valores da coluna modificada
  
  for (i in seq_along(coluna)) {
    palavras <- strsplit(coluna[i], " ")[[1]]  # Dividir a célula da coluna em palavras
    resultado <- character(0)  # Vetor vazio para armazenar as palavras
    
    for (j in seq_along(palavras)) {
      resultado <- c(resultado, palavras[j])  # Adicionar cada palavra ao vetor resultado
      
      if (j %% numPalavras == 0 && j != length(palavras)) {
        resultado <- c(resultado, "\n")  # Inserir quebra de linha a cada numPalavras palavras
      }
    }
    
    colunaNova[i] <- paste(resultado, collapse = " ")  # Combinar as palavras novamente em uma única string e atribuir à coluna modificada
  }
  
  return(colunaNova)
}
formata <- function(x){paste0(format(round(x,3)*100,decimal.mark = ",",nsmall = 1),"%")}

graf$nome_cbo_ocupacao <- quebra(graf$nome_cbo_ocupacao,4)

(graf |> 
    # Filtros: Remover NA's, remover CBOS de Vínculos Baixos
    filter(!is.na(variacao_vinculos)) |> 
    # variáveis de interesse
    select(cbo = nome_cbo_ocupacao, var = variacao_vinculos) |>
    # Transformação da varável de Variação
    mutate(var = var/100) |> 
    # rankeamento
    arrange(desc(var)) |>
    # recorte
    slice(1:10) %>%
    # gráfico
    ggplot(aes(x = reorder(cbo,var), y = var)) +
    # barras
    geom_col(fill = "#0f6bb5", width = 0.8) +
    # rótulos
    geom_text(aes(x = reorder(cbo,var), y = .06, 
              label = scales::percent(round(var,3)), fontface = "bold"), 
              show.legend = F, 
              col = "white") +
    # título
    labs(title = "RANKING - GANHO VÍNCULOS", x = "", y = "") +
    # eixo y
    scale_y_continuous(label = scales::percent) +
    # inverter eixos e remover eixos
    coord_flip() + theme_minimal(base_size = 15) +
    # fontes do gráfico
    theme(text=element_text(size=17,family = "Arial"),
          panel.grid.major = element_blank())+ 
    # remover linhas de grade
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())) |> plotly::ggplotly()

(graf |> 
    # Filtros: Remover NA's, remover CBOS de Vínculos Baixos
    filter(!is.na(variacao_vinculos)) |> 
    # variáveis de interesse
    select(cbo = nome_cbo_ocupacao, var = variacao_vinculos) |>
    # Transformação da varável de Variação
    mutate(var = var) |> 
    # rankeamento
    arrange(var) |>
    # recorte
    slice(1:10) %>%
    # gráfico
    ggplot(aes(x = reorder(cbo,-var), y = var)) +
    # barras
    geom_col(fill = "#f04933", width = 0.8) +
    # rótulos
    geom_text(aes(x = reorder(cbo,var), y = var+.09, 
                  label = scales::percent(round(var,3)), fontface = "bold"), 
              show.legend = F, 
              col = "white",
              hjust = "left") +
    # título
    labs(title = "RANKING - PERDA VÍNCULOS", x = "", y = "") +
    # eixo y
    scale_y_continuous(label = scales::percent) +
    # inverter eixos e remover eixos
    coord_flip() + theme_minimal(base_size = 15) +
    # fontes do gráfico
    theme(text=element_text(size=17,family = "Arial"),
          panel.grid.major = element_blank())+ 
    # remover linhas de grade
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())) |> plotly::ggplotly()


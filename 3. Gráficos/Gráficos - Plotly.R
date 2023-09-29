# Carregar pacotes ----

library(tidyverse)
library(readr)
library(plotly)

`%notin%` <- Negate(`%in%`)           # Função de filtro
options(readr.show_col_types = FALSE) # Omitir formato das colunas no console

# Importação dos dados ----

dados <- lapply(paste0("../2. Tratamento dos dados/Painel 2 - Remuneração e Ocupações/Resultados/", dir("../2. Tratamento dos dados/Painel 2 - Remuneração e Ocupações/Resultados")), read.csv2)
names(dados) <- c("ranking_CBO", "remun_media", "num_vinc", "rotatividade")

## Ranking de CBOs

### Ranking - ganho de vínculos ----

dados$ranking_CBO |>
   mutate(nome_cbo_familia = quebra(nome_cbo_familia, 4)) |>
   arrange(desc(variacao_vinculos)) |>
   slice(1:10) |>
   plot_ly(x = ~variacao_vinculos * 100) |>
   add_trace(y = ~nome_cbo_familia,
             type = "bar",
             color = I("#176180")) |>
   add_annotations(x = ~variacao_vinculos, 
                   y = ~nome_cbo_familia,
                   xanchor = "left",
                   yanchor = "rigth",
                   text = ~paste(round(variacao_vinculos * 100, 2), "%"),
                   textposition = "middle",
                   font = list(color = "white"),
                   showarrow = FALSE) |>
   config(displayModeBar = FALSE) |>
   layout(hovermode = "y",
          yaxis = list(title = "",
                       showgrid = FALSE,
                       showline = TRUE,
                       categoryorder = "total ascending"),
          xaxis = list(title = "",
                       hoverformat = ".1f",
                       showgrid = FALSE,
                       showline = TRUE,
                       ticksuffix = "%"),
          legend = list(orientation = "h",
                        title = list(text = "")),
          barmode = "relative",
          uniformtext = list(minsize = 8,
                             mode = "hide"))

### Ranking - perda de vínculos ----

dados$ranking_CBO |>
   mutate(nome_cbo_familia = quebra(nome_cbo_familia, 4)) |>
   arrange(variacao_vinculos) |>
   slice(1:10) |>
   plot_ly(x = ~variacao_vinculos * 100) |>
   add_trace(y = ~nome_cbo_familia,
             type = "bar",
             color = I("#f04933")) |>
   add_annotations(x = ~variacao_vinculos, 
                   y = ~nome_cbo_familia,
                   xanchor = "right",
                   yanchor = "rigth",
                   text = ~paste(round(variacao_vinculos * 100, 2), "%"),
                   textposition = "middle",
                   font = list(color = "white"),
                   showarrow = FALSE) |>
   config(displayModeBar = FALSE) |>
   layout(hovermode = "y",
          yaxis = list(title = "",
                       showgrid = FALSE,
                       showline = TRUE,
                       categoryorder = "total descending"),
          xaxis = list(title = "",
                       hoverformat = ".1f",
                       showgrid = FALSE,
                       showline = TRUE,
                       ticksuffix = "%"),
          legend = list(orientation = "h",
                        title = list(text = "")),
          barmode = "relative",
          uniformtext = list(minsize = 8,
                             mode = "hide"))

### Ranking - ganho salarial ----

dados$ranking_CBO |>
   mutate(nome_cbo_familia = quebra(nome_cbo_familia, 4)) |>
   arrange(desc(variacao_rendimento)) |>
   slice(1:10) |>
   plot_ly(x = ~variacao_rendimento * 100) |>
   add_trace(y = ~nome_cbo_familia,
             type = "bar",
             color = I("#176180")) |>
   add_annotations(x = ~variacao_rendimento, 
                   y = ~nome_cbo_familia,
                   xanchor = "left",
                   yanchor = "rigth",
                   text = ~paste(round(variacao_rendimento * 100, 2), "%"),
                   textposition = "middle",
                   font = list(color = "white"),
                   showarrow = FALSE) |>
   config(displayModeBar = FALSE) |>
   layout(hovermode = "y",
          yaxis = list(title = "",
                       showgrid = FALSE,
                       showline = TRUE,
                       categoryorder = "total ascending"),
          xaxis = list(title = "",
                       hoverformat = ".1f",
                       showgrid = FALSE,
                       showline = TRUE,
                       ticksuffix = "%"),
          legend = list(orientation = "h",
                        title = list(text = "")),
          barmode = "relative",
          uniformtext = list(minsize = 8,
                             mode = "hide"))

### Ranking - perda salarial ----

dados$ranking_CBO |>
   mutate(nome_cbo_familia = quebra(nome_cbo_familia, 4)) |>
   arrange(variacao_rendimento) |>
   slice(1:10) |>
   plot_ly(x = ~variacao_rendimento * 100) |>
   add_trace(y = ~nome_cbo_familia,
             type = "bar",
             color = I("#f04933")) |>
   add_annotations(x = ~variacao_rendimento, 
                   y = ~nome_cbo_familia,
                   xanchor = "right",
                   yanchor = "rigth",
                   text = ~paste(round(variacao_rendimento * 100, 2), "%"),
                   textposition = "middle",
                   font = list(color = "white"),
                   showarrow = FALSE) |>
   config(displayModeBar = FALSE) |>
   layout(hovermode = "y",
          yaxis = list(title = "",
                       showgrid = FALSE,
                       showline = TRUE,
                       categoryorder = "total descending"),
          xaxis = list(title = "",
                       hoverformat = ".1f",
                       showgrid = FALSE,
                       showline = TRUE,
                       ticksuffix = "%"),
          legend = list(orientation = "h",
                        title = list(text = "")),
          barmode = "relative",
          uniformtext = list(minsize = 8,
                             mode = "hide"))

## Remuneração mediana ----

dados$remun_media |>
   select(-c(salario_mes, filtro, geral)) |>
   pivot_wider(names_from = tipo,
               values_from = salario_hora) |>
   plot_ly(x = ~ano) |>
   add_trace(y = ~Analfabeto,
             type = "bar",
             name = "Analfabeto",
             color = I("#d92335")) |>
   add_trace(y = ~`Até fundamental completo`,
             type = "bar",
             name = "Até fundamental completo",
             color = I("#064471")) |>
   add_trace(y = ~`Até médio completo`,
             type = "bar",
             name = "Até médio completo",
             color = I("#bd3928")) |>
   add_trace(y = ~`Superior completo*`,
             type = "bar",
             name = "Superior completo",
             color = I("#f87d28")) |>
   add_trace(y = ~`Trabalhadores não técnicos`,
             type = "bar",
             name = "Trabalhadores não técnicos",
             color = I("#0a78c7")) |>
   add_trace(y = ~`Trabalhadores técnicos`,
             type = "bar",
             name = "Trabalhadores técnicos",
             color = I("#fb8e80")) |>
   add_trace(y = ~`Técnicos de nível médio`,
             type = "bar",
             name = "Técnicos de nível médio",
             color = I("#818589")) |>
   add_trace(y = ~`Técnicos de nível superior`,
             type = "bar",
             name = "Técnicos de nível superior",
             color = I("#2b597a")) |>
   config(displayModeBar = FALSE) %>%
   layout(hovermode = "x",
          yaxis = list(title = "",
                       tickprefix = "R$", 
                       hoverformat = '.1f',
                       showgrid = FALSE,
                       showline = TRUE),
          xaxis = list(title = "",
                       tickformat = "%Y",
                       tickmode = "array",
                       tickvals = c(min(dados$remun_media$ano):max(dados$remun_media$ano)),
                       showgrid = FALSE,
                       showline = TRUE),
          legend = list(orientation = "h",
                        title = list(text = "")),
          barmode = "group")

## Número de vínculos ----

### Número absoluto ----

#### Por escolaridade ----

dados$num_vinc |>
   pivot_wider(names_from = tipo,
               values_from = vinculos) |>
   plot_ly(x = ~ano) |>
   add_trace(y = ~Analfabeto,
             type = "scatter", 
             mode = "lines+markers",
             name = "Analfabeto",
             color = I("#d92335")) |>
   add_trace(y = ~`Até fundamental completo`,
             type = "scatter", 
             mode = "lines+markers",
             name = "Até fundamental completo",
             color = I("#064471")) |>
   add_trace(y = ~`Médio completo`,
             type = "scatter", 
             mode = "lines+markers",
             name = "Médio completo",
             color = I("#bd3928")) |>
   add_trace(y = ~`Superior completo`,
             type = "scatter", 
             mode = "lines+markers",
             name = "Superior completo",
             color = I("#bdd928")) |>
   config(displayModeBar = FALSE) |>
   layout(hovermode = "x",
          yaxis = list(title = "",
                       hoverformat = ".0f",
                       showgrid = FALSE,
                       showline = TRUE),
          xaxis = list(title = "",
                       tickformat = "%Y",
                       tickmode = "array",
                       tickvals = c(min(dados$num_vinc$ano):max(dados$num_vinc$ano)),
                       showgrid = FALSE,
                       showline = TRUE),
          legend = list(orientation = "h",
                        title = list(text = "")),
          barmode = "group")

#### Por atividade ----

dados$num_vinc |>
   pivot_wider(names_from = tipo,
               values_from = vinculos) |>
   plot_ly(x = ~ano) |>
   add_trace(y = ~`Técnico de nível médio`,
             type = "scatter", 
             mode = "lines+markers",
             name = "Técnicos de nível médio",
             color = I("#065411")) |>
   add_trace(y = ~`Técnico de nível superior`,
             type = "scatter", 
             mode = "lines+markers",
             name = "Técnicos de nível superior",
             color = I("#cabd28")) |>
   config(displayModeBar = FALSE) |>
   layout(hovermode = "x",
          yaxis = list(title = "",
                       hoverformat = ".0f",
                       showgrid = FALSE,
                       showline = TRUE),
          xaxis = list(title = "",
                       tickformat = "%Y",
                       tickmode = "array",
                       tickvals = c(min(dados$num_vinc$ano):max(dados$num_vinc$ano)),
                       showgrid = FALSE,
                       showline = TRUE),
          legend = list(orientation = "h",
                        title = list(text = "")),
          barmode = "group")

### Variação ----

dados$num_vinc |>
   mutate(variacao = ((vinculos / lag(vinculos, n = 1)) - 1) * 100) |>
   filter(ano > 2011) |>
   pivot_wider(id_cols = -vinculos,
               names_from = tipo,
               values_from = variacao) |>
   plot_ly(x = ~ano) |>
   #add_trace(y = ~Total,
   #          type = "bar",
   #          name = "Total",
   #          color = I("#d92335")) |>
   add_trace(y = ~`Trabalhadores técnicos`,
             type = "bar",
             name = "Trabalhadores técnicos",
             color = I("#064471")) |>
   add_trace(y = ~`Trabalhadores não técnicos`,
             type = "bar",
             name = "Trabalhadores não técnicos",
             color = I("#f87d28")) |>
   config(displayModeBar = FALSE) %>%
   layout(hovermode = "x",
          yaxis = list(title = "",
                       ticksuffix = "%", 
                       hoverformat = '.2f',
                       showgrid = FALSE,
                       showline = TRUE),
          xaxis = list(title = "",
                       tickformat = "%Y",
                       tickmode = "array",
                       tickvals = c(min(dados$num_vinc$ano):max(dados$num_vinc$ano)),
                       showgrid = FALSE,
                       showline = TRUE),
          legend = list(orientation = "h",
                        title = list(text = "")),
          barmode = "group")
   
## Taxa de rotatividade ----

#### Por escolaridade ----

dados$rotatividade |>
   select(-c(filtro, n_trabalhadores:desligados)) |>
   mutate(rotatividade = rotatividade * 100) |>
   pivot_wider(names_from = tipo,
               values_from = rotatividade) |>
   plot_ly(x = ~anodeclarado) |>
   add_trace(y = ~`Analfabeto`,
             type = "scatter", 
             mode = "lines+markers",
             name = "Analfabeto",
             color = I("#AFE1AF")) |>
   add_trace(y = ~`Fundamental completo e incompleto`,
             type = "scatter", 
             mode = "lines+markers",
             name = "Fundamental completo e incompleto",
             color = I("#d92335")) |>
   add_trace(y = ~`Médio completo e incompleto`,
             type = "scatter", 
             mode = "lines+markers",
             name = "Médio completo e incompleto",
             color = I("#f87d28")) |>
   add_trace(y = ~`Superior completo`,
             type = "scatter", 
             mode = "lines+markers",
             name = "Superior completo",
             color = I("#0a78c7")) |>
   config(displayModeBar = FALSE) |>
   layout(hovermode = "x",
          yaxis = list(title = "",
                       ticksuffix = "%",
                       hoverformat = ".2f",
                       showgrid = FALSE,
                       showline = TRUE),
          xaxis = list(title = "",
                       tickformat = "%Y",
                       tickmode = "array",
                       tickvals = c(min(dados$rotatividade$ano):max(dados$rotatividade$ano)),
                       showgrid = FALSE,
                       showline = TRUE),
          legend = list(orientation = "h",
                        title = list(text = "")),
          barmode = "group")

#### Por atividade ----

dados$rotatividade |>
   select(-c(filtro, n_trabalhadores:desligados)) |>
   mutate(rotatividade = rotatividade * 100) |>
   pivot_wider(names_from = tipo,
               values_from = rotatividade) |>
   plot_ly(x = ~anodeclarado) |>
   add_trace(y = ~`Técnicos de nível médio`,
             type = "scatter", 
             mode = "lines+markers",
             name = "Técnicos de nível médio",
             color = I("#fb8e80")) |>
   add_trace(y = ~`Técnicos de nível superior`,
             type = "scatter", 
             mode = "lines+markers",
             name = "Técnicos de nível superior",
             color = I("#2b597a")) |>
   config(displayModeBar = FALSE) |>
   layout(hovermode = "x",
          yaxis = list(title = "",
                       ticksuffix = "%",
                       hoverformat = ".2f",
                       showgrid = FALSE,
                       showline = TRUE),
          xaxis = list(title = "",
                       tickformat = "%Y",
                       tickmode = "array",
                       tickvals = c(min(dados$rotatividade$ano):max(dados$rotatividade$ano)),
                       showgrid = FALSE,
                       showline = TRUE),
          legend = list(orientation = "h",
                        title = list(text = "")),
          barmode = "group")

## Painel 3

### Painel 3.1 - Ocupações para empresas

cbo <- sample(unique(dados_painel3$ocup_cbo$nome_cbo_ocupacao), 1)

dados_painel3$ocup_cbo |>
   group_by(nome_cbo_ocupacao, nome_cnae_classe) |>
   summarise(vinculos = sum(vinculos)) |>
   ungroup() |>
   arrange(desc(vinculos), nome_cbo_ocupacao) |>
   filter(nome_cbo_ocupacao == "Bombeiro civil") |>
   slice(1:15) |>
   plot_ly(x = ~vinculos) |>
   add_trace(y = ~nome_cnae_classe,
             type = "bar",
             color = I("#176180")) |>
   add_annotations(x = ~vinculos, 
                   y = ~nome_cnae_classe,
                   xanchor = "left",
                   yanchor = "rigth",
                   text = ~round(vinculos, 1),
                   textposition = "middle",
                   font = list(color = "white"),
                   showarrow = FALSE) |>
   config(displayModeBar = FALSE) |>
   layout(title = paste("Gráfico para CBO:", "Bombeiro civil"),
          hovermode = "y",
          yaxis = list(title = "",
                       showgrid = FALSE,
                       showline = TRUE,
                       categoryorder = "total ascending"),
          xaxis = list(title = "",
                       hoverformat = ".1f",
                       showgrid = FALSE,
                       showline = TRUE),
          legend = list(orientation = "h",
                        title = list(text = "")),
          barmode = "relative",
          uniformtext = list(minsize = 8,
                             mode = "hide"))

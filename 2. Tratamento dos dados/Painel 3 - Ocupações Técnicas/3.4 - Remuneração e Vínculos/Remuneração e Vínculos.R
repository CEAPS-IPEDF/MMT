#'*

# Carregar pacotes ----

library(tidyverse) # Manipulação dos dados
library(plotly)

`%notin%` <- Negate(`%in%`)           # Função de filtro
options(readr.show_col_types = FALSE) # Omitir formato das colunas no console

# Importação dos dados ----

estrutura_cbo <- readRDS("../1. Extração dos dados/Dicionários/Dicionário CBO.RDS")

## Filtro de técnicos ----

rais_4 <- rais |> 
  filter(cbo_tec == 1,
         !is.na(cboocupacao2002)) |>
  left_join(estrutura_cbo)

# Tratamento dos dados ----

## Dados do ano mais recente ----

### Vínculos ----

dados_recente_vinculos <- rais_4 |>
  filter(tipovinculo == 1,                 # Celetista
         referencia == max(referencia)) |> # Filtar pelo ano mais recente
  group_by(cbo_familia) %>%
  summarise(referencia = max(referencia),
            vinculos_recente = n())

### Salários ----

dados_recente_salarios <- rais_4 |>
  filter(tipovinculo == 1,              # Celetista
         referencia == max(referencia), # Filtar pelo ano mais recente
         salario_hora > 0) |>
  group_by(cbo_familia) %>%
  summarise(referencia = max(referencia),
            mediana_rendimento_recente = median(salario_dez_defl, na.rm = TRUE), # Salário médio
            mediana_salario_hora_recente = median(salario_hora, na.rm = TRUE))   # Hora de trabalho média

dados_recente <- dados_recente_salarios |>
  left_join(dados_recente_vinculos)

remove(dados_recente_salarios, dados_recente_vinculos)

## Dados do ano base ----

### Vínculos ----

dados_base_vinculos <- rais_4 |>
  filter(tipovinculo == 1,                     # Celetista
         referencia == max(referencia) - 1) |> # Período base - t-1 (2020)
  group_by(cbo_familia) |>
  summarise(vinculos_base = n())

### Salários ----

dados_base_salarios <- rais_4 |>
  filter(tipovinculo == 1,                  # Celetista
         referencia == max(referencia) - 1, # Período base - t-1 (2020)
         salario_hora > 0) |>
  group_by(cbo_familia) |>
  summarise(mediana_rendimento_base = median(salario_dez_defl, na.rm = TRUE), # Salário médio
            mediana_salario_hora_base = median(salario_hora, na.rm = TRUE))   # Hora de trabalho média

dados_base <- dados_base_salarios |>
  left_join(dados_base_vinculos)

remove(dados_base_salarios, dados_base_vinculos)

## Merge das bases ----

dados <- dados_base |>
  left_join(dados_recente, by = "cbo_familia") |>
  left_join(estrutura_cbo |>
              select(cbo_familia, nome_cbo_familia),
            by = "cbo_familia") |>
  mutate(variacao_rendimento = (mediana_rendimento_recente / mediana_rendimento_base) - 1,
         variacao_vinculos = (vinculos_recente / vinculos_base) - 1) |>
  distinct_all()

# Gráfico Exemplo ----

dados |> 
  select(cbo_familia,nome_cbo_familia,
         variacao_rendimento,
         variacao_vinculos) |>
  mutate(cat = case_when(variacao_vinculos > 0 & variacao_rendimento > 0 ~"A",
                         variacao_vinculos < 0 & variacao_rendimento < 0 ~"B",
                         TRUE~"C")) |> 
  filter(abs(variacao_vinculos) < 1,
         abs(variacao_rendimento) < 1) |> 
  ggplot(aes(x = variacao_rendimento,y = variacao_vinculos,
             col = cat, label = nome_cbo_familia))+
  geom_vline(xintercept = 0, color = "lightgrey") + 
  geom_hline(yintercept = 0,color = "lightgrey") +
  geom_point(size = 5,alpha = .4) + theme_minimal(base_size = 15) +
  scale_color_manual(values = c("#0a78c7","#d92335","#fb8e80")) +
  labs(x="Varição Rendimentos",y="Varição Vínculos", col = "") +
  scale_y_continuous(label = scales::percent,limits = c(-.5,.5)) +
  scale_x_continuous(label = scales::percent,limits = c(-.5,.5)) +
  geom_text()

plot_ly(dados |> 
          select(cbo_familia,nome_cbo_familia,
                 variacao_rendimento,
                 variacao_vinculos) |>
          mutate(cat = case_when(variacao_vinculos > 0 & variacao_rendimento > 0 ~"A",
                                 variacao_vinculos < 0 & variacao_rendimento < 0 ~"B",
                                 TRUE~"C"),
                 variacao_vinculos = round(variacao_vinculos,3),
                 variacao_rendimento = round(variacao_rendimento,3)) |> 
          filter(abs(variacao_vinculos) < 1,
                 abs(variacao_rendimento) < 1),
        x = ~variacao_rendimento * 100,
        y = ~variacao_vinculos * 100,
        color = ~cat,
        colors = c("#0a78c7","#d92335","#fb8e80"),
        text = ~nome_cbo_familia, 
        type = 'scatter', 
        mode = 'markers',
        marker = list(size = 25,opacity = 0.4)) |> 
  layout(yaxis = list(title = "Variação Vínculos",ticksuffix = "%",range=c(-50,50)),
         xaxis = list(title = "Variação Rendimentos",ticksuffix = "%",range=c(-50,50)))

# Exportar CSV ----

write_excel_csv2(dados, "Painel 3 - Ocupações Técnicas/Resultados/3.4 - Remuneração e vínculos.csv")

remove(rais_4)
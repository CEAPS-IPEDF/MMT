#'*Script que calcula a remuneração média por hora trabalhada*
# Carregar pacotes ----

library(tidyverse) # Manipulação dos dados
library(imputeTS) # SUbstituir NA's

`%notin%` <- Negate(`%in%`)           # Função de filtro
options(readr.show_col_types = FALSE) # Omitir formato das colunas no console

# Importação dos dados ----

## RAIS ----

rais <- readRDS("../1. Extração dos dados/Dados/RAIS.RDS") |>
  filter(tipovinculo == 1,       # Celetista
         #referencia != 2011,
         !is.na(cboocupacao2002)) |>
  select(ano = referencia,
         tec = cbo_tec,
         escolaridade)

## Base por tipo de emprego ----

### Empregos técnicos ----

#vinculos_total <- rais |>
#  filter(ano == 2021) |>
#  summarise(vinculos = n()) |>
#  pull(vinculos)
#
#vinculos <- rais |>
#  filter(ano == 2021) |>
#  group_by(cboocupacao2002) |>
#  summarise(vinculos = n(),
#            freq = vinculos / vinculos_total) |>
#  arrange(desc(freq)) |>
#  mutate(freq_acum = cumsum(freq)) |>
#  filter(freq_acum <= .99) |>
#  nrow()
#
#vinculos / length(unique(rais[rais$ano == 2021, which(names(rais) == "cbo")])) * 100

base_tec <- rais |>
  filter(tec == 1) |>
  group_by(ano) |> 
  summarise(vinculos = n(),
            tipo = "Trabalhadores técnicos")

### Empregos não técnicos ----

base_nao_tec <-  rais |>
  filter(tec == 0) |> 
  group_by(ano) |> 
  summarise(vinculos = n(),
            tipo = "Trabalhadores não técnicos")

### Merge das bases ----

base_tipo_emprego <- rbind(base_tec, base_nao_tec) |>
  group_by(ano) |>
  summarise(vinculos = sum(vinculos),
            tipo = "Total") |>
  rbind(base_tec, base_nao_tec)

# Gráficos ----

#base_tipo_emprego |>
#  filter(tipo != "Total") |>
#  group_by(tipo) |>
#  mutate(variacao = (vinculos / lag(vinculos, n = 1)) - 1) |>
#  filter(ano > 2011) |>
#  ggplot(aes(x = ano, y = variacao, fill = tipo, label = paste0(format(variacao * 100, digits = 2, big.mark = ".", decimal.mark = ","), "%"))) +
#  geom_col(position = position_dodge()) +
#  geom_hline(yintercept = 0) +
#  geom_text(position = position_dodge(width = .9),
#            vjust = -.5) +
#  labs(title = "Variação anual do número de vínculos",
#       x = "",
#       y = "",
#       fill = "") +
#  scale_y_continuous(labels = scales::percent,
#                     n.breaks = 15) +
#  scale_x_continuous(n.breaks = 9) +
#  theme_minimal(base_size = 15) +
#  theme(panel.grid = element_blank(),
#        legend.position = "bottom")

#base_tipo_emprego |>
#  ggplot(aes(x = ano, y = vinculos, color = tipo, label = paste0(format(vinculos, digits = 2, big.mark = ".", decimal.mark = ",")))) +
#  geom_line() +
#  geom_point() +
#  geom_text(vjust = -.5) +
#  labs(x = "",
#       y = "",
#       color = "") +
#  scale_y_continuous(label = scales::comma,
#                     n.breaks = 10) +
#  scale_x_continuous(n.breaks = 10) +
#  theme_minimal() +
#  theme(panel.grid = element_blank(),
#        legend.position = "bottom")

# Exportar CSV ----

write_excel_csv2(base_tipo_emprego, "Painel 2 - Remuneração e Ocupações/Resultados/2.3 - Número de vínculos.csv")

library(tidyverse)

`%notin%` <- Negate(`%in%`)

# Base antiga
dados <- readRDS("Dados/RAIS.RDS")
teste <- readRDS("Dados/rais-panorama-2021.RDS")

# Comparações de Vínculos por Ano
cbind(
  dados |> group_by(referencia) |> summarise(n_atual = n()),
  teste |> group_by(referencia) |> summarise(n_antigo = n())|> select(n_antigo)) |>
  mutate(dif = n_atual - n_antigo)

#214940 - Higienista ocupacional
#225355 - Médico radiologista intervencionista

# CBOs que estão na base nova e não estão na base antiga
cbos <- data.frame(cbos=unique(dados$cboocupacao2002)) |> 
  filter(cbos %notin% unique(teste$cboocupacao2002))

dados |> filter(cboocupacao2002 %in% cbos$cbos) |> group_by(referencia) |> summarise(cbos_dif = n()) |> select(cbos_dif)


cbind(
  dados |> group_by(referencia) |> summarise(n_atual = n()),
  teste |> group_by(referencia) |> summarise(n_antigo = n())|> select(n_antigo)) |>
  mutate(dif = n_atual - n_antigo) |> 
  cbind(dados |> filter(cboocupacao2002 %in% cbos$cbos) |> group_by(referencia) |> summarise(cbos_dif = n()) |> select(cbos_dif)) |> 
  mutate(dif_dif = dif - cbos_dif)

cbo_diff <- dados |>
  count(cboocupacao2002) |>
  rename(atual = n) |>
  left_join(teste |>
              count(cboocupacao2002) |>
              rename(antigo = n)) |>
  mutate(diff = atual - antigo) |>
  filter(diff != 0)

cbind(
  dados |> group_by(referencia) |> summarise(n_atual = n()),
  teste |> group_by(referencia) |> summarise(n_antigo = n())|> select(n_antigo)) |>
  mutate(dif = n_atual - n_antigo)

cbind(
  dados |> group_by(cboocupacao2002) |> summarise(n_atual = n()),
  teste |> group_by(cboocupacao2002) |> summarise(n_antigo = n())|> select(n_antigo)) |>
  mutate(dif = n_atual - n_antigo)

data.frame(cbos=unique(dados$cboocupacao2002)) |> 
  filter(cbos %in% unique(teste$cboocupacao2002))
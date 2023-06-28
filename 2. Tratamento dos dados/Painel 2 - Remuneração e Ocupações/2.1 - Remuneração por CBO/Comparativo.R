summary(dados)
summary(base_variacao_final)

cbo <- unique(dados$cboocupacao2002)

base_variacao_final |>
  mutate(cboocupacao2002 = as.numeric(cboocupacao2002)) |>
  filter(cboocupacao2002 %notin% cbo) |>
  nrow()

cbos <- data.frame(cbos = unique(dados$cboocupacao2002)) |> 
  filter(cbos %notin% unique(base_variacao_final$cboocupacao2002))

base_variacao_final |>
  distinct(cboocupacao2002) |>
  mutate(referencia = "sim") |>
  left_join(dados |>
              select(cboocupacao2002, nome_cbo_ocupacao)) |>
  mutate(dados = case_when(is.na(nome_cbo_ocupacao) ~ "não", TRUE ~ "sim")) |>
  count(dados)
  

summary(dados) == summary(dados_1)

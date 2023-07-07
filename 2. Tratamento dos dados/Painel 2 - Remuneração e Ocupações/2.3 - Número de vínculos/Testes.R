base_tipo_emprego |>
  filter(!is.na(nome_cnae_divisao),
         tipo == "Trabalhadores técnicos",
         ano == 2019) |>
  ungroup() |>
  summarise(vinculos = sum(vinculos))

base_tipo_emprego |>
  filter(!is.na(nome_cnae_divisao),
         tipo == "Trabalhadores técnicos",
         ano == 2019) |>
  group_by(nome_cnae_divisao) |>
  summarise(vinculos = sum(vinculos),
            freq = (vinculos / 142716) * 100) |>
  arrange(desc(freq)) |>
  mutate(freq_acum = cumsum(freq)) |>
  View()

base_tipo_emprego |>
  filter(!is.na(nome_cnae_divisao),
         tipo == "Trabalhadores técnicos") |>
  group_by(nome_cnae_divisao, ano) |>
  summarise(vinculos = sum(vinculos)) |>
  View()

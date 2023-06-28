cbos_variacao <- dados |>
  filter(variacao_vinculos > 2) |>
  pull(cboocupacao2002)

cbos_problema <- data.frame(cbo = NULL,
                            ano = NULL,
                            vinculos = NULL)

for (i in cbos_variacao) {
  
  cbos_problema <- cbos_problema |>
    rbind(rais |>
            filter(tipovinculo == 1,
                   cboocupacao2002 == i) |>
            group_by(referencia) |>
            summarise(n = n(),
                      cbo = estrutura_cbo[estrutura_cbo$cboocupacao2002 == i,2]))
  
}

(cbos_problema |>
    filter(cbo %notin% "Médico clínico") |>
    ggplot(aes(x = referencia, y = n, color = cbo)) +
    geom_line() +
    #gghighlight(cbo %in% cbo) +
    theme(legend.position = "none")) |>
  plotly::ggplotly()

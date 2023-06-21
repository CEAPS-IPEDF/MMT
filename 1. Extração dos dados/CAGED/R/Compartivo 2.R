all.equal(rotatividade_caged_novo, rotatividade_caged_novo_alexandre)

rotatividade_caged_novo <- rotatividade_caged_novo |>
  arrange(anodeclarado, cbo2002ocupacao, escolaridade)

script_novo <- script_novo |>
  arrange(anodeclarado, cbo2002ocupacao, escolaridade)

all.equal(caged$anodeclarado, script_novo$anodeclarado)
all.equal(rotatividade_caged_novo$cbo2002ocupacao, script_novo$cbo2002ocupacao)
all.equal(rotatividade_caged_novo$escolaridade, script_novo$escolaridade)
all.equal(rotatividade_caged_novo$admitidos, script_novo$admitidos)
all.equal(rotatividade_caged_novo$desligados, script_novo$desligados)

table(rotatividade_caged_novo$anodeclarado, script_novo$anodeclarado)
table(rotatividade_caged_novo$cbo2002ocupacao, script_novo$cbo2002ocupacao)
table(rotatividade_caged_novo$admitidos, script_novo$admitidos)
table(rotatividade_caged_novo$desligados, script_novo$desligados)

summary(rotatividade_caged_novo)
summary(script_novo)


# Cria base de dados de migrantes, saldo do caged e dados dos cursos (curso e caracterizado pela cbo)

# Configuração
## Carrega pacotes
library(tidyverse)

## Carrega dados dos cursos 
migrantes = readr::read_csv2('dados/migrantes_2019.csv')
migrantes = migrantes %>% 
  spread(key = migrante, value = n, fill = 0) %>% 
  mutate(prop = `TRUE`/ (`TRUE` +  `FALSE`),
         cboocupacao2002 = as.numeric(cboocupacao2002)) %>% 
  select(cboocupacao2002, migrantes = prop)

### CAGED
caged_dp = data.table::fread('dados/caged_200701_atual_DP_202207040953.csv')
caged_fp = data.table::fread('dados/caged_200701_atual_FP_202207011731.csv')
data.table::setnames(caged_fp, 'anomovimentacao', 'anodeclarado')

saldo_caged = rbind(caged_dp, caged_fp)[admitidosdesligados == 1,.(saldo =.N), by = .(ano = anodeclarado, cbo2002ocupacao)][, .(mediana_saldo = median(saldo)), by = cbo2002ocupacao]
saldo_caged[, cbo2002ocupacao := as.numeric(str_remove(cbo2002ocupacao, '-'))]

## Carrega dados de vinculos por curso
vinculos_salarios_medio = readxl::read_excel('dados/curso-vinculos-salarios.xlsx', sheet = 'total medio') %>% 
  select(cboocupacao2002 , ocupação = ocupacao, Curso = curso, `Mediana 2011-2019` = mediana_vinculos, `Vínculos 2019` = vinculos_referencia, `Proporção` = proporcao, `Mediana salário hora 2011-2019` = mediana_salario_hora , `Mediana salário hora` = mediana_salario_hora_referencia ) %>% 
  left_join(migrantes) %>% 
  left_join(saldo_caged, by = c('cboocupacao2002'  = 'cbo2002ocupacao'))


vinculos_salarios_superior = readxl::read_excel('dados/curso-vinculos-salarios.xlsx', sheet = 'total superior') %>% 
  select(cboocupacao2002 , ocupação = ocupacao, Curso = curso, `Mediana 2011-2019` = mediana_vinculos, `Vínculos 2019` = vinculos_referencia, `Proporção` = proporcao, `Mediana salário hora 2011-2019` = mediana_salario_hora , `Mediana salário hora` = mediana_salario_hora_referencia ) %>% 
  left_join(migrantes) %>% 
  left_join(saldo_caged, by = c('cboocupacao2002'  = 'cbo2002ocupacao'))

# Salva Resultados
write_excel_csv2(vinculos_salarios_medio, 'dados/vinculos_salarios_medio.csv')
write_excel_csv2(vinculos_salarios_superior, 'dados/vinculos_salarios_superior.csv')

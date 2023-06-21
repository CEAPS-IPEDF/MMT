# Carrega pacotes ----
library(data.table)
library(tidyverse)
library(readxl)

# Diretorio ----
diretorio <- c("P:/RECENTES/DIEPS/GEFAPS/GEFAPS/2022/Panorama da Capacitação/rais-panorama/dados")
rais <- c("/primario/rais-panorama.csv")

# Ler os dados ----
## RAIS ----
dados <- data.table::fread(paste0(diretorio, rais)) #RAIS bruta

dados<- rais
# Selecionar variáveis ----
base <- dados %>%
  select(ano = referencia, horas = horas_mensais,
         salario_hora, #salário hora
         tec = cbo_tec, tec_em = cbo_tec_em_complet, tec_sup = cbo_tec_sup, #CBOS tec
         em_inc = medio_incompleto, em_comp = medio_completo, sup_comp = superior_completo, analf = analfabeto) #escolaridade

# Base ----
## Base Tec ----
base_tec <- base %>%
  filter(tec == 1,
         salario_hora > 0,
         horas > 0) %>% 
  mutate(tipo = case_when(tec_em == 1 ~"tec_em",
                          tec_sup == 1~"tec_sup")) %>% 
  group_by(ano, tipo) %>% #Agrupando por ano
  summarise(filtro = "tec",
            salario = mean(salario_hora, na.rm = T)) %>%
  rbind(base %>% 
          filter(tec == 1,
                 salario_hora > 0,
                 horas > 0) %>% 
          group_by(ano) %>% 
          summarise(tipo = "tec_geral",
                    filtro = "tec",
                    salario = mean(salario_hora, na.rm = T)))
## Base Não Tec ----
base_nao_tec <- base %>%
  filter(tec == 0,
         salario_hora > 0,
         horas > 0) %>%
  mutate(tipo = case_when(em_inc == 1 ~"em_incompleto",
                          em_comp == 1 ~"em_completo",
                          sup_comp == 1 ~"sup_completo",
                          analf == 1~"analfabeto")) %>% 
  group_by(ano, tipo) %>% 
  summarise(filtro = "nao_tec",
            salario = mean(salario_hora, na.rm = T)) %>% 
  rbind(base %>% 
          filter(tec == 0,
                 salario_hora > 0,
                 horas > 0) %>% 
          group_by(ano) %>% 
          summarise(tipo = "nao_tec_geral",
                    filtro = "nao_tec",
                    salario = mean(salario_hora, na.rm = T)))
## Bind ----
base_final <- rbind(base_tec,base_nao_tec) %>%
  mutate(geral = case_when(tipo %in% c("tec_geral", "nao_tec_geral")~"geral",
                           TRUE~"subgrupo"))
# Salvar Base ----
write_excel_csv2(base_final, paste0(diretorio,"/tabela_salario_hora_mmt.csv"))
# Gráficos ----
library(jcolors)
library("ggplot2")

## Linhas ----
base_final %>% ggplot(aes(x = ano, y = salario, color = tipo)) +
  geom_line(size = 1.5)+
  geom_point(size = 3)+
  theme_classic() + 
  scale_color_jcolors(palette = "pal10") +
  #facet_wrap(.~geral, ncol = 1) +
  scale_y_continuous(label = scales::label_dollar(prefix = "R$", big.mark = ".", decimal.mark = ","),
                     limits = c(0, 120),
                     breaks = seq(0, 120, by = 20),
                     expand = c(0,0.01))

base_final %>% ggplot(aes(x = ano, y = salario, color = tipo)) +
  geom_line(size = 1.5)+
  geom_point(size = 3)+
  theme_classic() + 
  scale_color_jcolors(palette = "pal10") +
  facet_wrap(.~geral, ncol = 1) +
  scale_y_continuous(label = scales::label_dollar(prefix = "R$", big.mark = ".", decimal.mark = ","),
                     limits = c(0, 120),
                     breaks = seq(0, 120, by = 20),
                     expand = c(0,0.01))

base_final %>% ggplot(aes(x = round(ano,0), y = salario, color = tipo)) +
  geom_line(size = 1.5)+
  geom_point(size = 3)+
  theme_classic() + 
  scale_color_jcolors(palette = "pal10") +
  facet_wrap(.~filtro, ncol = 1) +
  scale_y_continuous(label = scales::label_dollar(prefix = "R$", big.mark = ".", decimal.mark = ","),
                     limits = c(0, 120),
                     breaks = seq(0, 120, by = 20),
                     expand = c(0,0.01))


## Barras ----
base_final %>% ggplot(aes(x = ano, y = salario, fill = tipo)) +
  geom_col(stat = "identity", position = position_dodge2())+
  theme_minimal() +  
  scale_fill_brewer(palette = "Blues") +
  #facet_wrap(.~geral, ncol = 1) +
  scale_y_continuous(label = scales::label_dollar(prefix = "R$", big.mark = ".", decimal.mark = ","),
                     limits = c(0, 120),
                     breaks = seq(0, 120, by = 20),
                     expand = c(0,0.01))

base_final %>% ggplot(aes(x = ano, y = salario, fill = tipo)) +
  geom_col(stat = "identity", position = position_dodge2())+
  theme_bw() +  
  scale_fill_brewer(palette = "Blues") +
  facet_wrap(.~geral, ncol = 1) +
  scale_y_continuous(label = scales::label_dollar(prefix = "R$", big.mark = ".", decimal.mark = ","),
                     limits = c(0, 120),
                     breaks = seq(0, 120, by = 20),
                     expand = c(0,0.01))

`%notin%` <- Negate(`%in%`)

base_final %>% 
  filter(tipo %in% c("tec_em", "tec_sup","tec_geral")) %>% 
  ggplot(aes(x = as.character(ano), y = salario, fill = tipo)) +
  geom_col(stat = "identity", position = position_dodge2())+
  theme_minimal(base_size = 20) +  theme(legend.position = "bottom",
                                         axis.title.x = element_blank(),
                                         axis.title.y = element_blank()) +
  labs(title = "", fill= "") +
  scale_fill_manual(values = c("#f2cb64","#46a462","#2b7398","#a6a6a6","#0eb2ac","#0eb2ac","#2b7398","#a6a6a6")) +
  facet_wrap(.~filtro, ncol = 1) +
  scale_y_continuous(label = scales::label_dollar(prefix = "R$", big.mark = ".", decimal.mark = ","),
                     limits = c(0, 120),
                     breaks = seq(0, 120, by = 20),
                     expand = c(0,0.01))


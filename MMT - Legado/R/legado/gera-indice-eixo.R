# Calcula indice vinculos ----
# Calcula o numero de vinculos por eixo tecnologico diferindo por nivel educacional, considera 1 como o numero de vinculos em 2012

# Configuração ----
## Carrega pacote ----
library(data.table)
library(ggplot2)
library(tidyverse)
library(lemon)
### gerar função oposta ao %in% 
`%notin%` <- Negate(`%in%`)

## Carrega dados ----
dados = fread('dados/primario/rais-panorama.csv')

## Define função ----
calcula_indice = function(nome){
  temp = copy(dados)
    
  temp = temp[get(nome) == 1,]
  
  conta = temp[referencia %in% c(2011, 2012, 2016, 2020), .(indice = .N), by = referencia]

  conta[, indice := indice/first(indice)]
  conta[, nome := nome]
}

## Define Espaço do loop ----
nomes_superior = names(dados)[46:57]
nomes_medio = names(dados)[33:45]

# Loop ----
dados_indice = rbind(
  rbindlist(lapply(nomes_medio, calcula_indice)),
  rbindlist(lapply(nomes_superior, calcula_indice))
)

# Diferencia Nível Médio de Nível Superior
dados_indice$nivel <- case_when(str_detect(dados_indice$nome, "_em")~"Nível Médio",
                                TRUE~"Nível Superior")

paleta <- c("#407abc", "#99999c", "#e8bb2e", "#00a85a")

dados_indice[dados_indice$nome == "eixo_amb_saude_em", "nome"] <-"Ambiente e Saúde"
dados_indice[dados_indice$nome == "eixo_conteprocessos_em", "nome"] <-"Controle e Processos Industriais"
dados_indice[dados_indice$nome == "eixo_desedusoc_em", "nome"] <-"Desenvolvimento Educacional e Social"
dados_indice[dados_indice$nome == "eixo_negocios_em", "nome"] <-"Gestão e Negócios"
dados_indice[dados_indice$nome == "eixo_infoecomunic_em", "nome"] <-"Informação e Comunicação"
dados_indice[dados_indice$nome == "eixo_infraestrutura_em", "nome"] <-"Infraestrutura"
dados_indice[dados_indice$nome == "eixo_prodaliment_em", "nome"] <-"Produção Alimentícia"
dados_indice[dados_indice$nome == "eixo_prodcult_em", "nome"] <-"Produção Cultural e Design"
dados_indice[dados_indice$nome == "eixo_prodindust_em", "nome"] <-"Produção Industrial"
dados_indice[dados_indice$nome == "eixo_recnaturais_em", "nome"] <-"Recursos Naturais"
dados_indice[dados_indice$nome == "eixo_seguranca_em", "nome"] <-"Segurança"
dados_indice[dados_indice$nome == "eixo_hospelazer_em", "nome"] <-"Turismo Hospedagem e Lazer"
dados_indice[dados_indice$nome == "eixo_militar_em", "nome"] <-"Militar"
dados_indice[dados_indice$nome == "eixo_ambesaude_sup", "nome"] <-"Ambiente e Saúde"
dados_indice[dados_indice$nome == "eixo_conteprocessos_sup", "nome"] <-"Controle e Processos Industriais"
dados_indice[dados_indice$nome == "eixo_negocios_sup", "nome"] <-"Gestão e Negócios"
dados_indice[dados_indice$nome == "eixo_infoecomunic_sup", "nome"] <-"Informação e Comunicação"
dados_indice[dados_indice$nome == "eixo_infraestrutura_sup", "nome"] <-"Infraestrutura"
dados_indice[dados_indice$nome == "eixo_prodcult_sup", "nome"] <-"Produção Cultural e Design"
dados_indice[dados_indice$nome == "eixo_prodindust_sup", "nome"] <-"Produção Industrial"
dados_indice[dados_indice$nome == "eixo_seguranca_sup", "nome"] <-"Segurança"
dados_indice[dados_indice$nome == "eixo_hospelazer_sup", "nome"] <-"Turismo Hospedagem e Lazer"
dados_indice[dados_indice$nome == "eixo_prodaliment_sup", "nome"] <-"Produção Alimentícia"
dados_indice[dados_indice$nome == "eixo_militar_sup", "nome"] <-"Militar"

#Gráfico Nível Médio
dados_indice %>% filter(nivel == "Nível Médio",
                        nome != "Militar") %>%
  ggplot(aes(x = as.character(referencia), y = indice, fill = as.character(referencia))) +
  geom_col() + geom_text(aes(x = as.character(referencia),
                             y = indice,
                             label = round(indice,2),
                             colour = as.character(referencia),
                             fontface = "bold"),
                         vjust = -.5,
                         show.legend = F) +
  theme_classic(base_size = 15) + 
  theme(strip.background = element_blank(),
        legend.position = "bottom",
        axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  scale_fill_manual(values = paleta) +
  scale_color_manual(values = paleta) +
  labs(y = "", x = "", fill = "") +
  scale_y_continuous(limits = c(0,3),expand = c(0,0.01))+
  facet_wrap(.~nome, ncol = 6,
             labeller = label_wrap_gen(width = 25)) 

#Gráfico Nível Médio
dados_indice %>% filter(nivel == "Nível Médio",
                        nome != "Militar") %>%
  ggplot(aes(x = as.character(referencia), y = indice, fill = as.character(referencia))) +
  geom_col() + geom_text(aes(x = as.character(referencia),
                             y = indice,
                             label = round(indice,2),
                             colour = as.character(referencia),
                             fontface = "bold"),
                         vjust = -.5,
                         show.legend = F) +
  theme_classic(base_size = 15) + 
  theme(strip.background = element_blank(),
        legend.position = "bottom",
        axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  scale_fill_manual(values = paleta) +
  scale_color_manual(values = paleta) +
  labs(y = "", x = "", fill = "") +
  scale_y_continuous(limits = c(0,3),expand = c(0,0.01))+
  facet_wrap(.~nome, ncol = 4,
             labeller = label_wrap_gen(width = 25)) 

#Gráfico Nível Superior

dados_indice %>% filter(nivel == "Nível Superior") %>%
  ggplot(aes(x = as.character(referencia), y = indice, fill = as.character(referencia))) +
  geom_col() + geom_text(aes(x = as.character(referencia),
                             y = indice,
                             label = round(indice,2),
                             colour = as.character(referencia),
                             fontface = "bold"),
                         vjust = -.5,
                         show.legend = F) +
  theme_classic(base_size = 15) + 
  theme(strip.background = element_blank(),
        legend.position = "bottom",
        axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  scale_fill_manual(values = paleta) +
  scale_color_manual(values = paleta) +
  labs(y = "", x = "", fill = "") +
  scale_y_continuous(limits = c(0,15),expand = c(0,0.01))+
  facet_wrap(.~nome, ncol = 6,
             labeller = label_wrap_gen(width = 25)) 

dados_indice %>% filter(nivel == "Nível Superior",
                        nome %notin% c("Ambiente e Saúde",
                                       "Militar",
                                       "Produção Alimentícia")) %>%
  ggplot(aes(x = as.character(referencia), y = indice, fill = as.character(referencia))) +
  geom_col() + geom_text(aes(x = as.character(referencia),
                             y = indice,
                             label = round(indice,2),
                             colour = as.character(referencia),
                             fontface = "bold"),
                         vjust = -.5,
                         show.legend = F) +
  theme_classic(base_size = 15) + 
  theme(strip.background = element_blank(),
        legend.position = "bottom",
        axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  scale_fill_manual(values = paleta) +
  scale_color_manual(values = paleta) +
  labs(y = "", x = "", fill = "") +
  scale_y_continuous(limits = c(0,3),expand = c(0,0.01))+
  facet_wrap(.~nome, ncol = 4,
             labeller = label_wrap_gen(width = 25)) 

dados_indice %>% filter(nivel == "Nível Superior",
                        nome == "Ambiente e Saúde") %>%
  ggplot(aes(x = as.character(referencia), y = indice, fill = as.character(referencia))) +
  geom_col() + geom_text(aes(x = as.character(referencia),
                             y = indice,
                             label = round(indice,2),
                             colour = as.character(referencia),
                             fontface = "bold"),
                         vjust = -.5,
                         show.legend = F) +
  theme_classic(base_size = 15) + 
  theme(strip.background = element_blank(),
        legend.position = "bottom",
        axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  scale_fill_manual(values = paleta) +
  scale_color_manual(values = paleta) +
  labs(y = "", x = "", fill = "") +
  scale_y_continuous(limits = c(0,15),expand = c(0,0.01))+
  facet_wrap(.~nome, ncol = 4,
             labeller = label_wrap_gen(width = 25)) 

# Salva resultados ----
readr::write_excel_csv2(dados_indice, 'dados/dados-indice-novo.csv')

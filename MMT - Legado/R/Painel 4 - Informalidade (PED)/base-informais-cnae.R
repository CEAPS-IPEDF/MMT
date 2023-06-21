#### Scrip gera dados de Numero absoluto de informais por setor CNAE com a PED

# Carregando pacotes ----
# Limpa Diretório
rm(list=ls(all=T))
# Dados sem notação científica
options(scipen=100)
# Paacotes
easypackages::libraries("data.table","tidyverse","readxl","survey","TTR")

rstudioapi::writeRStudioPreference("data_viewer_max_columns", 300L)

# Conexão com DB ----
db <- RODBC::odbcConnect("db_codeplan",uid=('codeplan'),pwd=('codeplan'))

# Dados PED ----
sql = paste0('DB_CODEPLAN.ped.NovaPEDDF', 2018:2022)

ped = purrr::map_dfr(sql, 
                     function(x){
                       dados = RODBC::sqlQuery(db, paste0("SELECT * FROM ", x))
                       names(dados) = stringr::str_to_upper(names(dados))
                       return(dados)
                     })

# Funções ----

source('R/utis.R')

# Número de Informais por Setor CNAE por ano
informais_cnae_ano <- tabela_ped(base = ped,
                    x = "SETOR_CNAE",
                    y = "(POS==2|(POS==5 & F280==2)|(POS==6 & F280==2))",
                    filtro = "TRUE",
                    anual = T)[,-c(6,7)]

names(informais_cnae_ano) <- c("Ano",
                           "Indústria de Transformação",
                           "Construção",
                           "Comércio",
                           "Serviços")

informais_cnae_ano <- informais_cnae_ano %>% 
  gather(setor,informais, 2:5)

informais_cnae_ano %>%
  ggplot(aes(x = Ano, y = informais, color = setor, group = setor)) +
  geom_line(linewidth = 1) + geom_point(size = 2) + theme_classic() +
  scale_color_manual(values = c("#46a462","#2e818d","#2960a7","#f2cb64","#a6a6a6")) +
  scale_y_continuous(labels = scales::number,
                     limits = c(0, 150000),
                     breaks = seq(0, 150000, by = 25000))+
  scale_x_discrete(expand = c(.01,.01)) +
  theme(legend.position = "bottom") +
  labs(x = "", y = "nº de informais (em milhares)", color = "")

# Número de Informais por Setor CNAE por mês
informais_cnae_mes <- tabela_ped(base = ped,
                                 x = "SETOR_CNAE",
                                 y = "(POS==2|(POS==5 & F280==2)|(POS==6 & F280==2))",
                                 filtro = "TRUE",
                                 anual = F)[,-c(6,7)]

names(informais_cnae_mes) <- c("Ano",
                               "Indústria de Transformação",
                               "Construção",
                               "Comércio",
                               "Serviços")

informais_cnae_mes <- informais_cnae_mes %>% 
  gather(setor,informais, 2:5)

informais_cnae_mes %>%
  ggplot(aes(x = (Ano), y = informais, color = setor, group = setor)) +
  geom_line(linewidth = 1) + geom_point(size = 2) + theme_classic() +
  scale_color_manual(values = c("#064471","#2e818d","#3e9974","#84b263","#bebf64")) +
  scale_y_continuous(labels = scales::number,
                     limits = c(0,150),
                     breaks = seq(0,150, by = 25))+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = "", y = "nº de informais (em milhares)", color = "")

base_cnae <- left_join(
  tabela_ped(base = ped,
             x = "SETOR_CNAE",
             y = "(POS==2|(POS==5 & F280==2)|(POS==6 & F280==2))",
             filtro = "TRUE",anual = T) %>%
    gather(setor,informais, 2:7),
  tabela_ped(base = ped,
             x = "SETOR_CNAE",
             y = "(SIT==4)",
             filtro = "TRUE",anual = T) %>%
    gather(setor,ocupados, 2:7), by =c("ano","setor")) %>%
  filter(setor != 10000) %>% http://127.0.0.1:24557/graphics/plot_zoom_png?width=1920&height=1040
  mutate(prop_informais = informais/ocupados,
         setor = case_when(setor == 2000 ~"Indústria de Transformação",
                           setor == 3000 ~"Construção",
                           setor == 4000 ~"Comércio",
                           setor == 5000 ~"Serviços",
                           setor == 9999 ~"Demais Setores"))

base_cnae %>%
  ggplot(aes(x = (ano), y = prop_informais, fill = setor, group = setor)) +
  geom_bar(stat= "identity",width = .8, position = position_dodge2())+
  theme_minimal(base_size = 15) +
  scale_fill_manual(values = c("#064471","#2e818d","#3e9974","#84b263","#bebf64")) +
  scale_y_continuous(labels = scales::percent)+
  theme(legend.position = "bottom") +
  labs(x = "", y = "proporção de informais entre ocupados", fill = "")






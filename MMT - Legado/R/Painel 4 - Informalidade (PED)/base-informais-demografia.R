#  Scrip gera dados de Numero absoluto de  ----
#informais por características da população ----
# 1 - Carregando pacotes ----
# Limpa Diretório
rm(list=ls(all=T))
# Dados sem notação científica
options(scipen=100)
# Paacotes
easypackages::libraries("data.table","tidyverse","readxl","survey","TTR")

rstudioapi::writeRStudioPreference("data_viewer_max_columns", 300L)
# 2 - Funções ----

source('R/utis.R')

# 3 - Conexão com DB ----
db <- RODBC::odbcConnect("db_codeplan",uid=('c
                                            
                                            odeplan'),pwd=('codeplan'))

## Dados PED ----
sql = paste0('DB_CODEPLAN.ped.NovaPEDDF', 2018:2022)

ped = purrr::map_dfr(sql, 
                     function(x){
                       dados = RODBC::sqlQuery(db, paste0("SELECT * FROM ", x))
                       names(dados) = stringr::str_to_upper(names(dados))
                       return(dados)
                     })
# Fecha conexão com DB
RODBC::odbcCloseAll()


# 4 - Criando Variáveis ----
##Faixa de Idade ----
ped$faixa_idade <- case_when(ped$C050 %in% c(14,15)~"14 e 15 anos",
                             ped$C050 %in% c(16:24)~"16 a 24 anos",
                             ped$C050 %in% c(25:39)~"25 a 39 anos",
                             ped$C050 %in% c(40:49)~"40 a 49 anos",
                             ped$C050 %in% c(50:59)~"50 a 59 anos",
                             ped$C050>60 & ped$C050 != 1001~"Mais de 60 anos",
                             ped$C050 == 1001~"Outros",
                             TRUE~"Outros")

##Cor ----
ped$negro <- case_when(ped$C040 %in% c(1,4,5) ~"Não negros", #Não-Negros
                       ped$C040 %in% c(2,3) ~"Negros", #Negros
                       TRUE ~"Outros")#Não sabe

# 5 - Bases ----
## 5.1 Informais por Sexo ----
### Base Mensal ----
informais_sexo_mes <-  tabela_ped(base = ped,
                                  x = "C030",
                                  y = "(POS==2|(POS==5 & F280==2)|(POS==6 & F280==2))",
                                  filtro = "TRUE",
                                  anual = F)[,-c(6,7)]

names(informais_sexo_mes) <- c("Ano","Masculino","Feminino")

informais_sexo_mes$`Prop Masculino` <- informais_sexo_mes[,2]/rowSums(informais_sexo_mes[,2:3])
informais_sexo_mes$`Prop Feminino` <- 1 - informais_sexo_mes$`Prop Masculino`

informais_sexo_mes <- informais_sexo_mes[-c(1,2),c(1,4,5)] %>% 
  gather(sexo,informais, 2:3)

### Base Anual ----
informais_sexo_ano <-  tabela_ped(base = ped,
                                  x = "C030",
                                  y = "(POS==2|(POS==5 & F280==2)|(POS==6 & F280==2))",
                                  filtro = "TRUE",
                                  anual = T)[,-c(6,7)]

names(informais_sexo_ano) <- c("Ano","Masculino","Feminino")

informais_sexo_ano$`Prop Masculino`  <- informais_sexo_ano[,2]/rowSums(informais_sexo_ano[,2:3])
informais_sexo_ano$`Prop Feminino` <- 1 - informais_sexo_ano$`Prop Masculino` 

informais_sexo_ano <- informais_sexo_ano[,c(1,4,5)] %>% 
  gather(sexo,informais, 2:3)

base_sexo <- left_join(
  tabela_ped(base = ped,
             x = "C030",
             y = "(POS==2|(POS==5 & F280==2)|(POS==6 & F280==2))",
             filtro = "TRUE",anual = T) %>%
    gather(sexo,informais, 2:3),
  tabela_ped(base = ped,
             x = "C030",
             y = "(SIT==4)",
             filtro = "TRUE",anual = T) %>%
    gather(sexo,ocupados, 2:3), by =c("ano","sexo")) %>%
  mutate(prop_informais = informais/ocupados,
         sexo = ifelse(sexo == 1, "Masculino", "Feminino"))


## 5.2 Informais por Cor ----
### Base Mensal ----
informais_cor_mes <-  tabela_ped(base = ped,
                                  x = "negro",
                                  y = "(POS==2|(POS==5 & F280==2)|(POS==6 & F280==2))",
                                  filtro = "TRUE",
                                  anual = F)[,-c(6,7)]


informais_cor_mes$`Prop Não negro` <- informais_cor_mes[,2]/rowSums(informais_cor_mes[,2:3])
informais_cor_mes$`Prop Negro` <- 1 - informais_cor_mes$`Prop Não negro`

informais_cor_mes <- informais_cor_mes[-c(1,2),c(1,5,6)] %>% 
  gather(cor,informais, 2:3)

### Base Anual ----
informais_cor_ano <-  tabela_ped(base = ped,
                                  x = "negro",
                                  y = "(POS==2|(POS==5 & F280==2)|(POS==6 & F280==2))",
                                  filtro = "TRUE",
                                  anual = T)[,-c(6,7)]



informais_cor_ano$`Prop Não negro`  <- informais_cor_ano[,2]/rowSums(informais_cor_ano[,2:3])
informais_cor_ano$`Prop Negro` <- 1 - informais_cor_ano$`Prop Não negro` 

informais_cor_ano <- informais_cor_ano[,c(1,5,6)] %>% 
  gather(cor,informais, 2:3)

base_cor <- left_join(
  tabela_ped(base = ped,
             x = "negro",
             y = "(POS==2|(POS==5 & F280==2)|(POS==6 & F280==2))",
             filtro = "TRUE",anual = T) %>%
    select(-Outros) %>% 
    gather(cor,informais, 2:3),
  tabela_ped(base = ped,
             x = "negro",
             y = "(SIT==4)",
             filtro = "TRUE",anual = T) %>%
    select(-Outros) %>% 
    gather(cor,ocupados, 2:3), by =c("ano","cor")) %>%
  mutate(prop_informais = informais/ocupados,
         cor = ifelse(cor == 1, "Não negro", "Negro"))

## 5.3 Informais por faixa etária ----
### Base geral ----
base_idade <- left_join(
  tabela_ped(base = ped,
             x = "faixa_idade",
             y = "(POS==2|(POS==5 & F280==2)|(POS==6 & F280==2))",
             filtro = "TRUE",anual = T) %>%
    gather(idade,informais, 2:8),
  tabela_ped(base = ped,
             x = "faixa_idade",
             y = "(SIT==4)",
             filtro = "TRUE",anual = T) %>%
    gather(idade,ocupados, 2:8), by =c("ano","idade")) %>%
  filter(idade!= "Outros") %>% 
  mutate(prop_informais = informais/ocupados)

## 6 - Gráficos ----
### 6.1 Sexo ----
#### Barras proporção sexo ----
informais_sexo_ano %>% 
  ggplot(aes(x = Ano,y = informais, fill = sexo))+
  geom_bar(stat= "identity", position = position_dodge2(),width = .8)+
  theme_minimal() +
  scale_fill_manual(values = c("#46a462","#2e818d"))+
  scale_y_continuous(labels = scales::percent,
                     expand = c(0,0)) +
  scale_x_discrete(expand = c(.01,.01)) +
  labs(x = "",y="Proporção entre informais",fill="")+
  ggtitle("Proporção por sexo entre informais")+
  theme(legend.position = "bottom")

#### Barras empilhadas proporção sexo ----
informais_sexo_ano %>% 
  ggplot(aes(x = Ano,y = informais, fill = sexo))+
  geom_bar(stat= "identity",width = .95)+
  geom_text(label = scales::percent(informais_sexo_ano$informais),
            size = 3, 
            position = position_stack(vjust = 0.5),
            color = "white")+
  theme_minimal() +
  scale_fill_manual(values = c("#46a462","#2e818d"))+
  scale_y_continuous(labels = scales::percent,
                     expand = c(0,0)) +
  scale_x_discrete(expand = c(.01,.01)) +
  labs(x = "",y="Proporção entre informais",fill="")+
  ggtitle("Proporção por sexo entre informais")+
  theme(legend.position = "bottom")



#### Barra empilhada Proporção sexo mês ----
informais_sexo_mes %>% 
  ggplot(aes(x = Ano,y = informais, fill = sexo))+
  geom_bar(stat= "identity",width = .8)+
  theme_minimal() +
  scale_fill_manual(values = c("#46a462","#2e818d"))+
  scale_y_continuous(labels = scales::percent,
                     expand = c(0,0)) +
  scale_x_discrete(expand = c(.01,.01)) +
  labs(x = "",y="Proporção entre informais",fill="")+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#### Barras Proporção sexo mês ----
informais_sexo_mes %>% 
  ggplot(aes(x = Ano,y = informais, fill = sexo))+
  geom_bar(stat= "identity",width = .85, position = position_dodge2())+
  theme_minimal() +
  scale_fill_manual(values = c("#46a462","#2e818d"))+
  scale_y_continuous(labels = scales::percent,
                     expand = c(0,0)) +
  scale_x_discrete(expand = c(.01,.01)) +
  labs(x = "",y="Proporção entre informais",fill="")+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#### Linhas Proporção sexo mês ----
informais_sexo_mes %>% 
  ggplot(aes(x = Ano,y = informais, color = sexo, group = sexo))+
  geom_point(size = 2)+ geom_line(linewidth = .5)+
  theme_minimal() +
  scale_color_manual(values = c("#46a462","#2e818d"))+
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(expand = c(.01,.01)) +
  labs(x = "",y="Proporção entre informais",fill="")+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#### Área empilhada Proporção sexo mês ----
informais_sexo_mes %>% 
  ggplot(aes(x = Ano,y = informais, fill = sexo, group = sexo))+
  geom_area()+
  theme_minimal() +
  scale_fill_manual(values = c("#46a462","#2e818d"))+
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(expand = c(.01,.01)) +
  labs(x = "",y="Proporção entre informais",fill="")+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

### 6.2 Cor ----
#### Barras proporção cor ----
informais_cor_ano %>% 
  ggplot(aes(x = ano,y = informais, fill = cor))+
  geom_bar(stat= "identity", position = position_dodge2(),width = .8)+
  theme_minimal() +
  scale_fill_manual(values = c("#84b263","#bebf64"))+
  scale_y_continuous(labels = scales::percent,
                     expand = c(0,0)) +
  scale_x_discrete(expand = c(.01,.01)) +
  labs(x = "",y="Proporção entre informais",fill="")+
  theme(legend.position = "bottom")

#### Barras empilhadas proporção cor ----
informais_cor_ano %>% 
  ggplot(aes(x = ano,y = informais, fill = cor))+
  geom_bar(stat= "identity",width = .95)+
  geom_text(label = scales::percent(informais_cor_ano$informais),
            size = 3, 
            position = position_stack(vjust = 0.5),
            color = "white")+
  theme_minimal() +
  scale_fill_manual(values = c("#84b263","#bebf64"))+
  scale_y_continuous(labels = scales::percent,
                     expand = c(0,0)) +
  scale_x_discrete(expand = c(.01,.01)) +
  labs(x = "",y="Proporção entre informais",fill="")+
  theme(legend.position = "bottom")



#### Barra empilhada Proporção cor mês ----
informais_cor_mes %>% 
  ggplot(aes(x = mes_ano,y = informais, fill = cor))+
  geom_bar(stat= "identity",width = .8)+
  theme_minimal() +
  scale_fill_manual(values = c("#84b263","#bebf64"))+
  scale_y_continuous(labels = scales::percent,
                     expand = c(0,0)) +
  scale_x_discrete(expand = c(.01,.01)) +
  labs(x = "",y="Proporção entre informais",fill="")+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#### Barras Proporção cor mês ----
informais_cor_mes %>% 
  ggplot(aes(x = mes_ano,y = informais, fill = cor))+
  geom_bar(stat= "identity",width = .85, position = position_dodge2())+
  theme_minimal() +
  scale_fill_manual(values = c("#84b263","#bebf64"))+
  scale_y_continuous(labels = scales::percent,
                     expand = c(0,0)) +
  scale_x_discrete(expand = c(.01,.01)) +
  labs(x = "",y="Proporção entre informais",fill="")+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#### Linhas Proporção cor mês ----
informais_cor_mes %>% 
  ggplot(aes(x = mes_ano,y = informais, color = cor, group = cor))+
  geom_point(size = 2)+ geom_line(linewidth = .5)+
  theme_minimal() +
  scale_color_manual(values = c("#84b263","#bebf64"))+
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(expand = c(.01,.01)) +
  labs(x = "",y="Proporção entre informais",fill="")+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#### Área empilhada Proporção cor mês ----
informais_cor_mes %>% 
  ggplot(aes(x = mes_ano,y = informais, fill = cor, group = cor))+
  geom_area()+
  theme_minimal() +
  scale_fill_manual(values = c("#84b263","#bebf64"))+
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(expand = c(.01,.01)) +
  labs(x = "",y="Proporção entre informais",fill="")+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

### 6.3 Idade ----
#### Barras numéro informais por faixa etária ----
base_idade %>% 
  ggplot(aes(x = ano,y = informais, fill = idade))+
  geom_bar(stat= "identity", position = position_dodge2(),width = .8)+
  theme_minimal() +
  scale_fill_manual(values = c("#064471","#2960a7","#2e818d","#3e9974","#84b263","#bebf64"))+
  scale_y_continuous(labels = scales::number,
                     expand = c(0,0)) +
  scale_x_discrete(expand = c(.01,.01)) +
  labs(x = "",y="nº informais",fill="")+
  theme(legend.position = "bottom")

#### Barras proporção informais/ocupados por faixa etária ---- 
base_idade %>% 
  ggplot(aes(x = ano,y = prop_informais, fill = idade))+
  geom_bar(stat= "identity", position = position_dodge2(),width = .8)+
  theme_minimal() +
  scale_fill_manual(values = c("#064471","#2960a7","#2e818d","#3e9974","#84b263","#bebf64"))+
  scale_y_continuous(labels = scales::percent,
                     expand = c(0,0)) +
  scale_x_discrete(expand = c(.01,.01)) +
  labs(x = "",y="Proporção de informais entre os Ocupados",fill="")+
  theme(legend.position = "bottom")
  
  
  
# Pacotes ====

library(tidyverse)
library(RODBC)
library(stringr)
library(purrr)
library(haven)
library(zoo)
library(sidrar)
library(kableExtra)
library(survey)

# Funções ====
# Gerar função oposta ao %in% 
`%notin%` <- Negate(`%in%`)


plano <- function(base){
   svydesign(id = ~1, 
             weights = ~FATOR_ANO, 
             data = base)
}


# Função que faz a consulta ao DB
pedSQL <- function(x){
   dados = sqlQuery(db, paste0("SELECT * FROM ", x))
   names(dados) = str_to_upper(names(dados))
   print(x)
   return(dados)
}

# Conexão ao banco de dados ====
db <- RODBC::odbcConnect("db_codeplan",uid=('codeplan'),pwd=('codeplan'))

## PED 2018 a 2022 ====

### PED 2018 a 2022 ====
ped_18_22 <- map_dfr(paste0('DB_CODEPLAN.ped.NovaPEDDF', c(2018,2020,2021,2022)),pedSQL)

# Tratamento dos dados ====
## Seleção ====
### PED 2018 a 2022 ====     
dados18_22 <- ped_18_22 |> 
   select(FATOR,
          AAMM,
          C030,
          SIT, 
          F210,       # No seu trabalho principal, o Sr. é
          F220,       # Que tipo de Empregado o Sr. é
          POS,
          CNAE,       #Ramo de atividade dos ocupados no trabalho principal
          SETOR_CNAE, #Setor de atividade dos ocupados no trabalho principal
          F470,       #Quanto o Sr. recebeu no mês passado por esse trabalho?
          F481,       #Quanto o Sr. recebeu no mês passado por esse trabalho? - remuneração contratual
          F280,       # O Sr. contribui para a previdência social pública (INSS)? 
          F190)       #Qual a atividade do negócio ou da empresa que lhe paga?


## Variável de renda ====

#'*Obs (DIEESE)*
#Sugere-se excluir os valores muito altos do rendimento do trabalho principal. 
#Para definir esses valores, deve-se analisar a distribuição desse rendimento 
#no mês. Caso não hajam tais valores, é necessário considerar como corte superior
#o valor 9.999.999, que é o limite máximo de captação dessa variável. O valor 
#10.000.001 significa que o indivíduo respondente não declarou a renda.


### 2018 a 2022 =====
renda18_22 <- dados18_22 |> 
   filter(SIT == 4,                            # Filtra Ocupados
          F210 != 12 & F220 %notin% c(3,6),
          F481 != 0,
          F470 != 0,
   ) |>
   mutate(RENDA = as.numeric(ifelse(F470 == 1e9 ,F481,F470))) |>
   filter(RENDA < 1e7-1)

ped18_22 <- ped18_22 |>
   # Criar variável de ano e fator anual
   mutate(ANO = substr(AAMM,start = 1,stop = 4), 
          FATOR_ANO = FATOR/12,
          CNAE_n = CNAE
          ) |> 
   select(AAMM,ANO,FATOR,FATOR_ANO, C030, SIT,F210,F220,F280,F190,POS,FATOR_ANO,RENDA,RENDA_R,CNAE,SETOR_CNAE,CNAE_n )

### Renomeando as categorias =====
ped18_22$CNAE_n <- factor(ped18_22$CNAE_n, levels = c("1005",  "1035",  "1036",  "2000", "3000",  "4000",  "5100",  "5200",  "5300",  "5400", "5500",  "5600",  "5900",  "9999"),
                  labels = c( "Indústrias Extrativas", 
                              "Eletricidade e Gás", 
                              "Água, Esgoto,Atividades de Gestão de Resíduos e Descontaminação",
                              "Indústria de Transformação",
                              "Construção",
                              "Comércio",
                              "Transporte, Armazenagem e Correio", 
                              "Informação e Comunicação; Atividades Financeiras, Seguros e Relacionados,Atividades.Profissionais, Científicas e Técnica",
                              "Atividades Administrativas e Serviços Complementares", 
                              "Administração Pública, Defesa e Seguridade Social, Educação, Saúde Humana e Serviços Sociais", 
                              "Alojamento e Alimentação; Outras Atividades de Serviços;Artes, Cultura, Esporte e Recreação", 
                              "Atividades Imobiliárias", 
                              "Serviços Domésticos",
                              "Demais Setores" ))

ped_ativ<- ped18_22 %>%
   select(ANO, F190, FATOR_ANO) |>
   group_by(ANO, F190) |>
   summarise(n_trabalhadores = n())


saveRDS(ped_ativ, "C:/Users/Barbara.Carrijo/Desktop/copia de segurança/NT_desigualdade/ped_ativ.RDS")
#svytable(formula = ~ ANO + F190, design = plano(ped_ativ))

### Informais =====

# F280 - O Sr. contribui para a previdência social pública (INSS)? 
# 1- sim, 2 - não, 3 - não sabe e 10 não se aplica

# se POS = Assalariado no setor privado SEM CARTEIRA ou Autônomo para o público e Não contribui para previdência OU 
#Autônomo para a empresa e Não contribui para previdência, então informais será igual a 1, se não igual a 2. 



ped_informais<- ped18_22 %>%
   mutate(informais= case_when(POS==2|(POS==5 & F280==2)|(POS==6 & F280==2)~1,
                               TRUE~2)) |> 
   select(ANO,CNAE, CNAE_n, CNAE_n,informais) |> 
   group_by(ANO, CNAE,CNAE_n,informais) |>
   summarise(n_trabalhadores = n())
   
saveRDS(ped_informais, "C:/Users/Barbara.Carrijo/Desktop/copia de segurança/NT_desigualdade/ped_informais.RDS") 




# Função para cálculo da média móvel para rendimentos ----

mov_ave1 <- function(cat){
  cat.serie <- ts(cat)                       # serie temporal
  categoria <- round(SMA(cat.serie, n=3))         # calcula medias moveis (MA) de uma serie
  # categoria <- categoria[-c(1:2)]
  return(categoria)
}

# Função de cálculo do rendimento real médio ----

rend_real_med <- function(arquivo,var1,categorias,periodo){
  if(periodo=="mensal"){
  designz <- svydesign(id=~1, weights = ~FATOR, data = arquivo) 
  # var1<-var1
  # print(var)
  #tabela rendimento por grupos
  # tab_g<-svyby(formula=update(~renda, paste0("~AAMM +",var1)), designz, na.rm = TRUE, svymean) %>%
  # if(tipo=="mensal"){
  tab_g<-svyby(~renda,  as.formula(paste0("~AAMM +",var1)), designz, na.rm = TRUE, svymean) %>%
    as.data.frame.matrix()%>%
    mutate(AAMM1=as.yearmon(as.character(AAMM), "%Y%m")-1/12)%>%
    left_join(INPC,by="AAMM1") %>%
    mutate(renda=round(renda),
           renda_at=renda*inflator)%>%
    group_by(eval(parse(text=var1))) %>%
    mutate(media=mov_ave1(renda_at)) %>%
    ungroup()%>%
    pivot_wider(id_cols="AAMM",
                names_from=var1,
                values_from="media",
                names_prefix=var1)%>%
    # filter(!is.na(SETOR_CNAE2000))
    na.exclude
  # Agrega o valor médio dos rendimentos para o total de assalariados
  
  tab<-svyby(~renda, ~AAMM, designz, na.rm = TRUE, svymean) %>%
    as.data.frame.matrix() %>%
    mutate(AAMM1=as.yearmon(as.character(AAMM), "%Y%m")-1/12)%>%
    left_join(INPC,by="AAMM1") %>%
    mutate(renda=round(renda),
           renda_at=renda*inflator)%>%
    mutate(media=mov_ave1(renda_at))%>%
    filter(!is.na(media)) %>%
    dplyr::select("AAMM", "media") %>%
    left_join(tab_g) %>%
    `colnames<-`(categorias)
   }
  
   if(periodo=="anual"){
     ###anual
     designz <- svydesign(id=~1, weights = ~FATOR1, data = arquivo) 
     # var1="POS1"
     
     tab_g<-svyby(~renda_at,  as.formula(paste0("~ano +",var1)), designz, na.rm = TRUE, svymean) %>%
       as.data.frame.matrix()%>% 
       mutate(renda_at=round(renda_at)) %>% 
       pivot_wider(id_cols="ano",
                   names_from=var1,
                   values_from="renda_at",
                   names_prefix=var1)%>%
       # filter(!is.na(SETOR_CNAE2000))
       na.exclude
     
    # Agrega o valor médio dos rendimentos para o total de assalariados
     
     tab<-svyby(~renda_at, ~ano, designz, na.rm = TRUE, svymean) %>%
       mutate(renda_at=round(renda_at)) %>% 
       as.data.frame.matrix()%>%
       dplyr::select(-c("se")) %>% 
       left_join(tab_g) %>%
       `colnames<-`(categorias)
  }
  
  return(tab)
}

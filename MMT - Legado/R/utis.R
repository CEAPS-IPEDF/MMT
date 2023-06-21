# Funções ----
## Média Movel ----
mov_ave <- function(cat){
  cat.serie <- ts(cat) #objeto de série temporal
  categoria <- round(SMA(cat.serie, n=3))/1000 #arredonda e muda a escala
  return(categoria)
}
## Plano Amostral ----
plano <- function(base){
  svydesign(id = ~1, 
            weights = ~FATOR, 
            data = base)
}
## Taxa de Desemprego ----
tx_desemp <- function(x){
  taxa_desemprego <- data.frame(ano_mes = substr(lubridate::ym(x[-c(1:2),1]),1,7),
                                tx_desemp = 100*(rowSums(x[-c(1:2),3:5])/rowSums(x[-c(1:2),c(3:6)])))
  return(taxa_desemprego)
}
## Gerar Tabela-Base ----
tabela_ped <- function(base,x,y,filtro, anual = FALSE){
  if (anual == TRUE) {
    
    if (missing(y)){
      
      var <- x
      svytable(formula = as.formula(paste0("~AAMM +",var)), 
               design = plano(ped)) %>%
        as.data.frame.matrix() %>% 
        rownames_to_column(var="ano") %>% 
        mutate(ano=substr(ano,start = 1,stop = 4)) %>% 
        group_by(ano) %>% 
        summarise_all(mean) %>% 
        as.data.frame()
      
    } else {
      vars <- c(x,y)
      svytable(formula = as.formula(paste("~AAMM +",paste(vars,collapse = "+"))), 
               design = plano(ped))[,,y = filtro] %>%
        as.data.frame.matrix() %>% 
        rownames_to_column(var="ano") %>% 
        mutate(ano=substr(ano,start = 1,stop = 4)) %>% 
        group_by(ano) %>% 
        summarise_all(mean) %>% 
        as.data.frame()
    }
    
    
  } else {
    
    if (missing(y)){
      
      var <- x
      svytable(formula = as.formula(paste0("~AAMM +",var)), 
               design = plano(ped)) %>%
        as.data.frame.matrix() %>% 
        mutate_all(mov_ave) %>%
        rownames_to_column(var="mes_ano")
      
    } else {
      vars <- c(x,y)
      svytable(formula = as.formula(paste("~AAMM +",paste(vars,collapse = "+"))), 
               design = plano(ped))[,,y = filtro] %>%
        as.data.frame.matrix() %>% 
        mutate_all(mov_ave) %>%
        rownames_to_column(var="mes_ano")
    }
  }
}

# rotina
desafio de dados
###################################################
###         desafio de dados                    ###
###         Autor: Andre Jose                   ###
################################################### 

library(data.table)
 library(lubridate)
  library(openxlsx)
   library(tidyverse)
    library(stringr)
     library(scales)
      library(bit64)
       library (readxl)
## abrir as bases e criar uma serie ---- 
arquivos.nomes <- list.files("C:/Users/Andre/Desktop/desafio/saresp", full.names = T)
 data <- str_sub(arquivos.nomes, start = 42, end = 43)
  portal <- list()
   for (i in 1:length(arquivos.nomes)){
    portal[[i]] <- read.xlsx(arquivos.nomes[i])
     portal[[i]]$data <- data[i]
     }
  
  ## juntar 
  lista <- rbindlist((portal), use.names = T, fill = T)
  
  ## tratar ano a ano - 2018----
s18 <- lista %>% 
  filter(data =="18")%>% 
    filter(SERIE_ANO =="9º Ano EF")
     ## padronizar as materias
s18$ds_comp[s18$ds_comp %in% c("LÍNGUA PORTUGUESA")] <- "port"
 s18$ds_comp[s18$ds_comp %in% c("MATEMÁTICA")] <- "mat"
  s18 <- s18[, c("CODESC", "NOMESC", "SERIE_ANO","ds_comp","medprof","data")]
  ## tratar ano a ano - 2017----
  s17 <- lista %>% 
    filter(data =="17")%>% 
    filter(SERIE_ANO =="9Âº Ano EF")%>%
    filter(periodo =="GERAL")
  ## padronizar as materias
  table(s17$ds_comp)
  s17$ds_comp[s17$ds_comp %in% c("LÃ\u008dNGUA PORTUGUESA")] <- "port"
  s17$ds_comp[s17$ds_comp %in% c("MATEMÃ\u0081TICA")] <- "mat"
  s17 <- s17[, c("CODESC", "NOMESC", "SERIE_ANO","ds_comp","medprof","data")]
  
  ## tratar ano a ano - 2016----
  s16 <- lista %>% 
    filter(data =="16")%>% 
    filter(SERIE_ANO =="9Âº Ano EF")%>%
    filter(periodo =="GERAL")
  ## padronizar as materias
  table(s16$ds_comp)
  s16$ds_comp[s16$ds_comp %in% c("LÃ\u008dNGUA PORTUGUESA")] <- "port"
  s16$ds_comp[s16$ds_comp %in% c("MATEMÃ\u0081TICA")] <- "mat"
  s16 <- s16[, c("CODESC", "NOMESC", "SERIE_ANO","ds_comp","medprof","data")]

  ## tratar ano a ano - 2015----
  s15 <- lista %>% 
    filter(data =="15")%>% 
    filter(SERIE_ANO =="9Âº Ano EF")%>%
    filter(periodo =="GERAL")
  ## padronizar as materias
  table(s15$ds_comp)
  s15$ds_comp[s15$ds_comp %in% c("LÃ\u008dNGUA PORTUGUESA")] <- "port"
  s15$ds_comp[s15$ds_comp %in% c("MATEMÃ\u0081TICA")] <- "mat"
  s15 <- s15[, c("CODESC", "NOMESC", "SERIE_ANO","ds_comp","medprof","data")]
  
  ## tratar ano a ano - 2014----
  s14 <- lista %>% 
    filter(data =="14")%>% 
    filter(SERIE_ANO =="9Âº Ano EF")%>%
    filter(periodo =="GERAL")
  ## padronizar as materias
  table(s14$ds_comp)
  s14$ds_comp[s14$ds_comp %in% c("LÃ\u008dNGUA PORTUGUESA")] <- "port"
  s14$ds_comp[s14$ds_comp %in% c("MATEMÃ\u0081TICA")] <- "mat"
  s14 <- s14[, c("CODESC", "NOMESC", "SERIE_ANO","ds_comp","medprof","data")]
  s14 <- subset(s14, ds_comp %in% c("port", "mat"))
  ## tratar ano a ano - 2013----
  s13 <- lista %>% 
    filter(data =="13")%>% 
    filter(SERIE_ANO =="9Âº Ano EF")%>%
    filter(periodo =="GERAL")
  ## padronizar as materias
  table(s13$ds_comp)
  s13$ds_comp[s13$ds_comp %in% c("LÃ\u008dNGUA PORTUGUESA")] <- "port"
  s13$ds_comp[s13$ds_comp %in% c("MATEMÃ\u0081TICA")] <- "mat"
  s13 <- s13[, c("CODESC", "NOMESC", "SERIE_ANO","ds_comp","medprof","data")]
  s13 <- subset(s13, ds_comp %in% c("port", "mat"))
  ## tratar ano a ano - 2012----
  s12 <- lista %>% 
    filter(data =="12")%>% 
    filter(SERIE_ANO =="9Âº Ano EF")%>%
    filter(periodo =="GERAL")
  ## padronizar as materias
  table(s12$ds_comp)
  s12$ds_comp[s12$ds_comp %in% c("LÃ\u008dNGUA PORTUGUESA")] <- "port"
  s12$ds_comp[s12$ds_comp %in% c("MATEMÃ\u0081TICA")] <- "mat"
  s12 <- s12[, c("CODESC", "NOMESC", "SERIE_ANO","ds_comp","medprof","data")]
  s12 <- subset(s12, ds_comp %in% c("port", "mat"))
  ## tratar ano a ano - 2011----
  s11 <- lista %>% 
    filter(data =="11")%>% 
    filter(SERIE_ANO =="9Âº Ano EF")%>%
    filter(periodo =="GERAL")
  ## padronizar as materias
  table(s11$ds_comp)
  s11$ds_comp[s11$ds_comp %in% c("LÃ\u008dNGUA PORTUGUESA")] <- "port"
  s11$ds_comp[s11$ds_comp %in% c("MATEMÃ\u0081TICA")] <- "mat"
  s11 <- s11[, c("CODESC", "NOMESC", "SERIE_ANO","ds_comp","medprof","data")]
  s11 <- subset(s11, ds_comp %in% c("port", "mat"))
  
  ## fazer o valor base ----  
  total <- rbind(s11,s12,s13,s14,s15,s16,s17,s18)
   total <- total[, c("CODESC","ds_comp")]
    total$id <- paste(total$CODESC,total$ds_comp, sep="")
     total  <- as.data.frame(total[!duplicated(total$id),])
  ## tratar cada ano -----
## arrumar 2018
  s18$id <- paste(s18$CODESC,s18$ds_comp, sep="")
   s18 <- s18[, c("id" , "medprof")]
    names(s18)[2] <- c( "medprof_18")
  ## arrumar 2017
  s17$id <- paste(s17$CODESC,s17$ds_comp, sep="")
   s17 <- s17[, c("id" , "medprof")]
    names(s17)[2] <- c( "medprof_17")
  ## arrumar 2016
  s16$id <- paste(s16$CODESC,s16$ds_comp, sep="")
   s16 <- s16[, c("id" , "medprof")]
    names(s16)[2] <- c( "medprof_16")
  ## arrumar 2015
  s15$id <- paste(s15$CODESC,s15$ds_comp, sep="")
   s15 <- s15[, c("id" , "medprof")]
    names(s15)[2] <- c( "medprof_15")
  ## arrumar 2014
  s14$id <- paste(s14$CODESC,s14$ds_comp, sep="")
   s14 <- s14[, c("id" , "medprof")]
    names(s14)[2] <- c( "medprof_14")
  ## arrumar 2013
  s13$id <- paste(s13$CODESC,s13$ds_comp, sep="")
   s13 <- s13[, c("id" , "medprof")]
    names(s13)[2] <- c( "medprof_13")
  ## arrumar 2012
  s12$id <- paste(s12$CODESC,s12$ds_comp, sep="")
   s12 <- s12[, c("id" , "medprof")]
    names(s12)[2] <- c( "medprof_12")
  ## arrumar 2011
  s11$id <- paste(s11$CODESC,s11$ds_comp, sep="")
   s11 <- s11[, c("id" , "medprof")]
    names(s11)[2] <- c( "medprof_11")
    
  ## pareamento ----
    base <- merge(total, s11, by = c("id" = "id" ), all.x = TRUE)%>%
      merge(., s12, all.x = TRUE) %>%
      merge(., s13, all.x = TRUE) %>%
      merge(., s14, all.x = TRUE)%>% 
      merge(., s15, all.x = TRUE) %>%
      merge(., s16, all.x = TRUE) %>%
      merge(., s17, all.x = TRUE) %>%
      merge(., s18, all.x = TRUE) 
    base <- base[!duplicated(base$id),]
    ## exclusao de NA
    #base <- na.exclude(base)
##  (2) Analise dos rendimentos ----  
     ## 2.a escolas adequadas----
    mat_ad <- base %>% 
      filter(ds_comp =="mat")%>% 
      filter(medprof_11 > 250)%>%
      filter(medprof_12 > 250)%>%
      filter(medprof_13 > 250)%>%
      filter(medprof_14 > 250)%>%
      filter(medprof_15 > 250)%>%
      filter(medprof_16 > 250)%>%
      filter(medprof_17 > 250)%>%
      filter(medprof_18 > 250)
      ## criar um DF para comparar----  
      mat_ad$tipo <- "adequado"  
      mat_ad <- mat_ad[,c(2,12)]
      ## 2.b escolas nao adequadas----
      mat_nad <- base %>% 
      filter(ds_comp =="mat")%>% 
      filter(medprof_11 < 250)%>%
      filter(medprof_12 < 250)%>%
      filter(medprof_13 < 250)%>%
      filter(medprof_14 < 250)%>%
      filter(medprof_15 < 250)%>%
      filter(medprof_16 < 250)%>%
      filter(medprof_17 < 250)%>%
      filter(medprof_18 < 250)
      ## criar um DF para comparar----  
      mat_nad$tipo <- "inadequado"  
      mat_nad <- mat_nad[,c(2,12)]
## (3) Analise com a quantidade de alunos por sala ----
        qt <- read_excel("C:/Users/Andre/Desktop/desafio/qt_alunos.xlsx")
    qt <- qt %>% 
    filter(TipoEnsino =="ENSINO FUNDAMENTAL DE 9 ANOS")%>% 
    filter(SERIE == 9)%>%
    filter(TIPOCLASSE_DESC == "CLASSE COMUM")  
    ## selecionar colunas
    qt <- qt[, c(1,6,17)]
    ## agrupar a mediana de alunos por ano e escola
    qt <- qt %>% 
      group_by(ANO,COD_ESC) %>% summarise(median(QTDE_ALUNOS, na.rm = T))
    ## parear o resultado --- adequado
    qt$CODESC <- qt$COD_ESC
    qt_ad <- merge(qt, mat_ad, by = c("CODESC" = "CODESC" ), all.x = TRUE)
    qt_ad <- qt_ad %>% filter(tipo == "adequado")
    qt_ad <- qt_ad[, c(1,2,4)]
    names(qt_ad)[3] <- c( "med_alunos" )
    qt_ad <- qt_ad %>% 
    group_by(ANO) %>% summarise(mean(med_alunos, na.rm = T))
    ## parear o resultado --- inadequado
    qt_n <- merge(qt, mat_nad, by = c("CODESC" = "CODESC" ), all.x = TRUE)
    qt_n <- qt_n %>% filter(tipo == "inadequado")
    qt_n <- qt_n[, c(1,2,4)]
    names(qt_n)[3] <- c( "med_alunos" )
    qt_n <- qt_n %>% 
      group_by(ANO) %>% summarise(mean(med_alunos, na.rm = T))
## (4) Analise das dependencias da escola------ 
 dep <- read_excel("C:/Users/Andre/Desktop/desafio/depen_esc.xlsx")  
    ## separar as colunas
    dep <- dep[, c(5,13,18,19,20,21,22,28,32,64,77,78,82,155)]
    dep_ad <- merge(mat_ad, dep, by = c("CODESC" = "CODESC" ), all.x = TRUE)
    dep_nad <- merge(mat_nad, dep, by = c("CODESC" = "CODESC" ), all.x = TRUE)
## (5) Analise social -----   
social <- read_excel("C:/Users/Andre/Desktop/desafio/social.xlsx")
social <- social[,c(1,5)]
social_ad <- merge(mat_ad,social,by = c("CODESC" = "CODESC"), all.x = T)
social_nad <- merge(mat_nad,social,by = c("CODESC" = "CODESC"), all.x = T)
## (6) Mudança de diretores ----
  diretor<- read_excel("C:/Users/Andre/Desktop/desafio/diretores_de_escola.xlsx")
  diretor <- diretor %>% 
    filter(`2014` == "SIM")%>% 
    filter(`2015` == "SIM")%>%
    filter(`2016` == "SIM")%>%
    filter(`2017` == "SIM")%>%
    filter(`2018` == "SIM")
    diretor$freq <- 1
    diretor <- diretor[,c(1,17)]
    names(diretor)[1]<- c("CODESC")
    diretor_ad <- merge(mat_ad, diretor, by = c("CODESC" = "CODESC" ), all.x = TRUE)
    diretor_ad <- diretor_ad[!duplicated(diretor_ad$CODESC),]
    diretor_nad <- merge(mat_nad, diretor, by = c("CODESC" = "CODESC" ), all.x = TRUE)
    diretor_nad <- diretor_nad[!duplicated(diretor_nad$CODESC),]
## (7) salvar os arquivos -----   
    list_of_datasets <- list("base" = base,
                             "dep_ad" = dep_ad,
                             "dep_nad" = dep_nad,
                             "diretor_ad" = diretor_ad,
                             "diretor_nad" = diretor_nad,
                              "qt_ad" = qt_ad,
                             "qt_n"  = qt_n,
                             "social_ad" = social_ad,
                             "social_nad" = social_nad
    )  
    
    write.xlsx(list_of_datasets, file = "C:/Users/Andre/Desktop/desafio/desafio.xlsx")                
    
    
    
   
    

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("dplyr")
require(ggplot2)

setwd( "~/Documents/Maestria_2022/DMEYF/" )
dataset <- fread("datasets/competencia3_2022.csv.gz")

data_eval  <- dataset[ foto_mes== 202105  ]
data_eval <- data_eval[,c("numero_de_cliente","clase_ternaria")]

#cargo cada predicción por número de corte y genero un dataset por cada semilla. Luego repito por cada BO.

semillas <- c(609277, 425783, 493729, 315697, 239069)
BOS <- c('_01_022_','_02_053_')
cortes <- c('07000','07500','08000','08500','09000','09500','10000','10500','11000')
nombre_base <- 'ZZ9410_basal_'

BO_idx <- 1
for (BO in BOS){
  semilla_idx <- 1
  temp <- data.table()
  for (semilla in semillas){
    data_eval  <- dataset[ foto_mes== 202105,c("numero_de_cliente","clase_ternaria")  ]
    for (corte in cortes){
      df_pred <- fread(paste0('experimentos/basal/',nombre_base,semilla,BO,corte,'.csv'))
      data_eval <- df_pred %>% 
        right_join(., data_eval, by = "numero_de_cliente") 
      data_eval[  , paste0('ganancia_',corte) :=  ifelse( clase_ternaria=="BAJA+2" & Predicted ==1, 78000, -2000 ) ]
      data_eval[  , paste0('ganancia_',corte) :=  ifelse( Predicted == 0, 0, get(paste0('ganancia_',corte)) ) ]
      data_eval[, Predicted:=NULL]
    }
    data_eval[, clase_ternaria:=NULL]
    data_eval[, numero_de_cliente:=NULL]
    temp <- rbind(temp,data_eval%>%
      summarise_all(sum))
    assign(paste0('BO_',BO_idx,'_Semilla_', semilla_idx),data_eval)
    semilla_idx <- semilla_idx + 1
  }
  assign(paste0('BO_',BO_idx,'_summary'),temp)
  BO_idx <- BO_idx + 1
}

col_semilla <- c("Semilla 1","Semilla 2","Semilla 3","Semilla 4","Semilla 5", "Media" )
BO_1_summary <- rbind(BO_1_summary, BO_1_summary %>% summarise_all(mean))
BO_2_summary <- rbind(BO_2_summary, BO_2_summary %>% summarise_all(mean))

BO_1_summary <- cbind(col_semilla, BO_1_summary)
BO_2_summary <- cbind(col_semilla, BO_2_summary)

melteado <-melt(BO_1_summary, id = c('col_semilla'))


ggplot(data=melteado, aes(x=variable, y=value, group=col_semilla)) +
  geom_line(aes(color=col_semilla))+
  geom_point() 


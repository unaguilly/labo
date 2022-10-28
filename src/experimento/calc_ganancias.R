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

#genero una tabla con las medias y desvíos por corte
BO_1_media <-BO_1_summary %>% summarise_all(mean)
BO_1_media <-rbind(BO_1_media, BO_1_summary %>% summarise_all(sd))
BO_2_media <-BO_2_summary %>% summarise_all(mean)
BO_2_media <-rbind(BO_2_media, BO_2_summary %>% summarise_all(sd))

#formateo la tabla con nombres de filas y columnas y agrego la media de cada corte
col_semilla <- c("Semilla 1","Semilla 2","Semilla 3","Semilla 4","Semilla 5", "Media" )
BO_1_summary <- rbind(BO_1_summary, BO_1_summary %>% summarise_all(mean))
BO_2_summary <- rbind(BO_2_summary, BO_2_summary %>% summarise_all(mean))
BO_1_summary <- cbind(col_semilla, BO_1_summary)
BO_2_summary <- cbind(col_semilla, BO_2_summary)

colnames(BO_1_summary) <- c('col_semilla','7000','7500','8000','8500','9000','9500','10000','10500','11000')
colnames(BO_2_summary) <- c('col_semilla','7000','7500','8000','8500','9000','9500','10000','10500','11000')

#cambio estructura para poder graficar
BO_1_melt <-melt(BO_1_summary, id = c('col_semilla'))
BO_2_melt <-melt(BO_2_summary, id = c('col_semilla'))

#ploteo
p_bo_1 <- ggplot(data=BO_1_melt, aes(x=variable, y=value, group=col_semilla)) +
  geom_line(aes(color=col_semilla, size= col_semilla))+
  labs(x="Corte", y="Ganancia", color="Semilla", size="Semilla", title = "Basal - Modelo 1")+
 scale_size_manual(values = c("Semilla 1" = 0.5,"Semilla 2" = 0.5,"Semilla 3" = 0.5,
                               "Semilla 4" = 0.5,"Semilla 5" = 0.5, "Media" = 1.5 ))
  

p_bo_2 <- ggplot(data=BO_2_melt, aes(x=variable, y=value, group=col_semilla)) +
  geom_line(aes(color=col_semilla, size= col_semilla))+
  labs(x="Corte", y="Ganancia", color="Semilla", size="Semilla", title = "Basal - Modelo 2")+
  scale_size_manual(values = c("Semilla 1" = 0.5,"Semilla 2" = 0.5,"Semilla 3" = 0.5,
                               "Semilla 4" = 0.5,"Semilla 5" = 0.5, "Media" = 1.5 ))

p_bo_2

#----------- evaluación PASADO



semillas <- c(609277, 425783, 493729, 315697, 239069)
BOS <- c('_01_054_','_02_036_')
cortes <- c('07000','07500','08000','08500','09000','09500','10000','10500','11000')
nombre_base <- 'ZZ9410_pasado_'
dias <- c(202101,202102,202103,202104, 202105)

for (dia in dias){
  BO_idx <- 1
  for (BO in BOS){
    semilla_idx <- 1
    temp <- data.table()
    for (semilla in semillas){
      data_eval  <- dataset[ foto_mes== dia,c("numero_de_cliente","clase_ternaria")  ]
      for (corte in cortes){
        df_pred <- fread(paste0('experimentos/pasado/',nombre_base,semilla,'_',dia,BO,corte,'.csv'))
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
      assign(paste0(dia,'_BO_',BO_idx,'_Semilla_', semilla_idx),data_eval)
      semilla_idx <- semilla_idx + 1
    }
    assign(paste0(dia,'_BO_',BO_idx,'_summary'),temp)
    BO_idx <- BO_idx + 1
  }
}


resumenes1 <- c('202101_BO_1_summary', '202102_BO_1_summary','202103_BO_1_summary','202104_BO_1_summary','202105_BO_1_summary')
resumenes2 <- c('202101_BO_2_summary', '202102_BO_2_summary','202103_BO_2_summary','202104_BO_2_summary', '202105_BO_2_summary')
melts1 <- c('202101_BO_1_melt', '202102_BO_1_melt','202103_BO_1_melt','202104_BO_1_melt','202105_BO_1_melt')
melts2 <- c('202101_BO_2_melt', '202102_BO_2_melt','202103_BO_2_melt','202104_BO_2_melt', '202105_BO_2_melt')

#genero una tabla con las medias y desvíos por corte
for (i in (1:5)){
  temp <- get(resumenes1[i]) %>% summarise_all(mean)
  temp <- rbind(temp, get(resumenes1[i]) %>% summarise_all(sd))
  assign(paste0(dias[i],'_BO_1_media'), temp )
  temp2 <- get(resumenes2[i]) %>% summarise_all(mean)
  temp2 <- rbind(temp2, get(resumenes2[i]) %>% summarise_all(sd))
  assign(paste0(dias[i],'_BO_2_media'), temp2 )
}



#formateo la tabla con nombres de filas y columnas y agrego la media de cada corte
col_semilla <- c("Semilla 1","Semilla 2","Semilla 3","Semilla 4","Semilla 5", "Media" )
for (i in (1:5)){
  temp <- get(resumenes1[i])
  temp_mean <- get(resumenes1[i]) %>% summarise_all(mean)
  assign(paste0(resumenes1[i]), rbind(temp, temp_mean))
  assign(paste0(resumenes1[i]), cbind(col_semilla, get(resumenes1[i]) ))
  temp2 <- get(resumenes2[i])
  temp_mean2 <- get(resumenes2[i]) %>% summarise_all(mean)
  assign(paste0(resumenes2[i]), rbind(temp2, temp_mean2))
  assign(paste0(resumenes2[i]), cbind(col_semilla, get(resumenes2[i]) ))

}



for (i in (1:5)){
  temp <- get(resumenes1[i])
  colnames(temp) <- c('col_semilla','7000','7500','8000','8500','9000','9500','10000','10500','11000')
  assign(paste0(resumenes1[i]), temp)
  temp2 <- get(resumenes2[i])
  colnames(temp2) <- c('col_semilla','7000','7500','8000','8500','9000','9500','10000','10500','11000')
  assign(paste0(resumenes2[i]), temp2)
}

#cambio estructura para poder graficar
for (i in (1:5)){
  temp <- get(resumenes1[i])
  assign(paste(melts1[i]), melt(temp, id = c('col_semilla')))
  temp2 <- get(resumenes2[i])
  assign(paste(melts2[i]), melt(temp2, id = c('col_semilla')))
}



#ploteo


plot_list1 = list()
for (i in (1:5)){
  p <- ggplot(data=get(melts1[i]), aes(x=variable, y=value, group=col_semilla)) +
    geom_line(aes(color=col_semilla, size= col_semilla))+
    labs(x="Corte", y="Ganancia", color="Semilla", size="Semilla", title = paste0(dias[i]," - Modelo 1"))+
    scale_size_manual(values = c("Semilla 1" = 0.5,"Semilla 2" = 0.5,"Semilla 3" = 0.5,
                                 "Semilla 4" = 0.5,"Semilla 5" = 0.5, "Media" = 1.5 ))
  plot_list1[[i]] = p
}

pdf("pasado_BO1.pdf")
for (i in (1:5)){
  print(plot_list1[[i]])
}
dev.off()

plot_list2 = list()
for (i in (1:5)){
  p <- ggplot(data=get(melts2[i]), aes(x=variable, y=value, group=col_semilla)) +
  geom_line(aes(color=col_semilla, size= col_semilla))+
  labs(x="Corte", y="Ganancia", color="Semilla", size="Semilla", title = paste0(dias[i]," - Modelo 2"))+
  scale_size_manual(values = c("Semilla 1" = 0.5,"Semilla 2" = 0.5,"Semilla 3" = 0.5,
                               "Semilla 4" = 0.5,"Semilla 5" = 0.5, "Media" = 1.5 ))
  plot_list2[[i]] = p
}

pdf("pasado_BO2.pdf")
for (i in (1:5)){
  print(plot_list2[[i]])
}
dev.off()
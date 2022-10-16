rm( list=ls() ) #remove all objects
gc()       #garbage collection

require("data.table")

dataset <- fread("~/Documents/Maestria_22/DMEYF/datasets/competencia2_2022.csv.gz")


# for (col in cols) {
#  
#  dataset[ foto_mes==202103 , paste(col,"neg_rank", sep = "_") := -(frank(-dataset[ foto_mes==202103 , get(col) ], ties.method="min", na.last="keep" )-1)/(nrow(dataset[ foto_mes==202103 , ])-1)]
#  dataset[ foto_mes==202105 , paste(col,"neg_rank", sep = "_") := (frank(dataset[ foto_mes==202105 , get(col) ], ties.method="min", na.last="keep" )-1)/(nrow(dataset[ foto_mes==202105 , ])-1)]
#  dataset[  , paste(get(col),"rank", sep = "_") := ifelse( paste(col,"neg_rank", sep = "_")< 0, paste(col,"neg_rank", sep = "_"), paste(col,"pos_rank", sep = "_")) ]
# }
#  
# dataset[mcomisiones < 0]
# 
# dataset[ foto_mes==202103 , mcomisiones_neg_rank := -(frank(-dataset[ foto_mes==202103 , mcomisiones_neg ], ties.method="min", na.last="keep" )-1)/(nrow(dataset[ foto_mes==202103 , ])-1)]
# dataset[ foto_mes==202103 , mcomisiones_pos_rank := (frank(dataset[ foto_mes==202103 , mcomisiones_pos ], ties.method="min", na.last="keep" )-1)/(nrow(dataset[ foto_mes==202103 , ])-1)]
# dataset[ foto_mes==202105 , mcomisiones_neg_rank := -(frank(-dataset[ foto_mes==202105 , mcomisiones_neg ], ties.method="min", na.last="keep" )-1)/(nrow(dataset[ foto_mes==202105 , ])-1)]
# dataset[ foto_mes==202105 , mcomisiones_pos_rank := (frank(dataset[ foto_mes==202105 , mcomisiones_pos ], ties.method="min", na.last="keep" )-1)/(nrow(dataset[ foto_mes==202105 , ])-1)]
# dataset[  , mcomisiones_rank := ifelse(mcomisiones_neg_rank< 0, mcomisiones_neg_rank, mcomisiones_pos_rank) ]
# dataset <- dataset[, -c("mcomisiones", "mcomisiones_neg_rank", "mcomisiones_pos_rank","mcomisiones_neg", "mcomisiones_pos")]


-----
  
cols_to_rank <- c("mcomisiones", "mcuenta_corriente", "mcaja_ahorro", "mcuentas_saldo",
                    "mtarjeta_visa_consumo", "mprestamos_personales", "cprestamos_prendarios", "cprestamos_hipotecarios", "cplazo_fijo",
                    "mpayroll", "ccuenta_debitos_automaticos", "mcuenta_debitos_automaticos", "ctarjeta_master_debitos_automaticos",
                    "mttarjeta_master_debitos_automaticos", "ccomisiones_mantenimiento", "mforex_sell",
                    "ctransferencias_emitidas","ccheques_depositados","ccheques_emitidos","chomebanking_transacciones",
                    "Master_mfinanciacion_limite","Master_Fvencimiento", "Master_Finiciomora","Master_msaldototal",
                    "Master_fultimo_cierre", "Master_mpagado", "Master_mpagominimo","Visa_mfinanciacion_limite", 
                    "Visa_msaldototal","Visa_mconsumospesos","Visa_madelantodolares", "Visa_fultimo_cierre",
                    "Visa_mpagosdolares", "Visa_mconsumototal", "Visa_mpagominimo")



neg_cols_to_add <- c()
pos_cols_to_add <- c()
pos_neg_cols_to_remove <- c() 
for( col in cols_to_rank) {
  if( dataset[ get(col) < 0, .N ] > 0 ) {
    pos_neg_cols_to_remove <- c(pos_neg_cols_to_remove, col)
    dataset[, paste0( col, "_neg" ) := ifelse( get(col)< 0, get(col), 0 ) ]
    dataset[, paste0( col, "_pos" ) := ifelse( get(col)> 0, get(col), 0 ) ]
    neg_cols_to_add <- c(neg_cols_to_add, paste0( col, "_neg" ) )
    pos_cols_to_add <- c(pos_cols_to_add, paste0( col, "_pos" ) )
    set(dataset, j=col, value = NULL)
  }
}

cols_to_rank <- cols_to_rank[! cols_to_rank %in% pos_neg_cols_to_remove]

 

#hago el rankeo con las que no tienen negativos
for (col in cols_to_rank) {
  dataset[ foto_mes==202103 , paste0(col,"_ranked") := (frank(dataset[ foto_mes==202103 , col, with = FALSE ], ties.method="min", na.last="keep" )-1)/(nrow(dataset[ foto_mes==202103 , ])-1)]
  dataset[ foto_mes==202105 , paste0(col,"_ranked") := (frank(dataset[ foto_mes==202105 , col, with = FALSE ], ties.method="min", na.last="keep" )-1)/(nrow(dataset[ foto_mes==202105 , ])-1)]
  set(dataset, j=col, value = NULL)
}

#hago el rankeo con las que sí tienen negativos
for (col in neg_cols_to_add) {
  dataset[ foto_mes==202103 , paste0(col,"_rank") := -(frank(-dataset[ foto_mes==202103 , col, with = FALSE ], ties.method="min", na.last="keep" )-1)/(nrow(dataset[ foto_mes==202103 , ])-1)]
  dataset[ foto_mes==202105 , paste0(col,"_rank") := -(frank(-dataset[ foto_mes==202105 , col, with = FALSE ], ties.method="min", na.last="keep" )-1)/(nrow(dataset[ foto_mes==202105 , ])-1)]
  set(dataset, j=col, value = NULL)
  }  

for (col in pos_cols_to_add) {
  dataset[ foto_mes==202103 , paste0(col,"_rank") := (frank(dataset[ foto_mes==202103 , col, with = FALSE ], ties.method="min", na.last="keep" )-1)/(nrow(dataset[ foto_mes==202103 , ])-1)]
  dataset[ foto_mes==202105 , paste0(col,"_rank") := (frank(dataset[ foto_mes==202105 , col, with = FALSE ], ties.method="min", na.last="keep" )-1)/(nrow(dataset[ foto_mes==202105 , ])-1)]    
  set(dataset, j=col, value = NULL)
  }

for (col in pos_neg_cols_to_remove){
  dataset[, paste0(col,"_ranked") := ifelse( get(paste0(col,"_neg_rank"))< 0, get(paste0(col,"_neg_rank")), get(paste0(col,"_pos_rank")))]
  set(dataset, j=c(paste0(col,"_neg_rank"), paste0(col,"_pos_rank")), value = NULL)
}

write.csv(dataset,"./datasets/FE_competencia2_2022.csv", row.names = FALSE)


#DT = dataset[ foto_mes==202103, c("mcomisiones", "mcomisiones_neg", "mcomisiones_pos", "mcomisiones_ranked" )]

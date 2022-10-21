rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")

dataset <- fread("~/Documents/Maestria_2022/DMEYF/datasets/competencia2_2022.csv.gz")


# for (col in cols) {
#   
#   dataset[ foto_mes==202103  , paste(col,"neg_rank", sep = "_")  := -(frank(-dataset[ foto_mes==202103 , get(col) ], ties.method="min", na.last="keep" )-1)/(nrow(dataset[ foto_mes==202103 , ])-1)]
#   dataset[ foto_mes==202105  , paste(col,"neg_rank", sep = "_")  :=  (frank(dataset[ foto_mes==202105 , get(col) ], ties.method="min", na.last="keep" )-1)/(nrow(dataset[ foto_mes==202105 , ])-1)]
#   dataset[   , paste(get(col),"rank", sep = "_")  := ifelse( paste(col,"neg_rank", sep = "_")< 0,  paste(col,"neg_rank", sep = "_"),  paste(col,"pos_rank", sep = "_")) ]
# }
#   
# dataset[mcomisiones < 0]
# 
# dataset[ foto_mes==202103  , mcomisiones_neg_rank  := -(frank(-dataset[ foto_mes==202103 , mcomisiones_neg ], ties.method="min", na.last="keep" )-1)/(nrow(dataset[ foto_mes==202103 , ])-1)]
# dataset[ foto_mes==202103  , mcomisiones_pos_rank  :=  (frank(dataset[ foto_mes==202103 , mcomisiones_pos ], ties.method="min", na.last="keep" )-1)/(nrow(dataset[ foto_mes==202103 , ])-1)]
# dataset[ foto_mes==202105  , mcomisiones_neg_rank  := -(frank(-dataset[ foto_mes==202105 , mcomisiones_neg ], ties.method="min", na.last="keep" )-1)/(nrow(dataset[ foto_mes==202105 , ])-1)]
# dataset[ foto_mes==202105  , mcomisiones_pos_rank  :=  (frank(dataset[ foto_mes==202105 , mcomisiones_pos ], ties.method="min", na.last="keep" )-1)/(nrow(dataset[ foto_mes==202105 , ])-1)]
# dataset[   , mcomisiones_rank  := ifelse(mcomisiones_neg_rank< 0, mcomisiones_neg_rank, mcomisiones_pos_rank) ]
# dataset <- dataset[, -c("mcomisiones", "mcomisiones_neg_rank", "mcomisiones_pos_rank","mcomisiones_neg", "mcomisiones_pos")]


-----

cols_to_rank <- c("mcomisiones",  "mcuenta_corriente", "mcaja_ahorro", "mcuentas_saldo",
              "mtarjeta_visa_consumo", "mprestamos_personales", "cprestamos_prendarios", "cprestamos_hipotecarios", "cplazo_fijo",
              "mpayroll", "ccuenta_debitos_automaticos", "mcuenta_debitos_automaticos", "ctarjeta_master_debitos_automaticos",
              "mttarjeta_master_debitos_automaticos", "ccomisiones_mantenimiento",  "mforex_sell",
              "ctransferencias_emitidas","ccheques_depositados","ccheques_emitidos","chomebanking_transacciones",
              "Master_mfinanciacion_limite","Master_Fvencimiento", "Master_Finiciomora","Master_msaldototal",
              "Master_fultimo_cierre", "Master_mpagado",  "Master_mpagominimo","Visa_mfinanciacion_limite", 
              "Visa_msaldototal","Visa_mconsumospesos","Visa_madelantodolares", "Visa_fultimo_cierre",
              "Visa_mpagosdolares", "Visa_mconsumototal", "Visa_mpagominimo")

pos_neg_cols <- c()
for( col in cols_to_rank) {
  if(  dataset[ col > 0, .N ]  > 0 ) {
    print(col)
    pos_neg_cols <- c(pos_neg_cols, col)
    dataset[   , paste0( col, "_neg" ) := ifelse( col< 0, col, 0 ) ]
    dataset[   , paste0( col, "_pos" ) := ifelse( col> 0, col, 0 ) ]
    dataset[, get(col):=NULL]
  }
}

dataset$mcomisiones

#hago el rankeo
for (col in cols_to_rank) {
  dataset[ foto_mes==202103  , paste(col,"rank", sep = "_")  :=  (frank(-dataset[ foto_mes==202103 , get(col) ], ties.method="min", na.last="keep" )-1)/(nrow(dataset[ foto_mes==202103 , ])-1)]
  dataset[ foto_mes==202105  , paste(col,"rank", sep = "_")  :=  (frank(dataset[ foto_mes==202105 , get(col) ], ties.method="min", na.last="keep" )-1)/(nrow(dataset[ foto_mes==202105 , ])-1)]
  dataset[, get(col):=NULL]
}

ncol(dataset)
dataset$mcuenta_corriente

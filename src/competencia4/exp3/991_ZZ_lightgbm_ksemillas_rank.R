#Necesita para correr en Google Cloud
# 128 GB de memoria RAM
# 256 GB de espacio en el disco local
#   8 vCPU


#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")

require("lightgbm")
require("primes")

#Parametros del script
PARAM <- list()
PARAM$experimento <- "ZZ9912_exp3"
PARAM$exp_input <- "HT9410_exp3"


PARAM$modelo <- 1 # se usa el mejor de la OB, pero a futuro podria variar esto
PARAM$semilla_primos <- 239069
PARAM$semillerio <- 100 # ¿De cuanto será nuestro semillerio?
PARAM$indice_inicio_semilla <- 1
PARAM$indice_fin_semilla <- 60
# FIN Parametros del script

# genero un vector de una cantidad de PARAM$semillerio  de semillas,  buscando numeros primos al azar
primos <- generate_primes(min = 100000, max = 1000000) # genero TODOS los numeros primos entre 100k y 1M
set.seed(PARAM$semilla_primos) # seteo la semilla que controla al sample de los primos
ksemillas <- sample(primos)[1:PARAM$semillerio] # me quedo con  PARAM$semillerio primos al azar
ksemillas_used <- ksemillas[PARAM$indice_inicio_semilla:PARAM$indice_fin_semilla]
count <- length(ksemillas_used)
#count <- (length(ksemillas_used) - 50)
#------------------------------------------------------------------------------
options(error = function() { 
  traceback(20); 
  options(error = NULL); 
  stop("exiting after script error") 
})
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui empieza el programa
base_dir <- "~/buckets/b1/"

#creo la carpeta donde va el experimento
dir.create( paste0( base_dir, "exp/", PARAM$experimento, "/"), showWarnings = FALSE )
setwd(paste0( base_dir, "exp/", PARAM$experimento, "/"))   #Establezco el Working Directory DEL EXPERIMENTO

#leo la salida de la optimizaciob bayesiana
arch_log  <- paste0( base_dir, "exp/", PARAM$exp_input, "/BO_log.txt" )
tb_log  <- fread( arch_log )
setorder( tb_log, -ganancia )
IB <- tb_log[PARAM$modelo]$iteracion_bayesiana
cat(IB,file="Iteración_bayesiana.txt") #Guardo el rank de la iter Bayesiana usada

#leo el nombre del expermento de la Training Strategy
arch_TS  <- paste0( base_dir, "exp/", PARAM$exp_input, "/TrainingStrategy.txt" )
TS  <- readLines( arch_TS, warn=FALSE )

#leo el dataset donde voy a entrenar el modelo final
arch_dataset  <- paste0( base_dir, "exp/", TS, "/dataset_train_final.csv.gz" )
dataset  <- fread( arch_dataset )

#leo el dataset donde voy a aplicar el modelo final
arch_future  <- paste0( base_dir, "exp/", TS, "/dataset_future.csv.gz" )
dfuture <- fread( arch_future )


#defino la clase binaria
dataset[ , clase01 := ifelse( clase_ternaria %in% c("BAJA+1","BAJA+2"), 1, 0 )  ]

campos_buenos  <- setdiff( colnames(dataset), c( "clase_ternaria", "clase01") )

# Guardo las semillas Y EL ORDEN en que son usadas
write.csv(ksemillas_used, file = "ksemillas.csv", row.names = FALSE)

#genero un modelo para cada uno de las modelos_qty MEJORES iteraciones de la Bayesian Optimization
for( ksemilla in ksemillas[PARAM$indice_inicio_semilla:PARAM$indice_fin_semilla] )
{
  
  # optimización: si los archivos ya existen, puedo hacer skip de esta semilla
  
  nom_resultados <- paste0(
    PARAM$experimento,
    "_",
    sprintf("%d", ksemilla),
    "_resultados.csv"
  )
  
  # Salteo las semillas ya procesadas
  if ( file.exists(nom_resultados)) {
    next # si, podría ser mas sofisticado, pero queda para el refactor
  }
  
  message("procesando semilla ", ksemilla)# un poco de debug
  message("Faltan ", count)
  timestamp()
  parametros <- as.list(copy(tb_log[PARAM$modelo]))
  iteracion_bayesiana  <- parametros$iteracion_bayesiana
  
  message("Creando dataset ")
  
  #creo CADA VEZ el dataset de lightgbm
  dtrain  <- lgb.Dataset( data=    data.matrix( dataset[ , campos_buenos, with=FALSE] ),
                          label=   dataset[ , clase01],
                          weight=  dataset[ , ifelse( clase_ternaria %in% c("BAJA+2"), 1.0000001, 1.0)],
                          free_raw_data= FALSE
  )
  
  
  #elimino los parametros que no son de lightgbm
  parametros$experimento  <- NULL
  parametros$cols         <- NULL
  parametros$rows         <- NULL
  parametros$fecha        <- NULL
  parametros$prob_corte   <- NULL
  parametros$estimulos    <- NULL
  parametros$ganancia     <- NULL
  parametros$iteracion_bayesiana  <- NULL
  
  #Utilizo la semilla definida en este script
  parametros$seed  <- ksemilla
  
  #genero el modelo entrenando en los datos finales
  set.seed( parametros$seed )
  message("Entrenando el final model")
  
  modelo_final  <- lightgbm( data= dtrain,
                             param=  parametros,
                             verbose= -100 )
  
  message("Prediciendo")
  
  #genero la prediccion, Scoring
  prediccion  <- predict( modelo_final,
                          data.matrix( dfuture[ , campos_buenos, with=FALSE ] ) )
  timestamp()
  
  tb_prediccion  <- dfuture[  , list( numero_de_cliente, foto_mes ) ]
  tb_prediccion[ , prob := prediccion ]
  #hago el rank de las probabilidades
  tb_prediccion[, rank := frank(-prob, ties.method = "random")]
  tb_prediccion_rank <- data.table(tb_prediccion[, list(numero_de_cliente, foto_mes, rank)])
  colnames(tb_prediccion_rank) <- c("numero_de_cliente", "foto_mes", "prediccion")
  
  #guardo los resultados de la predicción, por cada registro su probabilidad y ranking
  fwrite(tb_prediccion[, list(numero_de_cliente, prob, rank)],
         file = nom_resultados,
         sep = ",")
  
  count <- count - 1
  #borro y limpio la memoria para la vuelta siguiente del for
  rm( tb_prediccion )
  rm( modelo_final)
  rm( parametros )
  rm( dtrain )
  gc()
}

# Esta funcion crea el bash para correr en la consola de linux cada corrida de CPT
# x_predictors = predictores de la TSM generados con el codigo de las areas predictoras (ruta)
# y_predicting = archivos de cada departamento (ruta)
# dir_output = directorio de salida donde se guardan los resultados 
# modes = archivo de configuraciones, en las filas se tinen: 
# x_modes, y_modes, cca_modes, trasformation (corregir esto si esta mal)
run_cpt<-function (x_predictors, y_predicting, modes, dir_output){
# cmd corresponde al bash que se guarda automaticamente con los outputs  
  cmd <- "@echo off

  (
  echo 611
  echo 545
  echo 1
  echo %X_PREDICTORS%
  echo 1
  echo %X_Modes%
  echo 2
  echo %Y_PREDICTING%
  echo 1
  echo %Y_Modes%
  echo 1
  echo %CCA_Modes%
  echo 9
  echo 1
  echo 554
  echo 2
  echo %Transformation%
  echo 522
  echo 2
  echo 112
  echo %RESULT_GI%
  echo 311
  echo 451
  echo 452
  echo 454
  echo 455
  echo 413
  echo 1
  echo %Pearson%
  echo 413
  echo 3
  echo %2AFC%
  echo 111
  echo 401
  echo %Canonical%
  echo 501
  echo %RESULT_PROB%
  echo 0
  echo 0
  ) | CPT_batch.exe"
  
  dep <- strsplit(tail(strsplit(y_predicting, "/")[[1]], 1), ".txt")[[1]]
  trim <- strsplit(tail(strsplit(x_predictors, "/")[[1]], 1), ".txt")[[1]]
  
  modos <- read.csv(file = modes, header =TRUE, row.names = 1)
  
  cmd<-gsub("%X_PREDICTORS%",x_predictors,cmd)
  cmd<-gsub("%X_Modes%",modos["x_modes",] ,cmd)
  cmd<-gsub("%Y_PREDICTING%",y_predicting,cmd)
  cmd<-gsub("%Y_Modes%",modos["y_modes",],cmd)
  cmd<-gsub("%CCA_Modes%",modos["cca_modes", ],cmd)
  cmd<-gsub("%RESULT_GI%",paste0(dir_output,"/GoodnessIndex_",dep, "_", trim,".txt"),cmd)
  cmd<-gsub("%Pearson%",paste0(dir_output,"/Pearsons_correlation_",dep, "_", trim,".txt"),cmd)
  cmd<-gsub("%2AFC%",paste0(dir_output,"/k_2AFC_Score_",dep, "_", trim,".txt"),cmd)
  cmd<-gsub("%Canonical%",paste0(dir_output,"/CanonicalCorrelations_",dep, "_", trim,".txt"),cmd)
  cmd<-gsub("%RESULT_PROB%",paste0(dir_output,"/ForecastProbabilities_",dep, "_", trim,".txt"),cmd)

  
  if(modos["trasformation",] == "no"){
    cmd<-gsub("%Transformation%"," ",cmd)} else if(modos["trasformation",] != "no"){
      cmd<-gsub("%Transformation%","541",cmd)}
  
  write(cmd,paste0(dir.output.G, "text.bat"))
  system(paste0(dir.output.G, "text.bat"), ignore.stdout = T, show.output.on.console = F)
  
  if(file.exists(paths = paste0(dir.output.G, "text.bat")) == TRUE){
    file.remove(paste0(dir.output.G, "text.bat"))
  }
  
}

# root - directorio donde se guardan los archivos de la seleccion del area predictora
root <- "C:/Users/AESQUIVEL/Desktop/selection_predictor_area"  # areas de Diego
# nombre de los departamentos sacados de las carpetas
Locations <- list.dirs(root, full.names = FALSE,  recursive = FALSE)


# directorio donde se guardan todas las salidas 
dir.output.G <-  "C:/Users/AESQUIVEL/Desktop/outputs/"

# Conf: direccion de los archivos de configuraciones del modelo  -  estas las entrega stiven
dir.Conf <- "C:/Users/AESQUIVEL/Desktop/conf/"


# Esta funcion corre cpt y guarda los archivos para todos los trimestres en un departamento
run_all_locations<- function(x, dir.output.G, dir.Conf){
  # cree un directorio con el nombre del departamento en caso de ser necesario
  if(dir.exists(paths = paste0(dir.output.G, x)) == FALSE){
    dir.create(path = paste0(dir.output.G, x))
  }
  
  # y: direccion del archivo de estaciones
  y_predicting <- paste0( root, "/", x ,"/precip/", list.files(path = paste0(root, "/", x ,"/precip/"), pattern = ".txt"))
  # x: direccion de los archivos de la tsm
  x_predictors <- paste0( root, "/", x ,"/tsm/", list.files(path = paste0(root, "/", x, "/", "tsm"), pattern = ".txt"))
  
  # Conf: direccion de los archivos de configuraciones del modelo 
  Conf<- paste0(dir.Conf, x, "/", list.files(paste0(dir.Conf, x, "/")))
  
  # directorio donde se guardara toda las corridas del departamento
  dir_output <- paste0(dir.output.G, x)
  
  # Corre cpt y guarda los archivos para todos los trimestres de un departamento
  mapply(run_cpt, x_predictors, y_predicting, Conf, dir_output, SIMPLIFY = FALSE)
}
# Corre cpt y guarda los archivos para todos departamentos
mapply(run_all_locations, Locations, dir.output.G, dir.Conf, SIMPLIFY = FALSE)
gc(reset = TRUE)

# Ejemplo corrida individual
#run_cpt(x_predictors = "C:/Users/AESQUIVEL/Desktop/selection_predictor_area/casanare/tsm/s1.txt", 
#        y_predicting = "C:/Users/AESQUIVEL/Desktop/selection_predictor_area/cordoba/precip/precip_cordoba.txt",
#        modes = "C:/Users/AESQUIVEL/Desktop/conf/cordoba/cordoba_s1.csv",
#        dir_output = "C:/Users/AESQUIVEL/Desktop/outputs")
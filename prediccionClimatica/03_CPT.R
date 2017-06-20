# Esta funcion crea el bash para correr en la consola de linux cada corrida de CPT
# x_predictors = predictores de la TSM generados con el codigo de las areas predictoras
# y_predicting = archivos de cada departamento
# dir_output = directorio de salida donde se guardan los resultados 
# modes = archivo de configuraciones 
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
  echo 90
  echo -90
  echo 0
  echo 359
  echo 1
  echo %Y_Modes%
  echo 1
  echo %CCA_Modes%
  echo 9
  echo 1
  echo 554
  echo 2
  echo %Transformation%
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
  
  write(cmd,"C:/Users/AESQUIVEL/Desktop/outputs/text.bat")
  system("C:/Users/AESQUIVEL/Desktop/outputs/text.bat", ignore.stdout = T, show.output.on.console = F)
}





#### Leer el archivo de configuracines para extraer los modos de x, y, cca y si se transforma o no



# directorio donde se guardan los archivos de la seleccion del area predictora
root <- "C:/Users/AESQUIVEL/Desktop/selection_predictor_area" 
Locations <- list.dirs(root, full.names = FALSE,  recursive = FALSE)


# La idea es hacer aqui un lapply por departamento 

function(x){
  
  # Completo
  y_predicting <- paste0( root, "/", x ,"/precip/", list.files(path = paste0(root, "/", x ,"/precip/"), pattern = ".txt"))
  # x hace referencia al departamento 
  x_predictors <- paste0( root, "/", x ,"/tsm/", list.files(path = paste0(root, "/", x, "/", "tsm"), pattern = ".txt"))
  
  Conf<- paste0("C:/Users/AESQUIVEL/Desktop/conf/", x, "/", list.files(paste0("C:/Users/AESQUIVEL/Desktop/conf/", x, "/")))
  
  
  
  run_cpt(x_predictors = "C:/Users/AESQUIVEL/Desktop/selection_predictor_area/casanare/tsm/s1.txt", 
          y_predicting = "C:/Users/AESQUIVEL/Desktop/selection_predictor_area/cordoba/precip/precip_cordoba.txt",
          modes = "C:/Users/AESQUIVEL/Desktop/conf/cordoba/cordoba_s1.csv",
          dir_output = "C:/Users/AESQUIVEL/Desktop/outputs")

  
  
  mapply(run_cpt, x_predictors, y_predicting, Conf, dir_output, SIMPLIFY = FALSE)
  
}











run_cpt(x_predictors = "C:/Users/AESQUIVEL/Desktop/selection_predictor_area/casanare/tsm/s1.txt", 
        y_predicting = "C:/Users/AESQUIVEL/Desktop/selection_predictor_area/cordoba/precip/precip_cordoba.txt",
        modes = "C:/Users/AESQUIVEL/Desktop/conf/cordoba/cordoba_s1.csv",
        dir_output = "C:/Users/AESQUIVEL/Desktop/outputs")
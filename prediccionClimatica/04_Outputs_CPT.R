# directorio donde se guardan todas las salidas del programa CPT
dir.output.G <-  "C:/Users/AESQUIVEL/Desktop/outputs/"

# directorio donde se guardan los outputs de CPT en R, es decir metricas y probabilidades
dir.output.cptR <- "C:/Users/AESQUIVEL/Desktop/outputs_CPT/"


# nombre de los departamentos sacado de los nombres de las carpetas
Locations <- list.dirs(dir.output.G, full.names = FALSE,  recursive = FALSE)

# Esta funcion permite organizar una lista con la informacion de una sola corrida de CPT.
# los parametros se leen dentro de otra funcion 
table_Ind<-function(x, gFiles, Pfiles, Kfiles, Can.files, prob.files){
  
  # Para un archivo individual 
  # lectura de archivos
  goodnex<-read.table(gFiles,  skip=6, colClasses=c(rep("numeric",3),"character",rep("numeric",4)))
  goodnex <- max(goodnex[,ncol(goodnex)]) #seleccion del valor
  Pearson<- read.table(Pfiles, sep="",  skip=3,colClasses=c("character","numeric"), col.names = c("id", "Pearson"))
  kendall<-read.table(Kfiles, sep="",  skip=3,colClasses=c("character","numeric"), col.names = c("id", "kendall"))
  canonical<-read.table(Can.files,  skip=3, nrow=2)[,2] # solo lee las dos primeras correlaciones canonicas
  
  w1<-read.table(prob.files,  sep="", skip =3, nrow=2,  fill=TRUE, check.names = FALSE)[-2,]
  w2<-read.table(prob.files,  sep="", skip =6, nrow=1, fill=TRUE, header = TRUE)
  w3<-read.table(prob.files,  sep="", skip =9, nrow=2, fill=TRUE)
  
  
  # Se obtiene el numero del mes central del trimestre apartir de la primera fila
  # del archivo de probabilidades
  month <- as.numeric(strsplit(strsplit(row.names(w1), "/")[[1]][1],"-")[[1]][2]) + 1
  if(month == 13){ month <- 1}
  # De igual forma sucede con el año del pronostico se selecciona del archivo de prob
  year <- as.numeric(strsplit(row.names(w1), "-")[[1]][1])
  if(month==1){ year <- year + 1}
  
  # se asigna un nombre la las filas de los objetos de probabilidades
  rownames(w1) <- "below" ; rownames(w2) <- "normal";  rownames(w3) <- "Above" 
  
  #### creacion de la tabla de probabilidades para una sola corrida
  forecast<- data.frame(year= year  , month = month,  
                        id = names(w1), 
                        cbind.data.frame(below = as.numeric(t(w1)), t(w2), t(w3), row.names=NULL))
  # Creacion del data.frame que incluye todas las metricas (2 correlaciones canonicas)
  Ind<-merge(Pearson,  kendall, all=TRUE)
  Ind<-cbind.data.frame(year= year  , month = month, Ind, goodnex,  corCanonica = t(canonical), row.names = NULL)
  # devuelve una lista con dos data frame (uno de probabilidades y otro de metricas)
  gc(reset = T) 
  return(list(forecast = forecast, metrics = Ind ))}


### Esta funcion guarda un archivo con todas 
### las metricas de las corridas para todos los trimestres
metrics_and_forecast <- function(x, dir.output.G, dir.output.cptR){
  
  # rutas donde se encuentran outputs de CPT 
  gFiles <- list.files(path = paste0(dir.output.G, x), pattern = "GoodnessIndex", full.names = TRUE)
  Pfiles <- list.files(path = paste0(dir.output.G,x), pattern = "Pearsons_correlation", full.names = TRUE)
  Kfiles <- list.files(path = paste0(dir.output.G,x), pattern = "k_2AFC_Score_", full.names = TRUE)
  Can.files<-list.files(path = paste0(dir.output.G,x), pattern= "CanonicalCorrelations", full.names = TRUE)
  prob.files<- list.files(path = paste0(dir.output.G,x), pattern = "ForecastProbabilities", full.names = TRUE)
  
  # cree un directorio para el departamento en caso de necesitarlo. 
  if(dir.exists(paths = paste0(dir.output.cptR, x)) == FALSE){
    dir.create(path = paste0(dir.output.cptR, x))
  }
  # para todas las corridas del departamento crea una lista que luego divide en dos tablas y las guarda como .csv
  summary_cpt<-mapply(table_Ind, x, gFiles, Pfiles, Kfiles, Can.files, prob.files, SIMPLIFY = FALSE); names(summary_cpt) <- NULL
  forecast<-do.call("rbind", lapply(summary_cpt, function(i){i$forecast}) ) 
  write.csv(x = forecast,row.names = FALSE, file =  paste0(dir.output.cptR, x, "/", "forecast.csv" ))
  metrics_cpt<-do.call("rbind", lapply(summary_cpt, function(i){i$metrics}) ) 
  write.csv(x = metrics_cpt,row.names = FALSE, file =  paste0(dir.output.cptR, x, "/", "metrics.csv" ))
  
}
# Corra para todos los departamentos y guarde los archivos. 
lapply(X = Locations, FUN = metrics_and_forecast, dir.output.G, dir.output.cptR)

# borre todos los archivos que genera el Cpt.bash
unlink(paste0(dir.output.G,"*"), recursive = TRUE)




#Cargar Librerias

library(foreach)
library(doSNOW)

cpus <- 2  ## Number the trheads in your computer
#
cl <- makeCluster(cpus)   
#
registerDoSNOW(cl)  ## For Windows

data=("C:/ORYZA/AgroClim2016/res.dat")

#oryza.dat <- function(data, ...){

dat_file <- readLines(data)
pos_head <- grep("TIME", dat_file)
imp.head <- scan(data, what = "character", skip = pos_head [1] -1, nlines = 1, quiet = T) 
path <- getwd()


length.info <- function(data, path){
  
  k <- 0
  
  test <- strsplit(dat_file[data + 2],split = path)
  while(!is.na(length(test)==1 && test[[1]]!="")){
    test <- strsplit(dat_file[data + k],split = path)
    k<- k+1
    
  }  
  
  return(k)
}


n_rows <- foreach(i = 1:length(pos_head), .combine = 'c') %do% {
  
  
  length.info(pos_head[i], path)
  
}

read.dat <- function(datos, i, ...){
  
  info <- read.table(data, ...)
  info <- data.frame(identifier = rep(i, dim(info)[1]), info)
  return(info)
  
}



# oryza_daily <- lapply(1:length(pos_head), function(i) read.dat(data, sep = "\t", header = F, skip = pos_head[i] + 1, nrows = n_rows[i] - 3, na.strings="-", colClasses = "numeric", i = i))




oryza_daily <- foreach(i = 1:length(pos_head)) %dopar% {
  
  
  read.dat(data, sep = "\t", header = F, skip = pos_head[i] + 1, nrows = n_rows[i] - 3, na.strings="-", 
           colClasses = "numeric", i = i)
  
}


headers <- function(datos){
  
  colnames(datos) <- c("identifier", imp.head)
  return(datos)
  
}

oryza_daily <- lapply(oryza_daily, headers)
##oryza_daily <- do.call("rbind", oryza_daily)

#return(oryza_daily)


#}
############################################
############################################

#lista_archivos=oryza.dat("C:/Users/CBARRIOS/Desktop/Prueba_Leer_Res/Primer_Semestre/FED2000/res_FED2000_Sem_1_1_5000.dat")
### Crea Graficas RES

####Seleccione las variables a analizar
names(oryza_daily[[1]])
oryza2<-lapply(oryza_daily,"[",,c(1,2,3,4,9,11,12,13,14,27))

###########################################
##year=c((rep(mean(oryza2[[1]]$YEAR[oryza2[[1]]$DVS<=0.65],na.rm = T),3)))

#yield= c((rep(max((oryza2[[1]]$WRR14), na.rm = T),3)))


res=function(x){
  
sday=c((sum(x$DVS<=0.65,na.rm = T)),(sum(x$DVS>0.65 & x$DVS<=1,na.rm = T )),(sum(x$DVS>1,na.rm = T)))
rain=c(sum(x$RAIN[x$DVS<=0.65],na.rm = T),sum(x$RAIN[x$DVS>0.65 & x$DVS<=1],na.rm = T),sum(x$RAIN[x$DVS>1],na.rm = T))
rdd=c(sum(x$RDD[x$DVS<=0.65],na.rm = T),sum(x$RDD[x$DVS>0.65 & x$DVS<=1],na.rm = T),sum(x$RDD[x$DVS>1],na.rm = T))
tmin=c(mean(x$TMIN[x$DVS<=0.65],na.rm = T),mean(x$TMIN[x$DVS>0.65 & x$DVS<=1],na.rm = T),mean(x$TMIN[x$DVS>1],na.rm = T))
tmax=c(mean(x$TMAX[x$DVS<=0.65],na.rm = T),mean(x$TMAX[x$DVS>0.65 & x$DVS<=1],na.rm = T),mean(x$TMAX[x$DVS>1],na.rm = T))
yield= c((rep(max((x$WRR14), na.rm = T),3)))
year= c((rep(mean(x$YEAR[x$DVS<=0.65],na.rm = T),3)))
ID= c((rep(mean(x$identifier[x$DVS<=0.65],na.rm = T), 3)))
final= rbind(ID,year,sday,rain,rdd,tmin,tmax,yield)

return(final)

}


resultados=lapply(oryza2,res)
resultados=docall(rbind,resultados)
rownames(resultados)=paste(rownames(resultados),"sim",sort(rep(1:length(oryza2),8)),sep="_")
colnames(resultados)=c("Vegetativa","Reproductiva","Madurez")


grep("rain",rownames(resultados))

sapita=resultados[grep("rain",rownames(resultados)),]

apply(sapita,2,mean)


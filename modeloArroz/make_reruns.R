## Funciones para generar el reruns de ORYZAv3

############################################################
########## Funcion para generar tablas de fechas ###########
############################################################


# ydays  TRUE or FALSE
# xdate inicio de fecha de simulacion (año-mes-dia)
# ydate final de fecha de simulacion (año-mes-dia) o se puede ingresar el numero de dias
# y_days si ingresa cuantos dias deseas simular
# by_days saltos de cada cuantos dias se desea simular

# xdate <- '2016-08-20' 
# ydate <- '2017-01-15'
# y_days <- FALSE
# by_days <- 5


## Segunda prueba
# ydate <- 45
# y_days <- TRUE


# Proof
# date_reruns(xdate, ydate, y_days, by_days)

date_reruns <- function(xdate, ydate, y_days = FALSE, by_days){
  
  require(lubridate)
  require(tidyverse)
  
  
  initial <- ymd(xdate)
  
  if(y_days == TRUE){
    
    final <- initial + ddays(ydate)
    
  } else{
    
    final <- ymd(ydate)
    
  }
  
  by_days <- 5  
  
  seq_date <- seq(initial,
                  final, by = by_days)
  
  dates_df <- tbl_df(data.frame(year = year(seq_date), day = yday(seq_date)))
  return(dates_df)
  
}


############################################################
########## Funcion para generar todas las combinaciones ####
############################################################ 

# FILEIT = 'DOCO.exp'                                         # Archivo(s) Experimental
# FILEI2 = 'MRCO.sol'	                                        # Archivo(s) Suelo
# FILEI1 = c('F733.crp', 'F2000.crp') 			                  # Archivo(s) de Cultivo
# CNTR = 'DOCO'								                                # Nombre(s) de la estacion
# n_scenarios = 99		                                        # Numero de simulaciones



## Proof
# date_reruns_df <- date_reruns(xdate, ydate, y_days, by_days)
# combinations_df <- combinations(date_reruns_df, n_scenarios, FILEIT, FILEI1, FILEI2, CNTR)


combinations <- function(date_reruns_df, n_scenarios, FILEIT, FILEI1, FILEI2, CNTR){
  
  require(lubridate)
  require(tidyverse)
  
  ISTN <- 1:n_scenarios
  
  all_combination_list <- crossing(date_reruns_df, ISTN, FILEIT, FILEI1, FILEI2, CNTR)
  return(all_combination_list)
  
}


############################################################
########## Funcion para generar el reruns Oryza v3 #########
############################################################ 

# combinations_df
# file_name <- 'DOCO_2016b.rer'

# Proof

# make_reruns(combinations_df, file_name)

make_reruns <- function(data, file_name){
  
  if(file.exists(file_name)){
    
    file.remove(file_name)
    
  } 
  
  sink(file = file_name, append = T)
  for(i in 1:nrow(data)){
    
    
    cat('********************', sep = '\n')
    cat(paste("FILEIT = ", "'",  data[i, 'FILEIT'], "'", sep = ""), sep = '\n')
    cat(paste("FILEI2 = ", "'", data[i, 'FILEI2'], "'", sep = ""), sep = '\n')
    cat(paste("FILEI1 = ", "'", data[i, 'FILEI1'], "'", sep = ""), sep = '\n')
    cat(paste("CNTR = ",   "'", data[i, 'CNTR'], "'", sep = ""), sep = '\n')
    cat(paste("ISTN = ", data[i, 'ISTN'], sep = ""), sep = '\n')
    cat(paste("IYEAR =  ",  data[i, 'year'], sep = ""), sep = '\n')
    cat(paste("STTIME =  ",  data[i, 'day'], ".", sep = ""), sep = '\n')
    cat(paste("EMD =  ", data[i, 'day'], sep = ""), sep = '\n')
    cat(paste("EMYR = ", data[i, 'year'], sep = ""), sep = '\n')
    
    
    
  }
  
  sink()
  
}








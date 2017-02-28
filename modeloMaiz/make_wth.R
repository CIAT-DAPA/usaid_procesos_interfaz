## Make .WTH
# Function to write .WTH, needs dataset path, out dir, lat, long and name of WTH
# the climate data example is:
# data frame that contain 
# day month  year     tmax  tmin precip     srad   (name columns data frame)

# path_functions <- 'D:/CIAT/USAID/DSSAT/multiple_runs/R-DSSATv4.6/'
# source(paste0(path_functions, 'main_functions.R'))

make_wth <- function(data, out_dir, lat, long, name_xfile_climate){
  
  Srad <- data$sol_rad
  Tmax <- data$t_max
  Tmin <- data$t_min
  Prec <- data$prec
  date <- data$date_dssat 
  
  sink(paste0(out_dir, name_xfile_climate, '.WTH'), append = F)
  ## Agregar las siguientes Lineas
  
  ##cat(paste("*WEATHER DATA :"),paste(coordenadas[1,1]),paste(coordenadas[1,2]))
  cat(paste("*WEATHER DATA :"), paste("USAID"))
  cat("\n")
  cat("\n")
  cat(c("@ INSI      LAT     LONG  ELEV   TAV   AMP REFHT WNDHT"))
  cat("\n")
  cat(sprintf("%6s %8.3f %8.3f %5.0f %5.1f %5.1f %5.2f %5.2f", "USCI", lat, long, -99,-99, -99.0, 0, 0))
  cat("\n")
  cat(c('@DATE  SRAD  TMAX  TMIN  RAIN'))
  cat("\n")
  cat(cbind(sprintf("%5s %5.1f %5.1f %5.1f %5.1f", date, Srad, Tmax, Tmin, Prec)), sep = "\n")
  sink()
  
  
}

# is necessary to add lat and long? change te results? 
# out_dir <- 'D:/CIAT/USAID/DSSAT/multiple_runs/R-DSSATv4.6/Proof_run/'
# dir_climate <- 'D:/CIAT/USAID/DSSAT/multiple_runs/R-DSSATv4.6/stations/Forecasts/Escenarios/'
# x <- load_climate(dir_climate)
# make_wth(x[[1]], out_dir, -99, -99, name_xfile_climate = 'USAID001')


























# maybe to the present simulation to calibrate the model 
# you need to have always the column called year because this is the var to change to actual date 
# date is get to ask server for the local date, remeber this depend of the forecasts year to simulate
# the data frame always needs to be
# day month  year     tmax  tmin precip     srad   (name columns data frame)



##  generate all .WTH files to run DSSAT 
## Proof
# data <- climate_list_df[[1]]
# out_dir <- 'D:/CIAT/USAID/DSSAT/multiple_runs/R-DSSATv4.6/Proof_run/'
# lat <- -99
# long <- -99

# climate_df <- read_csv(paste0(path, station, '.csv'))%>%
#   mutate(date = dmy(paste(day, month, year, sep = '/'))) %>%
#   mutate(julian_day = yday(date), year_2 = substr(year, 3, 4))  


# date_dssat <- 0      
# 
# for(i in 1:dim(climate_df)[1]){
#   
#   date_dssat[i] <- date_for_dssat(climate_df$year_2[i], climate_df$julian_day[i])
#   
# }
# 
# climate_df$date_dssat <- date_dssat


## Es necesario tener en cuenta que siempre las variables deben tener el mismo id
##  ejemplo
## Srad = srad
## Tmax = tmax
## Tmin = tmin
## Prec = precip
## Date = day_dssat   # fecha para la construccion del Julian day necesario para que DSSAT entienda la informacion climatica


# climate_list_df[[1]] %>%
  # mutate(day_dssat = frcast_date)

# climate_list_df <- lapply(climate_list_df, mutate, date_dssat = frcast_date)

## make a function to do this


### quitar del make_wth la extension de .WTH (la idea es que luego se pueda utilizar con .WTG o lo que sea)














### Codes to generate .WTH for historical climate data set and scenarios data set 


# library(tidyverse)
# library(lubridate)



# soure(main_functions)


## proof


# climate_df <- read_csv(paste0(path, station, '.csv'))[10, ]
# date_for_dssat(substr(climate_df$year, 3, 4), yday(dmy(paste(climate_df$day, climate_df$month, year, sep = '/'))))
# date_for_dssat(80, 10)

## Codigo quiza cambiar a tidyverse 
# library(dplyr)
# library(readr)




# library(tidyverse)
# library(lubridate)


# path <- 'D:/CIAT/USAID/DSSAT/multiple_runs/R-DSSATv4.6/stations/'
# station <- 'LaUnion'

# name_xfile_climate <- 'CCCR8000'  ## se debe indicar tal cual como en el WSTA dentro del archivo x-file

## Luego preguntar cuales son las benditas coordenadas
# substr(date, 9, 10)
# lat <- -99  
# long <- -99

## Las fechas relatan desde 1 Enero 1980 hasta 31 Dicimebre 2014 


# 
# climate_df <- read_csv(paste0(path, station, '.csv'))%>%
#                 mutate(date = dmy(paste(day, month, year, sep = '/'))) %>%
#                 mutate(julian_day = yday(date), year_2 = substr(year, 3, 4))  
# date_dssat <- 0             
# for(i in 1:dim(climate_df)[1]){
#   
#   date_dssat[i] <- date_for_dssat(climate_df$year_2[i], climate_df$julian_day[i])
#   
# }
# 
# climate_df$date_dssat <- date_dssat


# mutate(date_dssat = date_for_dssat(substr(year, 3, 4), yday(dmy(paste(day, month, year, sep = '/')))))


# out_dir <- 'D:/CIAT/USAID/DSSAT/multiple_runs/R-DSSATv4.6/Runs/'

## Es necesario tener en cuenta que siempre las variables deben tener el mismo id
##  ejemplo
## Srad = srad
## Tmax = tmax
## Tmin = tmin
## Prec = precip
## Date = day_dssat   # fecha para la construccion del Julian day necesario para que DSSAT entienda la informacion climatica


## El codigo esta construido para que funciones para las variables anteriormente mencionadas


## Proof
# data <- climate_df  
# out_dir <- 'D:/CIAT/USAID/DSSAT/multiple_runs/R-DSSATv4.6/Runs/'
# lat <- -99
# long <- -99







### Climate Scenarios functions - ORYZA linux --->> 
# Author: Rodriguez-Espinoza J., Mesa J., 
# https://github.com/jrodriguez88/
# 2022




# Lee archivos climaticos de la capeta de escenarios climaticos de remuestreo
load_all_climate <- function(dir_climate, dir_inputs_setup){
  
#  require(tidyverse)
#  require(lubridate)
climate_files <- list.files(dir_climate, full.names = T)  
  
# crea lista de datos de escenarios climaticos  
 data_list <- map(climate_files, read_csv, show_col_types =F)

#Checks if the current setup have plating_window.csv to insert the observed data into the climate scenaries
dir_current_setup_planting_window <- paste0(dir_inputs_setup, "planting_window.csv")
if(file.exists(dir_current_setup_planting_window)){
  data_list <- addObsDataToPlanting(path_output_observed_data,id_station,data_list,dir_current_setup_planting_window, dir_stations)
}

 #Add filter to delete NANA row due to february climatology   
 data_wth <- data_list %>% 
   map(~.x %>% mutate(date = make_date(year, month, day),
                      date_dssat = date_for_dssat(date)) %>% dplyr::arrange(date) %>% filter(str_detect(date_dssat, "NANA", negate=TRUE)))
    
}

    

# Crea formatos de fechas de simulacion 
make_sim_dates <- function(initial_date, planting_before, number_days, freq_sim){
  
#  require(tidyverse)
#  require(lubridate)
#  require(magrittr)
  
  
  start_date <- seq.Date(initial_date, initial_date + days(number_days), by = freq_sim)
  
  #plantig_date <- start_date + days(planting_before)
  
  

  plantig_date <- start_date + as.numeric(days_in_month(start_date))

  
  dates <- list(start_date = start_date, planting_date = plantig_date)
  
  return(dates)
  

}



## escribe archivo climatico (*.wth) en formato DSSAT
write_wth_file <- function(data, dir_run, id_name, lat, long){
  
#  require(tidyverse)
  
  
  
  srad <- data$sol_rad
  tmax <- data$t_max
  tmin <- data$t_min
  prec <- data$prec
  date <- data$date_dssat 
  
  sink(paste0(dir_run, id_name, '.WTH'), append = F)
  ## Agregar las siguientes Lineas
  
  ##cat(paste("*WEATHER DATA :"),paste(coordenadas[1,1]),paste(coordenadas[1,2]))
  cat(paste("*WEATHER DATA :"), paste("CIAT JR"))
  cat("\n")
  cat("\n")
  cat(c("@ INSI      LAT     LONG  ELEV   TAV   AMP REFHT WNDHT"))
  cat("\n")
  cat(sprintf("%6s %8.3f %8.3f %5.0f %5.1f %5.1f %5.2f %5.2f", "CIAT", lat, long, -99,-99, -99.0, 0, 0))
  cat("\n")
  cat(c('@DATE  SRAD  TMAX  TMIN  RAIN'))
  cat("\n")
  cat(cbind(sprintf("%5s %5.1f %5.1f %5.1f %5.1f", date, srad, tmax, tmin, prec)), sep = "\n")
  sink()
  
  
  
}





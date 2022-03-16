### Climate Scenarios functions - ORYZA linux --->> 
# Author: Rodriguez-Espinoza J., Mesa J., 
# https://github.com/jrodriguez88/
# 2022




# Lee archivos climaticos de la capeta de escenarios climaticos de remuestreo
load_all_climate <- function(dir_climate){
  
#  require(tidyverse)
#  require(lubridate)
  
  
# crea lista de datos de escenarios climaticos  
  load_climate <- function(dir_climate){
    

    
    climate_list <- list.files(dir_climate, full.names = T)
    
    climate_list_df <- lapply(climate_list, read_csv) 
    
    return(climate_list_df)
    
  }
  

#  
  tidy_climate_date <- function(data, scenario){
    
    options(warn = -1)

    
    current_year <- (Sys.Date() - 16) %>%
      year()  
    
    init_frcast <- ydm(paste(current_year, data$day[1], data$month[1], sep = "-"))
    end_frcast <- ymd(init_frcast) + ddays(dim(data)[1] - 1)
    
    frcast_date <- seq(init_frcast,
                       end_frcast, by = '1 day')
    
    data <-  tbl_df(data.frame(data, frcast_date)) %>%
      mutate(scenario = rep(scenario, length(day)),
             julian_day = yday(frcast_date), 
             year_2 =year(frcast_date), 
             sol_rad = sol_rad * 1000, 
             x = rep(-99 , length(day)))
    
    
    return(data)
    
  }
  
  
  

  number_scenarios <- 1:length(list.files(dir_climate))
  
  climate <- load_climate(dir_climate) %>%
    Map('tidy_climate_date', ., number_scenarios)
  
  return(climate)
  
}

# Crea formatos de fechas de simulacion 
make_sim_dates <- function(data, number_days){
  
#  require(tidyverse)
#  require(lubridate)
#  require(magrittr)
  
  
  after_days <- data[[1]] %>%
    filter( row_number() == 1:number_days) %>%
    dplyr::select(frcast_date) %>%
    mutate(pdate = frcast_date + months(1) - 16) %>%
    dplyr::select(pdate) %>%
    mutate(jpdate = yday(pdate)) %>%
    filter( row_number() == 1) %>%
    magrittr::extract2(1) 
  
  
  
  PDATE <- data[[1]] %>%
    # filter(frcast_date >= '2017-12-01')
    filter(frcast_date >= after_days + 16) %>%
    filter( row_number() == 1:number_days) %>%
    dplyr::select(julian_day) %>%
    extract2(1) %>%
    as.numeric()
  
  SDATE <- data[[1]] %>%
    dplyr::select(frcast_date, julian_day) %>%
    filter(frcast_date>= (after_days)) %>%
    filter( row_number() == 1:number_days) %>%
    dplyr::select(julian_day) %>%
    extract2(1) %>%
    as.numeric()
  
  DATE <- data[[1]] %>%
    filter( row_number() == 1:number_days) %>%
    dplyr::select(year_2) %>%
    extract2(1)
  
  IYEAR <- data[[1]] %>%
    filter(frcast_date >= after_days) %>%
    filter( row_number() == 1:number_days) %>%
    dplyr::select(year_2) %>%
    extract2(1)
  
  DATE_format <- data[[1]] %>%
    filter(frcast_date >= after_days + 16) %>%
    filter( row_number() == 1:number_days) %>%
    dplyr::select(frcast_date) %>%
    extract2(1)
  
  # dates_inputs <- crossing(PDATE, SDATE) %>%
  #   mutate(IYEAR = DATE, DATE = DATE_format)
  
  dates_inputs <- data_frame(PDATE, SDATE, IYEAR, DATE = DATE_format)
  
  return(dates_inputs)
  
}


## escribe archivo climatico (*.cli) en formato oryza
write_cli_file <- function(climate_df, filename, long, lat, elev){
  
#  require(tidyverse)
  
  climate_df <- climate_df %>%
    mutate(y = x) %>%
    dplyr::select(scenario, year_2, julian_day, sol_rad, t_min, t_max, x, y, prec)
  
  sink(file = filename, append = F)
  
  cat(paste0(long, ',', lat, ',', elev, ',', 0, ',', 0))
  cat('\n')
  write.table(climate_df, sep = ",", row.names = F, col.names = F)
  
  sink()
  
}


# escribe archivos  lista de archivos climaticos en carpeta especificada
write_weather_escenaries <- function(climate_scenarios, dir_run, long, lat, elev){
  
  wth_dir <- paste0(dir_run, 'wth', "/")
#  print(paste0("temp_dir: ", temp_dir))
  
  if(dir.exists(wth_dir)){
    
    dir.create(wth_dir)
  } else{
    
    unlink(file.path(wth_dir), recursive = T, force = T)
    dir.create(wth_dir)
  }
  

  # filename <- 'D:/CIAT/USAID/Oryza/usaid_forecast_rice/Prueba/'
  
  number_scenarios <- 1:length(climate_scenarios)
  
  names <- paste0(wth_dir, 'ciat', number_scenarios, '.cli')
  
  invisible(Map('write_cli_file', climate_scenarios, names, long, lat, elev))
  
  cat(paste("ORYZA WTH files created in ", wth_dir))
  
  
}



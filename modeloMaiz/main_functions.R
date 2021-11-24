# diferents functions to necessary to run DSSAT 
# the correct way to suppress warnings is suppressWarnings() instead of options(warn = -1) alone; similarly,
# you should use suppressMessages() to suppress messages


##############################################################################################################
#  # Functions Necessary to make WTH (.v46 file) 
##############################################################################################################

# date_for_dssat. This function generate a date necessary to make .WTH (year + julian day), only tow digits to year.

date_for_dssat <- function(year, day_year) {
  
  
  if(nchar(day_year) == 1){
    
    data <- paste0(year, '00', day_year)
  }
  
  if(nchar(day_year) == 2){
    
    data <- paste0(year, '0', day_year)
  }
  
  if(nchar(day_year) == 3){
    
    data <- paste0(year, day_year)
  }
  
  return(data)
  
}


# function to filter files
# the condition different is when you need a match with the conditions, different == T return the files that yo don't considered


filter_text <- function(data, matches, different = F){ 
  
  if(different == F){
    
    return(data[grep(matches, data)])
    
  }
  
  if(different == T){
    
    return(data[-grep(matches, data)])
    
  }
  
}


# function to make the real date for make WTH file

make_date <- function(data){
  
  # suppress warnings
  options(warn = -1)
  
  require(tidyverse)
  require(lubridate)
  
  # data <- read_csv(climate_list[[1]])
  current_year <- (Sys.Date() - 16) %>%
    year()
  
  init_frcast <- ydm(paste(current_year, data$day[1], data$month[1], sep = "-"))
  end_frcast <- ymd(init_frcast) + ddays(dim(data)[1] - 1)
  
  
  frcast_date <- seq(init_frcast,
                     end_frcast, by = '1 day')
  
  # is possible to eliminate some variables?
  
  data <-  tbl_df(data.frame(data, frcast_date)) %>%
    mutate(julian_day = yday(frcast_date),
           year_2 = as.numeric(substr(year(frcast_date), 3, 4))) %>%
    mutate(date_dssat = mapply(date_for_dssat, year_2, julian_day))
  
  return(data)
  
}



# Function to load all climate scenarios

# dir_climate <- 'D:/CIAT/USAID/DSSAT/multiple_runs/R-DSSATv4.6/stations/Forecasts/Escenarios/'
# x <- load_climate(dir_climate)
 

load_climate <- function(dir_climate){
  require(tidyverse)
  require(lubridate)
  climate_list <- list.files(dir_climate, full.names = T)
  
  ## function to extract some files that you need
  
  # omit_files <- "escenario_max.csv|escenario_min.csv|escenario_prom.csv"
  
  
  # pattern escenario It's to always filter only the climate scenarios
  # Is possible to do this into a function ?
  
  # climate_list <- list.files(dir_climate, pattern = 'escenario', full.names = T) %>%
    # filter_text(omit_files, different = T) %>%
    # .[1:99]             ## luego quitar el cargar solo las 99 veces
  
  climate_list_df <- lapply(climate_list, read_csv, col_types = cols()) %>%
    lapply(make_date)
  
  return(climate_list_df)
  
}



# function that Copy and paste files necessary to run DSSAT in a particular folder

# dir_dssat <- 'C:/DSSAT46/'  ## its necessary to have the parameters .CUL, .ECO, .SPE Updated for running (calibrated the crop (MAize))
# dir_run <- 'D:/CIAT/USAID/DSSAT/multiple_runs/R-DSSATv4.6/Proof_run/'
# dir_soil <- 'D:/CIAT/USAID/DSSAT/multiple_runs/R-DSSATv4.6/Runs/CC.SOL'  # for now
# files_dssat(dir_dssat, dir_run, dir_soil)

files_dssat <- function(dir_dssat, dir_run, dir_soil, dir_parameters){
  
  
  require(tidyverse)
  
  # files <- ".CUL|.ECO|.SPE"  ## special files
  
  CUL <- list.files(dir_parameters, full.names = TRUE) %>%
    grep("*.CUL", ., value = TRUE) 
  
  # CUL <- CUL[CUL]
    
  ECO <- list.files(dir_parameters, full.names = TRUE) %>%
      grep("*.ECO", ., value = TRUE) 
  
  SPE <- list.files(dir_parameters, full.names = TRUE) %>%
      grep("*.SPE", ., value = TRUE) 
    
  # exe_dssat <- paste0(dir_dssat, 'DSCSM046.EXE')    ## Executable DSSAT v 4.6
  
  # parameters <- dir_parameters %>%
    # list.files(full.names= T) %>%
    # filter_text(files, different = F)
  
  
  
  #file.copy(exe_dssat, dir_run)
  file.copy(CUL, dir_run)
  file.copy(ECO, dir_run)
  file.copy(SPE, dir_run)
  file.copy(dir_soil, dir_run)
  
  
}


# dir_run <- 'D:/CIAT/USAID/DSSAT/multiple_runs/R-DSSATv4.6/Proof_run/'
# execute_dssat(dir_run)
execute_dssat <- function(dir_run){
  
  setwd(dir_run)
  system(paste0("DSCSM046.EXE " , "MZCER046"," B ", "DSSBatch.v46"), ignore.stdout = T, show.output.on.console = F)
  setwd('..')
  
}


# dir_run <- 'D:/CIAT/USAID/DSSAT/multiple_runs/R-DSSATv4.6/Proof_run/'
# region <- "LaUnion" 
# day <- 1

# make_id_run(dir_run, region, day)

make_id_run <- function(dir_run, region, cultivar, day){
  
  if (!dir.exists(paste0(dir_run, region, '/', cultivar, '/', day))) { 
    
    dir.create(paste0(dir_run, region, '/', cultivar, '/', day), showWarnings = F, recursive = TRUE, mode = "777")
    # system('chmod 777 *.*')
    # paste0(dir_base, region, '/', cultivar,  '/', select_day)
    
  }
  
  return(paste0(dir_run, region, '/', cultivar, '/', day, '/'))
}


make_mult_wth <- function(scenarios, dir_run, filename, lat, long){
  
  # scenarios <- climate_scenarios
  num_scenarios <- 1:length(scenarios)
  filename <- paste0(filename, sprintf("%.3d", num_scenarios))
  mapply(make_wth, scenarios, dir_run, lat, long, filename) 

}


## Make PDATE and SDATE
# data <- climate_scenarios
# number_days <- 45
make_PS <- function(data, number_days){
  
  require(tidyverse)
  require(lubridate)
  require(magrittr)
  
  # PDATE <- data[[1]] %>%
  #   filter( row_number() == 1:number_days) %>%
  #   dplyr::select(date_dssat) %>%
  #   magrittr::extract2(1) %>%
  #   as.numeric()
  
  ## desde cuando comienza el forescast day 15 dias antes o 30 dias despues
  after_days <- data[[1]] %>%
    filter( row_number() == 1:number_days) %>%
    dplyr::select(frcast_date) %>%
    mutate(pdate = frcast_date + months(1)) %>%
    dplyr::select(pdate) %>%
    filter( row_number() == 1) %>%
    magrittr::extract2(1) 

    
  
  PDATE <- data[[1]] %>%
    filter(frcast_date >= after_days) %>%
    filter( row_number() == 1:number_days) %>%
    dplyr::select(date_dssat) %>%
    magrittr::extract2(1) %>%
    as.numeric()
  
  SDATE <- data[[1]] %>%
    dplyr::select(date_dssat, frcast_date) %>%
    filter(frcast_date >= after_days - ddays(16))  %>%
    dplyr::select(date_dssat)  %>%
    filter( row_number() == 1:number_days) %>%
    extract2(1) %>%
    as.numeric()
  
  DATE <- data[[1]] %>%
    filter(frcast_date >= after_days) %>%
    filter( row_number() == 1:number_days) %>%
    dplyr::select(frcast_date) %>%
    extract2(1)
  
  
  dates_inputs  <- data_frame(PDATE, SDATE, DATE)
  # dates_inputs <- crossing(PDATE, SDATE) %>%
  #                   mutate(DATE = DATE)
  
  return(dates_inputs)
  
}

tidy_climate <- function(dir_climate, number_days){


  climate_scenarios <- load_climate(dir_climate)
  input_dates <- make_PS(climate_scenarios, number_days)
  return(list(input_dates = input_dates, climate_scenarios = climate_scenarios))
}


# tidy_climate <- function(dir_climate, number_days){
#   
#   require(magrittr)
#   
#   climate_scenarios <- load_climate(dir_climate)
#   
#   input_dates <- make_PS(climate_scenarios, number_days)
#   
#   dates <- climate_scenarios[[1]] %>%
#               select(frcast_date) %>%
#               extract2(1)
#   
#   return(list(input_dates = input_dates, climate_scenarios = climate_scenarios, date = dates))
# }
### read output

read_summary <- function(dir_run){
  
  summary_out <- read_table(paste0(dir_run, 'summary.OUT'), skip = 3 , na = "*******", col_types = cols())
  

  return(summary_out)
}






read_weather <- function(data, skip_lines, i){
  
  require(data.table)
  require(tidyverse)
  require(lubridate)
  options(warn = -1)
  
  suppressWarnings(suppressMessages(fread(data, skip = skip_lines, stringsAsFactors = F, na.strings = "NaN", header = T, colClasses = list(
    integer = 1:3, numeric = 4:18)))) %>%
    tbl_df() %>%
    mutate_all(funs(as.numeric)) %>%
    mutate(scenario = rep(i, length(DOY)))

  
}

read_mult_weather <- function(data){
  
  require(tidyverse)
  
  data <- paste0(data, 'Weather.OUT')
  lines <- readLines(data)
  posToread <- grep("@YEAR", lines) - 1
  weather <- lapply(1:length(posToread), function(i) read_weather(data, posToread[i], i)) %>%
    bind_rows()
  
  return(weather)
  
}


mgment_no_run <- function(data){
  
  ifelse(data == -99, 0, data)
  
}




conf_lower <- function(var){
  
  t.test(var,na.rm=TRUE)$conf.int[1]
}

conf_upper <- function(var){
  
  t.test(var,na.rm=TRUE)$conf.int[2]
}


CV <- function(var){
  
  (sd(var,na.rm=TRUE)/mean(var,na.rm=TRUE))*100
  
}


calc_desc <- function(data, var){
  
  data <- dplyr::select_(data, var)
  reclas_call <- lazyeval::interp(~ mgment_no_run(var), var = as.name(var))
  
  data <- data %>%
    mutate_(.dots = setNames(list(reclas_call), var)) %>%
    summarise_each(funs(avg = mean(.,na.rm=TRUE), 
                        median = median(.,na.rm=TRUE), 
                        min = min(.,na.rm=TRUE), 
                        max = max(.,na.rm=TRUE), 
                        quar_1 = quantile(., 0.25,na.rm=TRUE), 
                        quar_2 = quantile(., 0.50,na.rm=TRUE), 
                        quar_3 = quantile(., 0.75,na.rm=TRUE), 
                        conf_lower = conf_lower(.), 
                        conf_upper = conf_upper(.), 
                        sd = sd(.,na.rm=TRUE), 
                        perc_5 = quantile(., 0.05,na.rm=TRUE),
                        perc_95 = quantile(., 0.95,na.rm=TRUE), 
                        coef_var = CV(.))) %>%
    mutate(measure = paste(var)) %>%
    dplyr::select(measure, everything())
  return(data)
}



tidy_descriptive <- function(data, W_station, soil, cultivar, start, end){
  
  require(lubridate)
  
  data <- data %>%
    mutate(weather_station = W_station,
           soil = soil, 
           cultivar = cultivar, 
           start = start, 
           end = end) %>%
    dplyr::select(weather_station, 
           soil, 
           cultivar, 
           start, 
           end, 
           everything())
  
  return(data)
  
}



run_mult_dssat <- function(dir_dssat, dir_soil, dir_run, dir_parameters, name_files, input_dates, select_day, cultivar, climate, id_soil, number_days, output, region, dir_output_maiz){
  
  
  # proof
  
  name_csv <- output$name_csv
  name_cultivar <- output$cultivar
  name_soil <- output$soil
  # number_days <- 3
  # input_dates <- climate_PS$input_dates
  # climate <- climate_PS$climate
  # id_soil <- ID_SOIL
  iterators <- rep(1:number_days, by = select_day)
  
  plan(multisession, workers = no_cores)
  out_summary <- future_lapply(iterators, function(i) {run_dssat(dir_dssat, dir_soil, dir_run, dir_parameters, name_files, input_dates, i, cultivar, climate, id_soil, name_csv, name_cultivar, name_soil, region)})
  #out_summary <- foreach(i = iterators) %do% {
    
    # print(i)
    #run_dssat(dir_dssat, dir_soil, dir_run, dir_parameters, name_files, input_dates, i, cultivar, climate, id_soil, name_csv, name_cultivar, name_soil, region)
    
    
  #}
  
  
  out_summary <- bind_rows(out_summary)
  write_csv(out_summary, paste0(dir_output_maiz, name_csv))
  return(out_summary)
}


output_names <- function(hashCrop, hashSoil, name_csv){
  
  output <- list()
  output$name_csv <- name_csv
  output$cultivar <- hashCrop 
  output$soil <- hashSoil
  
  return(output)
}


### A`nadir` por cada inputo dentro del x-file?

read_planting <- function(dir_parameters){
  
  require(tidyverse)
  
  details <- read_csv(paste0(dir_parameters, 'planting_details.csv'), col_types = cols())
  
}


### data frame to list 

frame_list <- function(data){
  
  setNames(split(data[,2], seq(nrow(data))), data[,1])
  
}

#Load coordinates from csv 
load_coordinates <- function(dir_parameters){
  
  require(readr)
  coordenadas <- read_csv(paste0(dir_parameters,'coordenadas.csv')) %>%
    as.data.frame() %>%
    frame_list()
  
}


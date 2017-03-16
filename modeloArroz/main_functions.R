

load_climate <- function(dir_climate){
  
  require(tidyverse)
  require(lubridate)
  
  climate_list <- list.files(dir_climate, full.names = T)
  
  climate_list_df <- lapply(climate_list, read_csv) 
  
  return(climate_list_df)
  
}



tidy_climate_date <- function(data, scenario){
  
  options(warn = -1)
  
  require(tidyverse)
  require(lubridate)
  
  current_year <- Sys.Date() %>%
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


load_all_climate <- function(dir_climate){
  
  require(tidyverse)
  number_scenarios <- 1:length(list.files(dir_climate))
  
  climate <- load_climate(dir_climate) %>%
    Map('tidy_climate_date', ., number_scenarios)
  
  return(climate)
  
}

## maybe filename USAID
## filename <- 'USAID'


make_mult_weather <- function(scenarios, dir_run, filename, long, lat, elev){
  
  # filename <- 'D:/CIAT/USAID/Oryza/usaid_forecast_rice/Prueba/'
  
  number_scenarios <- 1:length(scenarios)
  
  names <- paste0(dir_run, filename, number_scenarios, '.cli')
  
  invisible(Map('make_weather', scenarios, names, long, lat, elev, number_scenarios))
  
  
}


make_control <- function(out_file){
  
  # out_file <-  'D:/CIAT/USAID/Oryza/usaid_forecast_rice/Prueba/'
  
  proof <- write_head(out_file, settings_head('CONTROL', 1, 1))
  
  write_ctrl_params(proof, ctrl_params('RES.DAT', 'MODEL.LOG', 'USAID.rer'))
  write_wther(proof, ctrl_wther('USAID', 1))
  write_options_out(proof)
  
}


make_reruns <- function(data, dir_run){
  
  # data <- settins_reruns(region, CNTR, ISTN, IYEAR, STTIME, EMD, dir_run)
  
  write_reruns(data, dir_run)
  
}


## copy files necessary to run oryza

# dir_oryza <- 'C:/Program Files (x86)/ORYZA(v3)/'
# dir_run <- 'D:/CIAT/USAID/Oryza/usaid_forecast_rice/Prueba/'

# files_oryza(dir_oryza, dir_run)

files_oryza <- function(dir_oryza, dir_run){
  
  exe_oryza <- paste0(dir_oryza, 'ORYZA3.EXE')
  
  file.copy(exe_oryza, dir_run)
  
}




## add Experimental and cultivar files

# dir_files <- 'D:/CIAT/USAID/Oryza/usaid_forecast_rice/Experimental_Cultivar_Files/'
# add_exp_cul(dir_files, region, dir_run)
add_exp_cul <- function(dir_files, region, dir_run){
  
  require(stringr)
  require(tidyverse)
  
  
  
  dir_files <- list.files(dir_files, full.names = T)
  file.copy(dir_files, dir_run)
  
  pos_extract <- '.sol' %>%
    grep(dir_files)
  
  split_id_soil <- dir_files %>%
    .[pos_extract] %>%
    str_split('/') %>%
    .[[1]] 
  
  id_soil <- grep('.sol', split_id_soil, value = T) %>%
    str_split('.sol') %>%
    .[[1]] %>%
    .[1]
  
  
  pos_extract <- '.crp' %>%
    grep(dir_files)
  
  split_id_soil <- dir_files %>%
    .[pos_extract] %>%
    str_split('/') %>%
    .[[1]] 
  
  id_crp <- grep('.crp', split_id_soil, value = T) %>%
    str_split('.crp') %>%
    .[[1]] %>%
    .[1]
  
  pos_extract <- '.exp' %>%
    grep(dir_files)
  
  split_id_soil <- dir_files %>%
    .[pos_extract] %>%
    str_split('/') %>%
    .[[1]] 
  
  id_exp <- grep('.exp', split_id_soil, value = T) %>%
    str_split('.exp') %>%
    .[[1]] %>%
    .[1]
  
  return(list(id_soil = id_soil, id_crp = id_crp, id_exp = id_exp))
  
}

execute_oryza <- function(dir_run){
  
  setwd(dir_run)
  system(paste0(' ORYZA3.exe'), ignore.stdout = T, show.output.on.console = F)
  setwd('..')
  
  
}


## make id run
## dir_run <- 'D:/CIAT/USAID/Oryza/usaid_forecast_rice/Prueba/'
## region <- "Saldaña"
## cultivar <- 'fedearroz2000'   ## el nombre de que depende??
# day <- 1   ## dia para correr a partir de donde se genera el pronostico climatico

make_id_run <- function(dir_run, region, cultivar, day){
  
  
  require(stringr)
  dir <- paste0(dir_run, region, '/', cultivar, '/', day, '/')
  dir <- stringr::str_replace(dir, "ñ", "n")
  
  if (!dir.exists(dir)) { 
    
    dir.create(dir, showWarnings = F, recursive = TRUE, mode = "777")
    # system('chmod 777 *.*')
    # paste0(dir_base, region, '/', cultivar,  '/', select_day)
    
  }
  
  return(paste0(dir))
}




# data <- climate
# number_days <- 45

make_PS <- function(data, number_days){
  
  require(tidyverse)
  require(lubridate)
  require(magrittr)
  
  PDATE <- data[[1]] %>%
    filter( row_number() == 1:number_days) %>%
    select(julian_day) %>%
    extract2(1) %>%
    as.numeric()
  
  SDATE <- data[[1]] %>%
    filter( row_number() == 1) %>%
    select(julian_day) %>%
    extract2(1) %>%
    as.numeric()
  
  DATE <- data[[1]] %>%
    filter( row_number() == 1:number_days) %>%
    select(year_2) %>%
    extract2(1)
  
  DATE_format <- data[[1]] %>%
    filter( row_number() == 1:number_days) %>%
    select(frcast_date) %>%
    extract2(1)
  
  dates_inputs <- crossing(PDATE, SDATE) %>%
    mutate(IYEAR = DATE, DATE = DATE_format)
  
  return(dates_inputs)
  
}


tidy_climate <- function(dir_climate, number_days){
  
  
  climate_scenarios <- load_all_climate(dir_climate)
  input_dates <- make_PS(climate_scenarios, number_days)
  return(list(input_dates = input_dates, climate_scenarios = climate_scenarios))
}



## read summary oryza
# id_run
read_op <- function(dir_run){
  
  require(tidyverse)
  op_df <- read_table(paste0(dir_run, 'op.dat'))
  return(op_df)
}




##3 functions to make descriptive


mgment_no_run <- function(data){
  
  ifelse(data == -99, 0, data)
  
}




conf_lower <- function(var){
  
  t.test(var)$conf.int[1]
}

conf_upper <- function(var){
  
  t.test(var)$conf.int[2]
}


CV <- function(var){
  
  (sd(var)/mean(var))*100
  
}


# data <- op_dat
# var <- 'WRR14'

calc_desc <- function(data, var){
  
  data <- select_(data, var)
  reclas_call <- lazyeval::interp(~ mgment_no_run(var), var = as.name(var))
  
  data <- data %>%
    mutate_(.dots = setNames(list(reclas_call), var)) %>%
    summarise_each(funs(avg = mean(.), 
                        median = median(.), 
                        min = min(.), 
                        max = max(.), 
                        quar_1 = quantile(., 0.25), 
                        quar_2 = quantile(., 0.50), 
                        quar_3 = quantile(., 0.75), 
                        conf_lower = conf_lower(.), 
                        conf_upper = conf_upper(.), 
                        sd = sd(.), 
                        perc_5 = quantile(., 0.05),
                        perc_95 = quantile(., 0.95), 
                        coef_var = CV(.))) %>%
    mutate(measure = paste(var)) %>%
    select(measure, everything())
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
    select(weather_station, 
           soil, 
           cultivar, 
           start, 
           end, 
           everything())
  
  return(data)
  
}




run_mult_oryza <- function(dir_run, dir_files, region, cultivar, climate_scenarios, input_dates, location, select_day, number_days, output, dirModeloArrozOutputs){
  
  require(foreach)
  # proof
  
  # number_days <- 3
  # input_dates <- climate_PS$input_dates
  # climate <- climate_PS$climate
  # id_soil <- ID_SOIL
  
  out_csv <- output$name_csv
  iterators <- rep(1:number_days, by = select_day)  
  
  out_op <- foreach(i = iterators) %do% {
    
    # print(i)
    run_oryza(dir_run, dir_files, region, cultivar, climate$climate_scenarios, climate$input_dates, location, i, output)
    
    
    
  } 
  
  out_summary <- bind_rows(out_op)
  write_csv(out_summary, paste0(dirModeloArrozOutputs, out_csv))
  return(out_op)
}

frame_list <- function(data){
  
  setNames(split(data[,2], seq(nrow(data))), data[,1])
  
}


load_coordinates <- function(dir_parameters){
  
  require(readr)
  coordenadas <- read_csv(paste0(dir_parameters,'coordenadas.csv')) %>%
    as.data.frame() %>%
    frame_list()
  
}


temp_wther <- function(temp_dir){
  
  # temp_dir <- id_run
  temp_dir <- paste0(str_split(temp_dir, '/')[[1]][1], "/", 'tmp', "/")
  
  if(dir.exists(temp_dir)){
    
    dir.create(temp_dir)
  } else{
    
    unlink(file.path(temp_dir), recursive = T, force = T)
    dir.create(temp_dir)
  }
  return(temp_dir)
}


output_names <- function(hashCrop, hashSoil, name_csv){
  
  output <- list()
  output$name_csv <- name_csv
  output$cultivar <- hashCrop 
  output$soil <- hashSoil
  
  return(output)
}


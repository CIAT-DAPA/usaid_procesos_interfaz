### Get outputs simulation - DSSAT --->> Aclimate - EDACaP
# Author: Rodriguez-Espinoza J.
# https://github.com/jrodriguez88/
# 2022



## read summary dssat
# id_run
### read output

read_summary <- function(dir_run){
  
  summary_out <- read_table(paste0(dir_run, 'Summary.OUT'), skip = 3 , na = "*******", col_types = cols())
  
  
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



##3 functions to make descriptive



# data <- op_dat
# vars <- c('yield_14', 'prec_acu', 't_max_acu', 't_min_acu', 'bio_acu', 'd_har')
# outputs


extract_summary_aclimate <- function(data, var){
  
  
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
  
  

  data <- dplyr::select_(data, var)
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
    dplyr::select(measure, everything())
  return(data)
}


#map(c('yield_14', 'prec_acu', 't_max_acu', 't_min_acu', 'bio_acu', 'd_har'), 
#    ~extract_summary_aclimate(op_dat, .x)) %>%
#  bind_rows %>% tidy_descriptive(., region, output$soil, output$cultivar, DATE, DATE)
#

#output_names <- function(hashCrop, hashSoil, name_csv){
#  
#  output <- list()
#  output$name_csv <- name_csv
#  output$cultivar <- hashCrop 
#  output$soil <- hashSoil
#  
#  return(output)
#}

tidy_descriptive <- function(data, region, soil, cultivar, start, end){
  
#  require(lubridate)
  
  data <- data %>%
    mutate(weather_station = region,
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




#get_op_aclimate <- fu
#
#op_dat <- read_op(id_run) %>%
#  mutate(yield_14 = WRR14,
#         prec_acu = RAINCUM,
#         t_max_acu = TMAXC,
#         t_min_acu = TMINC,
#         bio_acu = WAGT, 
#         d_har = DAE)
#
#
#yield <- calc_desc(op_dat, 'yield_14') %>%
#  tidy_descriptive(region, output$soil, output$cultivar, DATE, DATE)
#
#prec_acu <- calc_desc(op_dat, 'prec_acu') %>%
#  tidy_descriptive(region, output$soil, output$cultivar, DATE, DATE)
#
#t_max_acu <- calc_desc(op_dat, 't_max_acu') %>%
#  tidy_descriptive(region, output$soil, output$cultivar, DATE, DATE)
#
#t_min_acu <- calc_desc(op_dat, 't_min_acu') %>%
#  tidy_descriptive(region, output$soil, output$cultivar, DATE, DATE)
#
#bio_acu <- calc_desc(op_dat, 'bio_acu') %>%
#  tidy_descriptive(region, output$soil, output$cultivar, DATE, DATE)
#
#d_har <- calc_desc(op_dat, 'd_har') %>%
#  tidy_descriptive(region, output$soil, output$cultivar, DATE, DATE)
#
#summary_stats <- dplyr::bind_rows(list(yield, 
#                                       prec_acu,
#                                       t_max_acu,
#                                       t_min_acu,
#                                       bio_acu,
#                                       d_har))
#
##Borrar .bins
#return(summary_stats)
## unlink(paste0(id_run, ''))
#setwd(dir_run)
#
#}






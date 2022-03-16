### Get outputs simulation - ORYZA linux --->> 
# Author: Rodriguez-Espinoza J.
# https://github.com/jrodriguez88/
# 2022



## read summary oryza
# id_run
read_op <- function(dir_run){
  
#  require(tidyverse)
  op_df <- read_table(paste0(dir_run, 'op.dat')) %>%
    mutate(yield_14 = WRR14,
           prec_acu = RAINCUM,
           t_max_acu = TMAXC,
           t_min_acu = TMINC,
           bio_acu = WAGT, 
           d_har = DAE) 
  
  
  
#  bin_files <- list.files(dir_run, pattern = fixed("^res"), full.names = T)
#  lapply(bin_files, file.remove)
  # delete climate files  
  unlink(file.path(list.files(dir_run, full.names = T)), recursive = T, force = T)
    
  return(op_df)
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






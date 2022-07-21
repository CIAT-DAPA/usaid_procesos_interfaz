### Get outputs simulation - DSSAT --->> Aclimate - EDACaP
# Author: Rodriguez-Espinoza J. Mesa-Diez J.
# https://github.com/jrodriguez88/
# 2022



## read summary dssat - 'Summary.OUT' file
read_summary <- function(dir_run){
  
  summary_out <- read_table(paste0(dir_run, 'Summary.OUT'), skip = 3 , na = "*******", col_types = cols()) %>%
    dplyr::mutate(yield_0 = HWAM,
                  d_dry = as.numeric(as.Date(as.character(MDAT), format("%Y%j")) - as.Date(as.character(PDAT), format("%Y%j"))),
                  prec_acu = PRCP,
                  bio_acu = CWAM)
  
  
  
  return(summary_out)
}

## read weather dssat - "Weather.OUT" file
read_wth_out <- function(dir_run){
  

file <- paste0(dir_run, "Weather.OUT")
skip <- read_lines(file)  %>% str_detect("@YEAR") %>% which()-1 

cal_summ <- function(data){
  
  data %>% tibble %>% mutate(across(.fns = as.numeric)) %>%
    summarise(t_max_acu = sum(TMXD), t_min_acu = sum(TMND), srad_acu = sum(SRAD))
  
}

data_wth <- map(skip, ~fread(file, skip = .x)) %>% 
  map(cal_summ)


data_wth %>% bind_rows(.id = "scenario")

}
  

## functions to make descriptive - Extract summary - STATS-metrics
extract_summary_aclimate <- function(data, var){
  
  

  conf_lower <- function(var){
    
    t.test(var)$conf.int[1]
  }
  
  conf_upper <- function(var){
    
    t.test(var)$conf.int[2]
  }
  
  
  CV <- function(var){
    
    (sd(var)/mean(var))*100
    
  }
  
  

  data <- dplyr::select(data, var) %>% drop_na()
  
  
  data <- data %>%
    summarise(across(var, .fns =list(avg = mean, 
                        median = median, 
                        min = min, 
                        max = max, 
                        quar_1 = ~quantile(.x, 0.25), 
                        quar_2 = ~quantile(.x, 0.50), 
                        quar_3 = ~quantile(.x, 0.75), 
                        conf_lower = conf_lower, 
                        conf_upper = conf_upper, 
                        sd = sd, 
                        perc_5 = ~quantile(.x, 0.05),
                        perc_95 = ~quantile(.x, 0.95), 
                        coef_var = CV))) %>%
    mutate(measure = paste(var)) %>%
    dplyr::select(measure, everything()) %>% 
    rename_with(~str_remove(., paste0(var, "_")))
  return(data)
}


## Function to arrange data to final data frame
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







### Get outputs simulation - DSSAT --->> Aclimate - EDACaP
# Author: Rodriguez-Espinoza J. Mesa-Diez J.
# https://github.com/jrodriguez88/
# 2022



## read summary dssat - 'Summary.OUT' file
read_summary <- function(dir_run){
  
  var_names <- read_lines(paste0(dir_run, 'Summary.OUT'))[4] %>% 
    str_sub(5, -1) %>% str_split("\\s{1,}") %>% pluck(1)
  
  summary_out <-  fread(paste0(dir_run, 'Summary.OUT'), header = F, col.names = var_names, na.strings = "-99") %>%
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

data_wth <- suppressWarnings(map(skip, ~fread(file, skip = .x))) %>% 
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
  
  

  data <- dplyr::select(data, all_of(var)) %>% drop_na()
  
  
  data <- data %>%
    summarise(across(all_of(var), .fns =list(avg = mean, 
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
safe_extract_summary <- purrr::possibly(extract_summary_aclimate, NULL)

#list_st = list of values
tidy_stress <- function(list_st, names_op) {
  
  names_st <- names_op[-c(1:5)]
  
  dat_st <- map(list_st, 
                ~tibble(names = names_st, values = .x) %>% 
                  pivot_wider(names_from = names, values_from = values)) %>% 
    bind_rows()
  
  return(dat_st)
    
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


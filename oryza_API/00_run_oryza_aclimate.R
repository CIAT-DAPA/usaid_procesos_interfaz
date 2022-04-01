### Run ORYZA ACLIMATE - ORYZA linux --->> RUn simulation by set up (45 days - 100 climate scenaries)
# Author: Rodriguez-Espinoza J.
# https://github.com/jrodriguez88/
# 2022

#### Process start here ---new ORYZA API
tictoc::tic()
#library(tidyverse)
#library(rstudioapi) 
library(foreach)
library(parallel)
library(iterators)
library(doParallel)
library(rebus)
library(dplyr)
library(purrr)
library(readr)
library(lubridate)
library(stringr)
library(lazyeval)
library(magrittr)


# Set working directory
script_dir <- dirname(sys.frame(1)$ofile)
setwd(paste0(script_dir, "/"))

# Folders
dir_scripts <- "oryza_scripts/"
dir_outputs <- "outputs/" ; dir.create(dir_outputs)


  # Set up - Simulation days 
#sim_ini_day <- 1 ## dia a correr a partir del pronostico climatico generado - Variable estatica?
#sim_end_day <- 45 ## numero de dias a simular 45

# Set up run paths
dir_inputs_climate <- "inputs/climate/"
dir_inputs_setup <- "inputs/setups/"

# Source oryza-aclimate functions
walk(list.files(dir_scripts, pattern = ".R$", full.names = T), ~source(.x))



## Location vars/ data / resampling scenaries / planting dates
id <- list.dirs(dir_inputs_setup)[2] %>% str_split("//") %>% map_chr(2)
site <- map_chr(str_split(id, "_"), 1)
cultivar <- map_chr(str_split(id, "_"), 2)
soil <- map_chr(str_split(id, "_"), 3)


location <- load_coordinates(paste0((list.dirs(dir_inputs_setup)[2]), "/"))
climate_scenaries <- load_all_climate(paste0((list.dirs(dir_inputs_climate)[2]), "/"))
input_dates <- make_sim_dates(climate_scenaries, 45)



#output <- output_names(hashCrop, hashSoil, name_csv)


### RUN ORYZA

#select_day <- sim_ini_day


lat <- location$lat
long <- location$long
elev <- location$elev

## Crea las configuraciones  para simular 45 dias 

dir_run <- map(1:45, ~make_dir_run(dir_outputs, .x))


## crea scenarios climaticos en formato ORYZA/ copia inputs / escribe control

#map(dir_run, ~write_weather_escenaries(climate_scenaries, .x, long, lat, elev))

map(dir_run, ~copy_inputs(paste0((list.dirs(dir_inputs_setup)[2]), "/"), .x))

map(dir_run, write_control)

pmap(.l = list(dir_run = dir_run, 
               EMD = input_dates$PDATE, 
               STTIME = input_dates$SDATE, 
               IYEAR = input_dates$IYEAR, 
               EMYR = year(input_dates$DATE)), 
     .f = write_reruns_aclimate)


dir_run %>% map(~file.copy(list.files(".", pattern = ".EXE$", full.names = T), .x))


#tictoc::tic()
ncores <- detectCores()/2
registerDoParallel(ncores)


sim_data  <- foreach(
  i = dir_run, 
  .export=c('execute_oryza', 'read_op', 'write_weather_escenaries', 'write_cli_file', 'climate_scenaries', 'lat', 'long', 'elev'), 
  .packages=c('dplyr', 'readr', 'lubridate')) %dopar% {
    write_weather_escenaries(climate_scenaries, i, long, lat, elev)
    execute_oryza(i) 
    read_op(i)
  
}

closeAllConnections()

outputs_df <- map2(.x = sim_data,
                   .y = input_dates$DATE, 
                   function(x,y){
                     map(c('yield_14', 'prec_acu', 't_max_acu', 't_min_acu', 'bio_acu', 'd_har'), 
                         ~extract_summary_aclimate(x, .x)) %>% 
                       bind_rows() %>% 
                       tidy_descriptive(., site, soil, cultivar, y, y)}) %>% 
  compact %>% bind_rows()






write_csv(outputs_df, paste0("outputs/", id, ".csv"))

tictoc::toc()



#dir_oryza = "oryza_API/"
#list.files(dir_oryza, pattern = ".EXE$", full.names = T) 


map(dir_run, ~unlink(.x, recursive=TRUE))


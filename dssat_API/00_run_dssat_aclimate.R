### Run DSSAT ACLIMATE - DSSAT linux --->> RUn simulation by set up (45 days - 100 climate scenaries)
# Author: Rodriguez-Espinoza J.
# https://github.com/jrodriguez88/
# 2022

#### Process start here ---
#tictoc::tic()
#library(tidyverse)
#library(rstudioapi) 
library(foreach)
library(parallel)
library(iterators)
library(doParallel)
library(rebus)
library(dplyr)
library(tidyr)
library(tibble)
library(purrr)
library(readr)
library(lubridate)
library(stringr)
library(lazyeval)
library(magrittr)


#Set crop
crop <- "wheat"
cultivar <- c("CROP00",  "P30F35")
soil <- "USAID00001"


# Set working directory
script_dir <- dirname(sys.frame(1)$ofile)
setwd(paste0(script_dir, "/"))

# Folders
dir_scripts <- "dssat_scripts/"
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
initial_date  <- climate_scenaries[[1]]$date[[1]] + days(15)
input_dates <- make_sim_dates(initial_date, planting_before = 15, number_days = 30, freq_sim = 3)
sim_number <- length(input_dates$start_date)  # It depends of planting window form forecast




#output <- output_names(hashCrop, hashSoil, name_csv)


### RUN DSSAT

#select_day <- sim_ini_day
lat <- location$lat
long <- location$long
elev <- location$elev

## Crea las configuraciones  para simular 45 dias 

dir_run <- map(1:sim_number, ~make_dir_run(dir_outputs, .x))


## crea scenarios climaticos en formato ORYZA/ copia inputs / escribe control

#map(dir_run, ~write_weather_escenaries(climate_scenaries, .x, long, lat, elev))


# copy defaulf inputs
map(dir_run, ~copy_inputs(paste0((list.dirs(dir_inputs_setup)[2]), "/"), .x))


# write DSSAT Batch file 
#crop, xfile, treatments_number, filename
id_name <- "CIAT0001"    ### ID for new workflow
batch_filename <- paste0(dir_run, "/", "DSSBatch.v47")
xfile <- crop_name_setup(id_name, crop)[[3]]
treatments_number <- length(climate_scenaries) - 1   # number of escenaries

map(batch_filename, ~write_batch_aclimate(crop, xfile, treatments_number, .x))

#CR <- read_lines(list.files(dir_inputs_setup, full.names = T, recursive = T, pattern = "*.CUL")) 


## Write Xfile - Set management params

#cultivar <- c("AW0071","Yecora_Rojo")
#cultivar <- c("CROP00",  "P30F35")
#soil <- "USAID00001"
wth_station <- paste0("CIAT", sprintf("%.4d", 1:treatments_number))
planting_details <- map(paste0(dir_run, "planting_details.csv"), read_csv) %>% 
  map(~.x %>% pivot_wider(names_from = name))
irri <- F
fert_in <- NULL

X_param <- dir_run %>% unlist() %>% enframe(name = NULL, value = "path") %>%
  mutate(id_name = id_name,
         crop = crop, 
         cultivar = list(cultivar),
         soil = soil, 
         wth_station = list(wth_station),
         planting_details = planting_details, 
         irri = irri,
         fert_in = list(fert_in),
         start_date = input_dates$start_date,
         planting_date= input_dates$planting_date,
         emergence_date = -99, 
         treatments_number = treatments_number)
  

pmap(X_param, write_exp_dssat)

#test <- X_param %>% slice(1)
#
#write_exp_dssat(test$path, test$id_name, test$crop, test$cultivar[[1]], 
#                test$soil, test$wth_station, test$planting_details[[1]], test$irri, test$fert_in, test$start_date,
#                test$planting_date, test$emergence_date, test$treatments_number)


#dir_run %>% map( function(x) map2(climate_scenaries[-100], wth_station, ~write_wth_file(.x, x, .y, lat, long)))

#tictoc::tic()
ncores <- detectCores()-2
if(ncores > sim_number){ncores <- sim_number}
registerDoParallel(ncores)


sim_data  <- foreach(
  i = dir_run, 
  .export=c('execute_dssat', 'write_wth_file', 'wth_station', 'climate_scenaries', 'lat', 'long'), 
  .packages=c('dplyr', 'readr', 'lubridate', 'purrr')) %dopar% {
  map2(climate_scenaries[-100], wth_station, ~write_wth_file(.x, i, .y, lat, long))
    #execute_oryza(i) 
#    read_op(i)
  
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


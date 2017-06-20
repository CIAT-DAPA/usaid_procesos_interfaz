## script to proof all functions generated
#library(tidyverse)
#library(lubridate)
#library(magrittr)
#library(data.table)
#library(lazyeval)
#library(foreach)

# path_functions <- "D:/CIAT/USAID/DSSAT/multiple_runs/R-DSSATv4.6/"  # path when is the functions necessary to run by region, scenario and multiple days to platform USAID forecast
path_functions <- dirModeloMaiz
# print(path_functions)

# print(output)


# dir_dssat <- 'C:/DSSAT46/'  ## its necessary to have the parameters .CUL, .ECO, .SPE Updated for running (calibrated the crop (Maize))
# dir_run <- 'D:/CIAT/USAID/DSSAT/multiple_runs/R-DSSATv4.6/'
# dir_soil <- 'D:/CIAT/USAID/DSSAT/multiple_runs/R-DSSATv4.6/Runs/CC.SOL'  # it is not only the folder is all path when is the soil file
# dir_climate <- 'D:/CIAT/USAID/DSSAT/multiple_runs/R-DSSATv4.6/stations/Forecasts/Escenarios/'
# dir_climate <- 'D:/CIAT/USAID/DSSAT/multiple_runs/R-DSSATv4.6/stations/Forecasts/Escenarios_update_csv/'
# region <- "LaUnion" 
name_files <-"USAID"   ## Weather station (generic), x-file name and for the indicate the run into the DSSBatch
cultivar <- 'CROP00'   ## cultivar a correr
ID_SOIL <- 'USAID00001' ## id para el suelo



## Esto deberia ser constante  
select_day <- 1 ## 1 primer dia a simular 2 segundo dia etc....  select day to simulate from first date of climate forecast o then
number_days <- 45 ## Numero de dias a simular desde el primer dia del pronostico climatico (SDATE siempre seria el primer dia del pronostico)

## agregar out dir donde van a estar las salidas (el .csv que se necesita para el servidor)
# name_csv <- paste(dir_outMaiz, "prueba.csv", sep = "", collapse = NULL)
# name_csv <- "prueba.csv"

## add source functions


source(paste0(path_functions, 'make_wth.R'))
source(paste0(path_functions, 'make_batch.R'))
source(paste0(path_functions, 'make_parameters.R'))
source(paste0(path_functions, 'main_functions.R'))
source(paste0(path_functions, 'settings_xfile.R'))
source(paste0(path_functions, 'functions_xfile.R'))
source(paste0(path_functions, 'run_dssat.R'))


output <- output_names(hashCrop, hashSoil, name_csv)


# input_dates <- tidy_climate(dir_climate)
## Climate data wit PDATE and SDATE tidy

climate_PS <- tidy_climate(dir_climate, number_days)
# climate_scenarios <- load_climate(dir_climate)
# input_dates <- climate_PS$input_dates
# climate <- climate_PS$climate


## simulating by input_dates from rows
## select day is a variable that configure when starting the simulation from forescast climate

# run_dssat(dir_dssat, dir_soil, dir_run, region, name_files, climate_PS$input_dates, 2, cultivar, climate_PS$climate, ID_SOIL)
# run_dssat(dir_dssat, dir_soil, dir_run, region, name_files, climate_PS$input_dates, 1, cultivar, climate_PS$climate, ID_SOIL, name_csv)

run_mult_dssat(dir_dssat,
               dir_soil,
               dir_run,
               dir_parameters,
               name_files,
               climate_PS$input_dates,
               select_day,
               cultivar,
               climate_PS$climate,
               ID_SOIL,
               number_days,
               output,
               region,
               dirModeloMaizOutputs)

# if (file.exists(file.path(dir_base))){
#     unlink(file.path(dir_base), recursive = TRUE, force = TRUE)
#     cat (paste0('\n... directorio "',dir_base,'" eliminado\n'))
# }

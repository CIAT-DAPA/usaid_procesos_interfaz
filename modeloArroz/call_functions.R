# Librerias Necesarias

# library(lubridate)
# library(tidyverse)
# library(rebus)   ## for backslash
# library(lubridate)
# library(magrittr)
# library(stringr)
# library(foreach)




#path_functions <- 'D:/CIAT/USAID/Oryza/usaid_forecast_rice/'
path_functions <- dirModeloArroz
dir_out_csv <- dirModeloArrozOutputs

#dir_climate <- "D:/CIAT/USAID/Oryza/Escenarios_update_csv/"
#dir_run <- 'D:/CIAT/USAID/Oryza/usaid_forecast_rice/'
filename <- 'USAID'  ## name files
dir_oryza <- 'C:/Program Files (x86)/ORYZA(v3)/'  ## necesario crear una carpeta en esta direccion con los cultivares y archivos experimentales para cada region
#region <- "Salda?a"
# cultivar <- 'D:/CIAT/USAID/Oryza/usaid_forecast_rice/Experimental_Cultivar_Files/'


#cultivar <- "fedearroz2000"
day <- 1 ## dia a correr a partir del pronostico climatico generado
number_days <- 45 ## numero de dias a simular 45

# dir_exp_files <- 'D:/CIAT/USAID/Oryza/usaid_forecast_rice/Experimental_Cultivar_Files/'   ## directorio donde se encuentran los archivos experimentales y cultivares
dir_exp_files <- dir_parameters

## add source functions

source(paste0(path_functions, "write_control.R"))
source(paste0(path_functions, "settings_control.R"))
source(paste0(path_functions, "main_functions.R"))
source(paste0(path_functions, "make_weather.R"))
source(paste0(path_functions, "write_reruns.R"))
source(paste0(path_functions, "settings_reruns.R"))
source(paste0(path_functions, "run_oryza.R"))


climate <- tidy_climate(dir_climate, number_days) ## carga todos los escenarios climaticos, organiza los valores para ORYZa y a?ade la fecha del pronostico ademas de a?adir planting date and simulation date


## function to do this, depend by region?

location <- load_coordinates(dir_exp_files) 

output <- output_names(hashCrop, hashSoil, name_csv)
## is necessary to add lat, long, elev ? or is possible to put -99 for this variables 

# run_oryza(dir_run, dir_files, region, cultivar, climate$climate_scenarios, climate$input_dates, location, day)

# name_csv <- "prueba.csv" 

run_mult_oryza(dir_run,
               dir_exp_files,
               region,
               cultivar,
               climate$climate_scenarios,
               climate$input_dates,
               location,
               day,
               number_days,
               # name_csv,
               output,
               dir_out_csv, 
               filename, 
               dir_oryza)



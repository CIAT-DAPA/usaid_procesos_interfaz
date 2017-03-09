## make climate file

# libs required
# library(tidyverse)
# library(lubridate)

## Path scenarios climate file
# path <- '//dapadfs/Workspace_cluster_9/USAID_Project/Product_3_agro-climatic_forecast/crop/Rice/Clima/Escenarios/'
# path <- 'D:/CIAT/USAID/DSSAT/multiple_runs/R-DSSATv4.6/stations/Forecasts/Escenarios_update_csv/'

## verify if ORYZA use a head for lat, long, elev, 0,0   (Jeferson Rodriguez said me yes!!!!)
# estos parametros deberian estar incluidos en la estacion basicamente es la Region a correr

# lat <- 8.84
# long <- -75.8
# elev <- 84


## first question 
## how to generate the name of the file for the weather to run ORYZA v3

# climate_list <- load_climate(path)

## add make_date


#  climate_df <- climate[[1]]


# make_weather(proof_file, filename = 'EO1.016', long, lat, elev, scenario)
# filename <- paste0('D:/CIAT/USAID/Oryza/usaid_forecast_rice/Prueba/USAI1.clim')

make_weather <- function(climate_df, filename, long, lat, elev, scenario){
  
  require(tidyverse)
  
  climate_df <- climate_df %>%
    mutate(y = x) %>%
    select(scenario, year_2, julian_day, sol_rad, t_min, t_max, x, y, prec)
  
  sink(file = filename, append = F)
  
  cat(paste0(long, ',', lat, ',', elev, ',', 0, ',', 0))
  cat('\n')
  write.table(climate_df, sep = ",", row.names = F, col.names = F)
  
  sink()
  
}


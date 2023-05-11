### Run DSSAT EDACaP - DSSAT linux --->> RUn simulation by set up (45 days - 100 climate scenaries)
# Author: Rodriguez-Espinoza J.
# https://github.com/jrodriguez88/
# 2022


# DSSAT Version 4.8
# Crops tested : c("rice", "maize", "barley", "sorghum", "wheat", "bean", "fababean", "teff")

##Settings:
# Number of Climate Scenaries: 99 ( max allow by DSSAT x file)
# 11 Planting dates (30 days - 1 sim/3 days) ..first planting date = 15 days after first day forecast (climate scenaries)
# 1 Soil - DSSAT ID
# irri <- F  --  Rainfed mode
# fert_in <- NULL - No fertilization


##Libraries to prof funtion performance
#library(profvis)
#library(bench)
#library(tictoc)



##Conect geoserver
#library(raster)
#library(furrr)
options(warn = 1)


### Inputs - Arguments

#id = "5e91e1c214daf81260ebba59_60a16e2826e98d13b8dbb878_6334a6d230243c12cc1fa8c3_3"
#path = "/forecast/workdir/dssat_API/"
#crop = "wheat"
#cultivar = c("AW0071","Yecora_Rojo")
#soil = "IB00000001"
# Source dssat-aclimate functions
#walk(list.files("dssat_scripts/", pattern = ".R$", full.names = T), ~ source(.x))


run_crop_dssat <- function(id, crop, current_dir_inputs_climate, current_setup_dir, ndays = 45, soil=NULL, cultivar=NULL){
  
  path = "/forecast/usaid_procesos_interfaz/dssat_API/"
  setwd(path)
  wd_p <- paste0(getwd(), "/")
  if(str_detect(wd_p, pattern = path, negate = T)) {setwd(path)}
  
  
  #id <- id
  #crop <- crop
  #cultivar <- cultivar
  #soil <- soil
  
  options(warn = 1)
  
  
  #Set crop
  #crop <- "wheat"
  #crop <- "barley"
  #cultivar <- c("AW0071","Yecora_Rojo")
  #cultivar <- c( "IB0030", "Maris Badger")
  #soil <- "IB00000001"
  #id <- "5a7e2e6a57d7f316c8bc514a_59b024a0b74a4a10f487eaa6_5b3edfe7b16a0d2edc1107e4_1"
  #id <- paste(crop, cultivar[2], soil, irri, fert, sep = "_") %>% str_remove('\\*')
  id_station <- map_chr(str_split(id, "_"), 1)
  id_cultivar <- map_chr(str_split(id, "_"), 2)
  id_soil <- map_chr(str_split(id, "_"), 3)
  sim_freq <- as.numeric(map_chr(str_split(id, "_"), 4))
  
  
  # Set working directory
  #script_dir <- dirname(sys.frame(1)$ofile)
  #setwd(paste0(script_dir, "/"))
  
  # Folders
#  dir_scripts <- "dssat_scripts/"
  #dir_outputs <- "outputs/" ; dir.create(dir_outputs)
  dir_outputs <- paste0(dirOutputs, "cultivos/", if (currentCountry == "COLOMBIA" && crop == "maize") "maiz" else crop, "/", sep = "", collapse = NULL)

  # Set up run paths
  dir_inputs_climate <- current_dir_inputs_climate
  dir_inputs_setup <- current_setup_dir
  dir_inputs_soil <- current_setup_dir
  dir_inputs_cultivar <- current_setup_dir
  
  # Source oryza-aclimate functions
 # walk(list.files(dir_scripts, pattern = ".R$", full.names = T), ~ source(.x))
  
  # ISO code / codigo iso de cada configuracion --- define el nombre del archivo de salida
  ## Location vars/ data / resampling scenaries / planting dates
  location <- load_coordinates(dir_inputs_setup)
  
  ### For EDACaP location file contain id_soil and cultivar name
  # if(currentCountry=="COLOMBIA" || (currentCountry=="ETHIOPIA" && crop == "wheat")){
  #   soil <- soil
  #   cultivar <- cultivar
  # } else {
  #   soil <- location$id_soil %>% str_sub(., 2,-1)
  #   cultivar <- c(location$var_cul, location$cul_name)
  # }
  
  climate_scenaries <- load_all_climate(dir_inputs_climate)[-100]
  planting_details_column_name <- if (currentCountry=="COLOMBIA") "value" else crop
  
  planting_details <- read_csv(paste0(dir_inputs_setup, "planting_details.csv"), show_col_types = F) %>%
    dplyr::select(name, all_of(planting_details_column_name)) %>%  pivot_wider(names_from = name, values_from = all_of(planting_details_column_name))
  
  # Definir fecha inicial de simulacion/  
  #En este caso la define automaticamente de la fecha inicial de los escenarios climaticos
  initial_date  <- climate_scenaries[[1]]$date[[1]] + days(30)
  
  input_dates <- make_sim_dates(initial_date, planting_before = 15, number_days = ndays, freq_sim = strtoi(strsplit(id, "_", fixed=T)[[1]][4]))
  sim_number <- length(input_dates$start_date)  # It depends of planting window form forecast
  
  ## Parallel computing 
  ncores <- detectCores()-2
  if(ncores > sim_number){ncores <- sim_number}
  #plan(multisession, workers = ncores)
  
  
  ### RUN DSSAT
  #select_day <- sim_ini_day
  lat <- as.numeric(location$lat)
  long <- as.numeric(location$long)
  elev <- as.numeric(location$elev)
  
  ## Crea las configuraciones  para simular 45 dias 
  current_dir_run <- paste0(dir_outputs, id, "/")
  dir.create(current_dir_run)
  dir_run <- map(1:sim_number, ~make_dir_run(current_dir_run, .x))
  
  # copy default inputs
  map(dir_run, ~copy_inputs(dir_inputs_setup, dir_inputs_soil, dir_inputs_cultivar, crop, .x))
  
  
  # write DSSAT Batch file 
  id_name <- "CIAT0001"    ### ID for new workflow
  batch_filename <- paste0(dir_run, "/", "DSSBatch.v48")
  xfile <- crop_name_setup(id_name, crop)[["ext"]]
  treatments_number <- length(climate_scenaries)    # number of escenaries
  
  map(batch_filename, ~write_batch_aclimate(crop, xfile, treatments_number, .x))
  
  #CR <- read_lines(list.files(dir_inputs_setup, full.names = T, recursive = T, pattern = "*.CUL")) 
  
  
  ## Write Xfile - Set management params
  
  wth_station <- paste0("CIAT", sprintf("%.4d", 1:treatments_number))
  
  
  irri <- ifelse(planting_details$IRR == "YES", T, F)
  fert_in <- get_fertilizer(crop, planting_details, dir_inputs_setup, lat, long)
  
  X_param <- dir_run %>% unlist() %>% enframe(name = NULL, value = "path") %>%
    mutate(id_name = id_name,
           crop = crop, 
           cultivar = list(cultivar),
           soil = soil, 
           wth_station = list(wth_station),
           planting_details = list(planting_details), 
           irri = irri,
           fert_in = list(fert_in),
           start_date = input_dates$start_date,
           planting_date= input_dates$planting_date,
           emergence_date = -99, 
           treatments_number = treatments_number)
  
  
  pmap(X_param, write_exp_dssat)
  
  
  #tictoc::tic()
  
  
  registerDoParallel(ncores)
  sim_data  <- foreach(
    i = dir_run, 
    .export=c('crop', 'execute_dssat', 'write_wth_file', 'wth_station', 'climate_scenaries', 'lat', 'long', 'read_summary', 'read_wth_out', 'crop_name_setup'),
    .packages=c('dplyr', 'stringr', 'readr', 'lubridate', 'purrr', 'data.table')) %dopar% {
      map2(climate_scenaries, wth_station, ~write_wth_file(.x, i, .y, lat, long))
      execute_dssat(i, crop) 
      list(summary = read_summary(i) , weather = read_wth_out(i))
      
    }
  
  closeAllConnections()
  
  #tictoc::toc()
  

  outputs_df1 <- map2(.x = map(sim_data, "summary"),
                      .y = input_dates$planting_date, 
                      function(x,y){
                        map(c('yield_0', 'd_dry', 'prec_acu', 'bio_acu'), 
                            ~safe_extract_summary(x, .x)) %>% compact() %>%
                          bind_rows() %>% 
                          tidy_descriptive(., id_station, id_soil, id_cultivar, y, y)}) %>% 
    compact %>% bind_rows()
  
  outputs_df2 <- map2(.x = map(sim_data, "weather"),
                      .y = input_dates$planting_date, 
                      function(x,y){
                        map(c('t_max_acu', 't_min_acu'), 
                            ~safe_extract_summary(x, .x)) %>% compact() %>%
                          bind_rows() %>% 
                          tidy_descriptive(., id_station, id_soil, id_cultivar, y, y)}) %>% 
    compact %>% bind_rows()

  land_preparation_data <- land_preparation_all(dir_run)
  outputs_df4 <- map2(.x = land_preparation_data,
                      .y = input_dates$planting_date, 
                      function(x,y){
                         x %>% mutate(measure='land_pre_day') %>% dplyr::select(measure, everything()) %>%
                          tidy_descriptive(., id_station, id_soil, id_cultivar, y, y)}) %>% 
    compact %>% bind_rows()
  #Deleting NA rows
  outputs_df4 <- na.omit(outputs_df4)

  

#If crop_conf exists run stress_risk
 if(file.exists(paste0(dir_inputs_setup, "crop_conf.csv"))){
#if(FALSE){
  stress_risk_all_days <- stress_risk_all_safe(dir_run, dir_inputs_setup, initial_date)
  names_op <- names(outputs_df1)
  outputs_df3 <- map2(.x = stress_risk_all_days,
                      .y = input_dates$planting_date, 
                      function(x,y){
                        tidy_stress(x, names_op) %>% 
                        mutate(across(.cols = -measure, .fns = as.numeric))%>%
                          tidy_descriptive(., id_station, id_soil, id_cultivar, y, y)}) %>% 
    compact %>% bind_rows()

  #Replacing NA values
  outputs_df3[is.na(outputs_df3)] <- 0
  drop_na(outputs_df3, coef_var)

  hazard_indicators_count_days <- hazard_count_days_all(dir_run)
  outputs_df5 <- map2(.x = hazard_indicators_count_days,
                      .y = input_dates$planting_date, 
                      function(x,y){
                         x %>% tidy_descriptive(., id_station, id_soil, id_cultivar, y, y)}) %>% 
    compact %>% bind_rows()

  hazard_indicators_water <- hazard_water_all(dir_run)
  outputs_df6 <- map2(.x = hazard_indicators_water,
                      .y = input_dates$planting_date, 
                      function(x,y){
                         x %>% tidy_descriptive(., id_station, id_soil, id_cultivar, y, y)}) %>% 
    compact %>% bind_rows()


  #Fixing end date column. start + sim_freq
  final_csv <- bind_rows(outputs_df1, outputs_df2, outputs_df3, outputs_df4, outputs_df5, outputs_df6)
  final_csv$end <- as.Date(final_csv$end) + (sim_freq - 1)
  final_csv <- na.omit(final_csv)
  write_csv(final_csv, paste0(dir_outputs, id, ".csv"))
  message(paste0("Successful Simulation \n Crop: ", 
                 crop, " - Cultivar: ", cultivar[2], "\n Soil: ", soil, 
                 "\n Irrigation: ", planting_details$IRR, "\n Fertilization: ", planting_details$FERT ))
  
  # This part extract phenological phase dates per each setup
  crop_conf = read_csv(paste0(dir_inputs_setup,"crop_conf.csv"))
  for(i in 1:length(dir_run)){
    data_files <- paste0(dir_run[i])
    phenological_phase_dates = getPhenologicalPhaseDates(data_files,crop_conf,initial_date,id_station,id_cultivar,id_soil)
    write_csv(phenological_phase_dates, paste0(dir_outputs, id, "_phenological-phases.csv"))
  }
  
  setwd(wd_p)
} else {
  #Fixing end date column. start + sim_freq
  final_csv <- bind_rows(outputs_df1, outputs_df2, outputs_df4)
  final_csv$end <- as.Date(final_csv$end) + (sim_freq - 1)
  write_csv(final_csv, paste0(dir_outputs, id, ".csv"))

  
  #tictoc::toc()
  
  message(paste0("Successful Simulation \n Crop: ", 
                 crop, " - Cultivar: ", cultivar[2], "\n Soil: ", soil, 
                 "\n Irrigation: ", planting_details$IRR, "\n Fertilization: ", planting_details$FERT ))
  
  setwd(wd_p)

}

#Deleting model run files
map(current_dir_run, ~unlink(.x, recursive=TRUE))
  
}


#tic()
#run_crop_dssat(id, path, crop, ndays = 44)
#toc()


#tic()
#profvis({run_crop_dssat(id, path, crop, ndays = 44)})

#time <- toc(log = T)
#> time
#$tic
#elapsed 
#508908.3 
#
#$toc
#elapsed 
#509017.5 
#
#$msg
#logical(0)
#
#$callback_msg
#[1] "109.16 sec elapsed"

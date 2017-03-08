
run_dssat <- function(dir_dssat, dir_soil, dir_run, dir_parameters, name_files, input_dates, select_day, cultivar, climate, id_soil, name_csv, name_cultivar, name_soil, region){
  
  ## make dir to run based on a folder input by climate scenario (folder_001, ..... , folder_100) 
  
  options(encoding = "UTF-8")
  
  dir_base <- paste0(dir_run, 'temporal/')

  dir_run_id <- make_id_run(dir_base, region, cultivar, select_day) ## make folder by PDATE? is it confusing them?
  
  ## in this point its necessary to add all functions that can to wirte files (x-file, weather, soil, batch)
  
  # paste0(dir_base, region, '/', cultivar,  '/', select_day)
  
  ## add function to load climate datasets 
  
  # climate_scenarios <- load_climate(dir_climate)
  # select_day <- day
  
  
  ## Quizá lo siguiente agregarlo a una funcion para ser facil de modificar luego 
  
  # PDATE <- climate_scenarios[[1]] %>%
  #   filter( row_number() == select_day) %>%
  #   select(date_dssat) %>%
  #   extract2(1) 
  
  # SDATE <- climate_scenarios[[1]] %>%
  #   filter( row_number() == 1) %>%
  #   select(date_dssat) %>%
  #   extract2(1) 
  
  # PDATE + day 
  
  PDATE <- input_dates$PDATE[select_day]  ## for now when proof change, delete [1]
  SDATE <- input_dates$SDATE[select_day]
  DATE <- input_dates$DATE[select_day]
  
  # DATE <- paste(day(DATE), sprintf("%.2d", month(DATE)), year(DATE), sep = '/')
  
  ## id_soil <- ID_SOIL
  make_xfile_region(dir_parameters, paste0(name_files, sprintf("%.3d", 1:99)), paste0(dir_run_id, name_files, '.MZX'), PDATE, SDATE, cultivar, id_soil) ## Remember them can to change the filename to different regions
  
  
  invisible(make_mult_wth(climate, dir_run_id, name_files))
  
  # Make Batch
  
  CSMbatch("MAIZE", paste0(name_files, '.MZX'), paste0(dir_run_id, "DSSBatch.v46"))
  
  # add files necessay to run DSSAT

  files_dssat(dir_dssat, dir_run_id, dir_soil, dir_parameters)
  
  ### here add function to execute DSSAT
  execute_dssat(dir_run_id)
 
  
  ## here add function to load de output necessary
  
  summary_out <- read_summary(dir_run_id) %>%
                  mutate(yield_0 = HWAH,
                         d_dry = MDAT-PDAT,
                         prec_acu = PRCP,
                         bio_acu = CWAM)   ## rename some variables for the server
  
  weather_out <- read_mult_weather(dir_run_id) %>%
                  group_by(scenario) %>%
                  summarise(t_max_acu = sum(TMXD), t_min_acu = sum(TMND)) 
                  
  ### files .OUT to new dir
  
  
  ## make a Descriptive Statistics
  
  # soil <- ID_SOIL
  
  yield <- calc_desc(summary_out, "yield_0") %>%
              tidy_descriptive(region, name_soil, name_cultivar, DATE, DATE)
  
  d_dry <- calc_desc(summary_out, "d_dry") %>%
              tidy_descriptive(region, name_soil, name_cultivar, DATE, DATE)
  
  prec_acu <- calc_desc(summary_out, "prec_acu") %>%
                tidy_descriptive(region, name_soil, name_cultivar, DATE, DATE)
  
  t_max_acu <- calc_desc(weather_out, "t_max_acu") %>%
                tidy_descriptive(region, name_soil, name_cultivar, DATE, DATE)
  
  t_min_acu <- calc_desc(weather_out, "t_min_acu") %>%
    tidy_descriptive(region, name_soil, name_cultivar, DATE, DATE)
  
  bio_acu <- calc_desc(summary_out, "bio_acu") %>%
    tidy_descriptive(region, name_soil, name_cultivar, DATE, DATE)
  
  summary_stats <- dplyr::bind_rows(list(yield, 
                                         d_dry, 
                                         prec_acu,
                                         t_max_acu,
                                         t_min_acu,
                                         bio_acu))

  
  ## Write files in a particular folder
  
  setwd(dir_run)
  setwd('..')
  return(summary_stats)
  
  # unlink(paste0(strsplit(dir_run_id, "/")[[1]], collapse = "/"), recursive = TRUE)
}

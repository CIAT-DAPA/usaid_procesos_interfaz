
# function(dir_run, dir_files = dir_exp_files, region, cultivar, climate_scenarios = climate$climate_scenarios, input_dates = climate$input_dates, location, select_day = day, output)


run_oryza <- function(dir_run, dir_files, region, cultivar, climate_scenarios, input_dates, location, select_day, output){
  
  require(stringr)
  lat <- location$lat
  long <- location$elev
  elev <- location$elev
  
  ## make id run 
  
  id_run <- make_id_run(paste0(dir_run, 'Temporal/'), region, cultivar, select_day)
  
  temp <- temp_wther(id_run)
  
  # make_mult_weather(climate_scenarios, id_run, filename, long, lat, elev)
  make_mult_weather(climate_scenarios, temp_dir, filename, long, lat, elev)
  

  
  make_control(id_run)   ## mirar la funcion para cambiar las especificaciones
  
  
  ## por ahora parece ser que se puede tener todo el vector
  
  PDATE <- input_dates$PDATE[select_day]
  SDATE <- input_dates$SDATE[select_day]
  IYEAR <- input_dates$IYEAR[select_day]
  ISTN <- 1:length(climate_scenarios)
  DATE <- input_dates$DATE[select_day]
  
  ## esta parte se puede integrar antes de anadir los archivos que necesita oryza y que no depende de una funcion
  id_s <- add_exp_cul(dir_files, region, id_run)  ## controla los parametros por region y retorna el id del suelo, crp and xfile
  
  # parameters_reruns <- settins_reruns(PDATE, SDATE, IYEAR, ISTN, id_run, id_s)
  parameters_reruns <- settins_reruns(SDATE, PDATE, IYEAR, ISTN, temp, id_s)
  
  make_reruns(parameters_reruns, id_run)
  files_oryza(dir_oryza, id_run)
  
  execute_oryza(id_run)
  
  ## copy climate oryza to current directory
  
  file.copy(file.path(list.files(temp, full.names = T)), id_run, recursive = TRUE)
  unlink(file.path(temp), recursive = T, force = T)
  ## extraer summary
  
  op_dat <- read_op(id_run) %>%
    mutate(yield_14 = WRR14,
           prec_acu = RAINCUM,
           t_max_acu = TMAXC,
           t_min_acu = TMINC,
           bio_acu = WAGT, 
           d_har = DAE)
  
  
  yield <- calc_desc(op_dat, 'yield_14') %>%
    tidy_descriptive(region, output$soil, output$cultivar, DATE, DATE)
  
  prec_acu <- calc_desc(op_dat, 'prec_acu') %>%
    tidy_descriptive(region, output$soil, output$cultivar, DATE, DATE)
  
  t_max_acu <- calc_desc(op_dat, 't_max_acu') %>%
    tidy_descriptive(region, output$soil, output$cultivar, DATE, DATE)
  
  t_min_acu <- calc_desc(op_dat, 't_min_acu') %>%
    tidy_descriptive(region, output$soil, output$cultivar, DATE, DATE)
  
  bio_acu <- calc_desc(op_dat, 'bio_acu') %>%
    tidy_descriptive(region, output$soil, output$cultivar, DATE, DATE)
  
  d_har <- calc_desc(op_dat, 'd_har') %>%
    tidy_descriptive(region, output$soil, output$cultivar, DATE, DATE)
  
  summary_stats <- dplyr::bind_rows(list(yield, 
                                         prec_acu,
                                         t_max_acu,
                                         t_min_acu,
                                         bio_acu,
                                         d_har))
  
  return(summary_stats)
  # unlink(paste0(id_run, ''))
  setwd(dir_run)
  
}





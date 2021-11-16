# function(dir_run, dir_files = dir_exp_files, region, cultivar, climate_scenarios = climate$climate_scenarios, input_dates = climate$input_dates, location, select_day = day, output)


run_oryza <- function(dir_run, dir_files, region, cultivar, climate_scenarios, input_dates, location, select_day, output, filename, dir_oryza){

#  require(stringr)
  lat <- location$lat
  long <- location$long
  elev <- location$elev

  ## make id run #  crea directorio a carpeta con archivos de simulacion para oryza : Control, EXP, RER, etc

  id_run <- make_id_run(paste0(dir_run, 'Temporal/'), region, cultivar, select_day)
  print(paste0("id_run: ", id_run))
  temp <- temp_wther(id_run)

  # make_mult_weather(climate_scenarios, id_run, filename, long, lat, elev) # crea archivos clima para oryza en carpeta ./tmp
  make_mult_weather(climate_scenarios, temp, filename, long, lat, elev)
 
  
  make_control(id_run)   ## mirar la funcion para cambiar las especificaciones


  ## por ahora parece ser que se puede tener todo el vector

  PDATE <- input_dates$PDATE[select_day]
  SDATE <- input_dates$SDATE[select_day]
  IYEAR <- input_dates$IYEAR[select_day]
  EMYR <- year(input_dates$DATE[select_day])
  ISTN <- 1:length(climate_scenarios)
  DATE <- input_dates$DATE[select_day]


  ## esta parte se puede integrar antes de anadir los archivos que necesita oryza y que no depende de una funcion

  #Esta funcion copia los archivos de los inputs (setups) y copia en cada configuracion de outputs --suelo, crp and xfile
  id_s <- add_exp_cul(dir_files, region, id_run)  ## controla los parametros por region y retorna el id del suelo, crp and xfile

  # parameters_reruns <- settins_reruns(PDATE, SDATE, IYEAR, ISTN, id_run, id_s)
  parameters_reruns <- settins_reruns(PDATE, SDATE, IYEAR, EMYR, ISTN, " tmp/", id_s) # Retorna lista de parametros

  make_reruns(parameters_reruns, id_run) # escribe archivo control
  # files_oryza(dir_oryza, id_run)

  execute_oryza(id_run, dir_oryza)  #main_function L186 

  ## copy climate oryza to current directory

#  file.copy(file.path(list.files(temp, full.names = T)), id_run, recursive = TRUE)
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

  #Borrar .bins
  return(summary_stats)
  # unlink(paste0(id_run, ''))
  setwd(dir_run)

}
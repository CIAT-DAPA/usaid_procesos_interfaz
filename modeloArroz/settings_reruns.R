
# EMD <- PDATE    ## Planting date
# STTIME <- SDATE  ## Simulation date
# IYEAR ## year of simulation
# dir_run <- id_run


# settins_reruns(region, EMD, STTIME, IYEAR, ISTN, dir_run)

settins_reruns <- function(EMD, STTIME, IYEAR, ISTN, dir_run, id_s){
  
  require(rebus)
  # ISTN <- 1:length(EMD) ## para controlar el escenario climatico a simular
  # IYEAR <- reruns_params$IYEAR
  # STTIME <- reruns_params$STTIME 
  # EMD <- reruns_params$EMD
  
  # crossing()
  
  id_soil <- id_s$id_soil
  id_exp <- id_s$id_exp
  id_crp <- id_s$id_crp
  
  CNTR <- 'USAID'
  
  WTRDIR = paste0("'", gsub('/', BACKSLASH, dir_run), "'")
  
  
  data <- data.frame(FILEIT = paste0(id_exp, '.exp'), 
                     FILEI2 = paste0(id_soil, '.sol'),
                     FILEI1 = paste0(id_crp, '.crp'),
                     CNTR,
                     ISTN,
                     IYEAR, 
                     STTIME,
                     EMD,
                     EMYR = IYEAR, 
                     WTRDIR = WTRDIR)
  
  
  
  return(data)
}



# EMD <- PDATE    ## Planting date
# STTIME <- SDATE  ## Simulation date
# IYEAR ## year of simulation
# dir_run <- id_run


# settins_reruns(region, EMD, STTIME, IYEAR, ISTN, dir_run)

settins_reruns <- function(region, EMD, STTIME, IYEAR, ISTN, dir_run){
  
  require(rebus)
  # ISTN <- 1:length(EMD) ## para controlar el escenario climatico a simular
  # IYEAR <- reruns_params$IYEAR
  # STTIME <- reruns_params$STTIME 
  # EMD <- reruns_params$EMD
  
  # crossing()
  
   
  CNTR <- 'USAID'
  
  WTRDIR = paste0("'", gsub('/', BACKSLASH, dir_run), "'")
  
  if(region == "Saldaña"){
    
    data <- data.frame(FILEIT = 'FEMO.exp', 
                       FILEI2 = 'FEMO.sol',
                       FILEI1 = 'F2000.crp',
                       CNTR,
                       ISTN,
                       IYEAR, 
                       STTIME,
                       EMD,
                       EMYR = IYEAR, 
                       WTRDIR = WTRDIR)
    
  }
  
  return(data)
}


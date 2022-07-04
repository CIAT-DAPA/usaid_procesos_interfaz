### Run Model - DSSAT Aclimate - EDACaP
# Author: Rodriguez-Espinoza J.
# https://github.com/jrodriguez88/
# 2022


#Executes the model depending on OS
# dir_run <- Carpeta con todos los inputs necesarios para simulacion con DSSAT 
# Para windows, el ejecutable V4.8 "DSCSM048.EXE" debe estar instalado o dispnible en "C:/DSSAT48/DSCSM048.EXE 
# Para windows tener imagen de dssat 


execute_dssat <- function(dir_run, crop, id_name = "CIAT0001"){
  
  # set dir of simulation
  wd <- getwd()
  setwd(dir_run)
  
  
  
    # run oryza - OS
    print(paste('Simulation: ', dir_run, " - ", crop))

    if (Sys.info()['sysname'] == 'Windows'){ 
      
      model <- paste0(crop_name_setup(id_name, crop)[["model"]], 048) 
      system(paste0("C:/DSSAT48/DSCSM048.EXE " , model," B ", "DSSBatch.v48"), ignore.stdout = F, show.output.on.console = T)
    }
    else{
      system(paste0('dssat ',  dir_run,' B DSSBatch.v48'))
    
  }
  
 
 setwd(wd)
  
  
}

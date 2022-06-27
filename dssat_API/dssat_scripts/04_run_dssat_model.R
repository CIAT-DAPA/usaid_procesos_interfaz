### Run Model - DSSAT Aclimate - EDACaP
# Author: Rodriguez-Espinoza J.
# https://github.com/jrodriguez88/
# 2022


#Executes the model depending on OS
# dir_run <- Carpeta con todos los inputs necesarios para simulacion con DSSAT 



execute_dssat <- function(dir_run){
  
  # set dir of simulation
  wd <- getwd()
  setwd(dir_run)
  
  
  
  # run oryza - OS
    print(paste('ruta: ', dir_run))

    if (Sys.info()['sysname'] == 'Windows'){ 
      
      model <- paste0(list.files(dir_run, pattern = "*.CUL") %>% str_sub(1,2), "CER047") 
      system(paste0("DSCSM047.EXE " , model," B ", "DSSBatch.v47"), ignore.stdout = T, show.output.on.console = F)
    }
    else{
      system(paste0('dssat ',  dir_run,' B DSSBatch.v47'))
    
  }
  
 
 setwd(wd)
  
  
}

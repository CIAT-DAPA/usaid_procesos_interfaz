### Run Model - ORYZA linux --->> 
# Author: Rodriguez-Espinoza J.
# https://github.com/jrodriguez88/
# 2022


#Executes the model depending on OS
# dir_run <- Carpeta con todos los inputs necesarios para simulacion con Oryza  
#dir_oryza <-  "C:/Program Files (x86)/ORYZA(v3)/"  -- Directorio donde se encuentra el ejecutable ORYZA3.EXE. 



execute_oryza <- function(dir_run){
  
  # set dir of simulation
  wd <- getwd()
  setwd(dir_run)
  
  # run oryza - OS
  if (Sys.info()['sysname'] == 'Windows'){ 
#    exe_oryza <- paste0(dir_oryza, 'ORYZA3.EXE')
#    file.copy(exe_oryza, dir_run)
#    system(paste0(gsub(" ","\\\\ ",dir_run), "ORYZA3.EXE"), ignore.stdout = F, show.output.on.console = T)
    
 #   system(paste0("cd .", dir_run, " ORYZA3.EXE"), ignore.stdout = F, show.output.on.console = T)
    system("ORYZA3.EXE", ignore.stdout = F, show.output.on.console = T)
  }
  else{
    # print(paste(dir_run, "oryza ."))
    system(paste0("cd ", dir_run, "; oryza ."))
    
  }
  
# Sys.sleep(5) 
# # delete res and *.bin files 
# bin_files <- list.files(dir_run, pattern = fixed("^res"), full.names = T)
# lapply(bin_files, file.remove)
# # delete climate files  
# unlink(file.path(list.files(dir_run, pattern = fixed("wth"), full.names = T)), recursive = T, force = T)
#  
 setwd(wd)
  
  
}

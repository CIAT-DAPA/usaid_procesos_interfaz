### Set Model - ORYZA linux --->> Control + Reruns
# Author: Rodriguez-Espinoza J.
# https://github.com/jrodriguez88/
# 2022



# Arguments
# dir_inputs = dir of input oryza files c(".sol", ".crp", ".exp") and coordinates of weather station
# dir_run = dir of specific simulation setups
# EMD  = Emergence day - julian date 
# STTIME = Start time simulation - julian date
# IYEAR = Initial Year - numerical XXXX
# EMYR = Emergence Year - numerical XXXX
# ISTN =  Station code - numerical - number of scenario (1:99)






make_dir_run <- function(dir_run_main, days){
  
  
#  require(stringr)
  dir <- paste0(dir_run_main, days, '/')
  dir <- stringr::str_replace(dir, "ñ", "n")
  
  if (!dir.exists(dir)) { 
    
    dir.create(dir, showWarnings = F, recursive = TRUE, mode = "777")
    # system('chmod 777 *.*')
    # paste0(dir_base, region, '/', cultivar,  '/', select_day)
    
  }
  
  return(paste0(dir))
}




# Funcion copia inputs base en directorio de simulacion de cada setups

copy_inputs <- function(dir_inputs, dir_run){
  
  dir_files <- list.files(dir_inputs, full.names = T)
  file.copy(dir_files, dir_run)
  
  walk2(.x = c(".sol", ".crp", ".exp"), 
       .y = paste0("standard", c(".sol", ".crp", ".exp")), 
       ~file.rename(
         from = list.files(dir_run, pattern = .x, full.names = T), 
         to = paste0(dir_run, .y)))
        
  
}


# Lee informacion de geolocalizacion
load_coordinates <- function(dir_inputs_run){
  
  frame_list <- function(data){
    
    setNames(split(data[,2], seq(nrow(data))), data[,1])
    
  }
  
  
  print(paste0("dir_inputs_run: ", dir_inputs_run))
#  require(readr)
  coordenadas <- read_csv(paste0(dir_inputs_run,'coordenadas.csv')) %>%
    as.data.frame() %>%
    frame_list()
  
}

#load_wth_data <- function( )


#Crea archivo control en el directorio especifico de la simulacion

write_control <- function(dir_run){
  
  
  file_name <- paste0(dir_run, 'control.dat')
  
  # print(file_name)
  if(file.exists(file_name)){
    
    file.remove(file_name)
    
  } 
  
  sink(file = file_name, append = T)

cat(paste0("CONTROLFILE = 'control.dat'"), sep = '\n')
cat(paste0("STRUN = 1"), sep = '\n')
cat(paste0("ENDRUN = 100"), sep = '\n')
cat(sep = '\n')
cat(paste0("FILEON = 'res.dat'"), sep = '\n')
cat(paste0("FILEOL = 'model.log'"), sep = '\n')
cat(paste0("FILEIR = 'reruns.rer'"), sep = '\n')
cat(paste0("FILEIT = 'standard.exp'"), sep = '\n')
cat(paste0("FILEI1 = 'standard.crp'"), sep = '\n')
cat(paste0("FILEI2 = 'standard.sol'"), sep = '\n')
cat(sep = '\n')
cat(paste0("MULTIY = 'YES'"), sep = '\n')
cat(sep = '\n')
cat(paste0("PRDEL = 1."), sep = '\n')
cat(paste0("IPFORM = 5 "), sep = '\n')
cat(paste0("COPINF = 'N'"), sep = '\n')
cat(paste0("DELTMP = 'N'"), sep = '\n')
cat(paste0("IFLAG  = 1100 "), sep = '\n')
  
  
sink()  
  
  
}


#Crea archivo reruns en el directorio especifico de la simulacion
write_reruns_aclimate <- function(dir_run, EMD, STTIME, IYEAR, EMYR){
  
#  require(rebus)
  
  CNTR <- 'ciat'
  ISTN <- 1:100
  
  if (Sys.info()['sysname'] == 'Windows'){ 
    WTRDIR = paste0("'", gsub('/', BACKSLASH, " wth/"), "'")
  }
  else{
    WTRDIR = paste0("'", gsub('/', '/', " wth/"), "'")
    
  }
  

  
  # ISTN <- 1:length(EMD) ## para controlar el escenario climatico a simular
  # IYEAR <- reruns_params$IYEAR
  # STTIME <- reruns_params$STTIME 
  # EMD <- reruns_params$EMD
  

  # 
  data <- data.frame(FILEIT = paste0('standard', '.exp'), 
                     FILEI2 = paste0('standard', '.sol'),
                     FILEI1 = paste0('standard', '.crp'),
                     CNTR,
                     ISTN,
                     IYEAR, 
                     STTIME,
                     EMYR, 
                     EMD,
                     WTRDIR = WTRDIR)
  
  

  file_name <- paste0(dir_run, 'reruns.rer')
  
  # print(file_name)
  if(file.exists(file_name)){
    
    file.remove(file_name)
    
  } 
  
  sink(file = file_name, append = T)
  for(i in 1:nrow(data)){
    
    
    cat('********************', sep = '\n')
    cat(paste("FILEIT = ", "'",  data[i, 'FILEIT'], "'", sep = ""), sep = '\n')
    cat(paste("FILEI2 = ", "'", data[i, 'FILEI2'], "'", sep = ""), sep = '\n')
    cat(paste("FILEI1 = ", "'", data[i, 'FILEI1'], "'", sep = ""), sep = '\n')
    cat(paste("CNTR = ",   "'", data[i, 'CNTR'], "'", sep = ""), sep = '\n')
    cat(paste("ISTN = ", data[i, 'ISTN'], sep = ""), sep = '\n')
    cat(paste("IYEAR =  ",  data[i, 'IYEAR'], sep = ""), sep = '\n')
    cat(paste("STTIME =  ",  data[i, 'STTIME'], ".", sep = ""), sep = '\n')
    cat(paste("EMYR = ", data[i, 'EMYR'], sep = ""), sep = '\n')
    cat(paste("EMD =  ", data[i, 'EMD'], sep = ""), sep = '\n')
    cat(paste("WTRDIR = ", as.character(data[i, 'WTRDIR']), sep = ""), sep = '\n')
    
    
  }
  
  sink()
  
}








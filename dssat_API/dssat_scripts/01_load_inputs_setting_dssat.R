### Set Model - DSSAT RUN Aclimate--->> 
# Author: Rodriguez-Espinoza J.
# https://github.com/jrodriguez88/
# 2022




make_dir_run <- function(dir_run_main, sim_number){
  
  
#  require(stringr)
  dir <- paste0(dir_run_main, sim_number, '/')
  dir <- stringr::str_replace(dir, "ñ", "n")
  
  if (!dir.exists(dir)) { 
    
    dir.create(dir, showWarnings = F, recursive = TRUE, mode = "777")
    # system('chmod 777 *.*')
    # paste0(dir_base, region, '/', cultivar,  '/', select_day)
    
  }
  
  return(paste0(dir))
}


# Funcion copia inputs base en directorio de simulacion de cada setups
copy_inputs <- function(dir_inputs, dir_run, crop){
  
  
  patt <- 
  
  dir_files <- list.files(dir_inputs, full.names = T)
  file.copy(dir_files, dir_run)
  
#  map2(.x = c("*.SPE", "*.ECO", "*.CUL"), 
#       .y = paste0("standard", c("*.SPE", "*.ECO", "*.CUL")), 
#       ~file.rename(
#         from = list.files(dir_run, pattern = .x, full.names = T), 
#         to = paste0(dir_run, .y)))
        
  
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

# Function to write Crop names/extension into DSSAT format
crop_name_setup <- function(id_name, crop){
  
  base_tb <- tibble(
    crop_name = c("rice", "maize", "bean", "barley", "sorghum", "wheat", "teff"),
    CR = c("RI", "MZ", "BN", "BA", "SG", "WH", "TF"))
  
  cul <- base_tb %>% 
    dplyr::filter(crop_name %in% crop) %>%
    mutate(crop_name =  toupper(crop_name),
           ext = paste0(id_name, ".", CR, "X"))
  
  return(cul)
  
  
}

#crop_name_setup("CIAT0001", "barley")[[3]]


#Crea archivo X file  en el directorio especifico de la simulacion


#Crea archivo reruns en el directorio especifico de la simulacion
write_batch_aclimate <- function(crop, xfile, treatments_number, filename){
  
  batchfile <- rbind(
    rbind(
      # Batchfile headers            
      paste0("$BATCH(", toupper(crop), ")"),            
      "!",            
      cbind(sprintf("%6s %89s %6s %6s %6s %6s", "@FILEX", "TRTNO", "RP", "SQ", "OP", 
                    "CO"))),            
    cbind(sprintf("%6s %83s %6i %6i %6i %6i",            
                  paste0(xfile),
                  1:treatments_number,  # Variable for treatment number            
                  1,  # Default value for RP element            
                  0,  # Default value for SQ element            
                  1,  # Default value for OP element            
                  0)))  # Default value for CO element 
  
  # Write the batch file to the selected folder  
  write(batchfile, file = filename, append = F)
  

  
}



# conevrt date to DSSAT format 
date_for_dssat <- function(date) {
  stopifnot(class(date)=="Date")
  stopifnot(require(lubridate))
  
  yr <- str_sub(year(date), -2)
  doy <- yday(date)
  
  paste0(yr, sprintf("%.3d", doy))
  
}






